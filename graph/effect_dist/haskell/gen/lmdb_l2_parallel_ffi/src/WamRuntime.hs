{-# LANGUAGE BangPatterns #-}
module WamRuntime where

import qualified Data.HashMap.Strict as Map
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.Array (Array, listArray, (!), bounds)
import qualified Data.Set as Set
import Data.List (isPrefixOf, foldl', nub)
import Data.Maybe (fromMaybe)
-- Phase 4.2: intra-query parallelism. parMap/rdeepseq spark the
-- alternative clauses of a forkable ParTryMeElse choice point; the
-- WamState NFData instance lives in WamTypes.
import Control.Parallel.Strategies (parMap, rdeepseq)
import Control.DeepSeq (NFData(..), deepseq)
-- Phase 4.4: race-to-cancel for parallel negation. async/waitAny
-- let us cancel remaining branches once one succeeds.
import Control.Concurrent.Async (async, cancel, waitAny)
import Control.Exception (evaluate)
import System.IO.Unsafe (unsafePerformIO)
import Database.LMDB.Raw
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek, poke, peekElemOff)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (withArray)
import Foreign.C.String (withCStringLen, peekCStringLen)
import Foreign.C.Types (CSize(..), CChar)
import Data.Int (Int32)
import Data.Word (Word8)
import Data.Bits ((.&.))
import Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicModifyIORef')
import qualified Data.Array as A
import qualified Data.Array.IO as IOA
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Control.Exception as E
import Control.Monad (forM_, when, foldM)
import Control.Concurrent (runInBoundThread, myThreadId, ThreadId, threadCapability, getNumCapabilities)
import Control.Concurrent.Async (mapConcurrently)
import WamTypes

-- | Execute a single WAM instruction.
-- The WamContext argument is read-only and threaded through (does NOT
-- become part of any per-step record allocation).
step :: WamContext -> WamState -> Instruction -> Maybe WamState
step !ctx s (GetConstant c ai) =
  let val = derefVar (wsBindings s) <$> IM.lookup ai (wsRegs s)
  in case val of
    Just v | v == c -> Just (s { wsPC = wsPC s + 1 })
    Just (Unbound vid) ->
      Just (s { wsPC = wsPC s + 1
              , wsRegs = IM.insert ai c (wsRegs s)
              , wsBindings = IM.insert vid c (wsBindings s)
              , wsTrail = TrailEntry vid (IM.lookup vid (wsBindings s)) : wsTrail s
              , wsTrailLen = wsTrailLen s + 1
              })
    _ -> Nothing

step !ctx s (GetVariable xn ai) =
  case IM.lookup ai (wsRegs s) of
    Just val -> let dv = derefVar (wsBindings s) val
                in Just ((putReg xn dv s) { wsPC = wsPC s + 1 })
    Nothing -> Nothing

step !ctx s (GetValue xn ai) =
  let va = derefVar (wsBindings s) <$> IM.lookup ai (wsRegs s)
      vx = getReg xn s
  in case (va, vx) of
    (Just a, Just x) | a == x -> Just (s { wsPC = wsPC s + 1 })
    (Just (Unbound vid), Just x) ->
      Just (s { wsPC = wsPC s + 1
              , wsRegs = IM.insert ai x (wsRegs s)
              , wsBindings = IM.insert vid x (wsBindings s)
              , wsTrail = TrailEntry vid (IM.lookup vid (wsBindings s)) : wsTrail s
              , wsTrailLen = wsTrailLen s + 1
              })
    -- Symmetric case: xn (X-register) holds the unbound side, ai
    -- holds the bound value. Used by the =../2 / functor/3 compose
    -- lowering, which emits get_value T_reg, TermReg where T_reg is
    -- the fresh output and TermReg is the freshly-constructed Str.
    -- Unification is symmetric, so bind the xn vid to a.
    (Just a, Just (Unbound vid)) ->
      Just (s { wsPC = wsPC s + 1
              , wsBindings = IM.insert vid a (wsBindings s)
              , wsTrail = TrailEntry vid (IM.lookup vid (wsBindings s)) : wsTrail s
              , wsTrailLen = wsTrailLen s + 1
              })
    _ -> Nothing

step !ctx s (PutConstant c ai) =
  Just (s { wsPC = wsPC s + 1, wsRegs = IM.insert ai c (wsRegs s) })

step !ctx s (PutVariable xn ai) =
  let vid = wsVarCounter s
      var = Unbound vid
      s1 = putReg xn var s
  in Just (s1 { wsPC = wsPC s + 1
              , wsRegs = IM.insert ai var (wsRegs s1)
              , wsVarCounter = vid + 1
              })

step !ctx s (PutValue xn ai) =
  case getReg xn s of
    Just val -> Just (s { wsPC = wsPC s + 1, wsRegs = IM.insert ai val (wsRegs s) })
    Nothing -> Nothing

step !ctx s (PutStructure fnId ai arity) =
  Just (s { wsPC = wsPC s + 1
          , wsBuilder = BuildStruct fnId ai arity []
          })

-- PutStructureDyn: like PutStructure but functor name and arity come
-- from registers at runtime. Used when the term shape is computed
-- dynamically (e.g., after =../2 or functor/3 with variable args).
step !ctx s (PutStructureDyn nameReg arityReg targetReg) =
  let mName = derefVar (wsBindings s) <$> getReg nameReg s
      mArity = derefVar (wsBindings s) <$> getReg arityReg s
  in case (mName, mArity) of
    (Just (Atom fnId), Just (Integer arity)) | arity >= 0 ->
      Just (s { wsPC = wsPC s + 1
              , wsBuilder = BuildStruct fnId targetReg (fromIntegral arity) []
              })
    _ -> Nothing  -- name must be an atom, arity a non-negative integer

step !ctx s (PutList ai) =
  Just (s { wsPC = wsPC s + 1
           , wsBuilder = BuildList ai []
           })

step !ctx s (SetValue xn) =
  case getReg xn s of
    Just val -> addToBuilder val s
    Nothing -> Nothing

step !ctx s (SetVariable xn) =
  let vid = wsVarCounter s
      var = Unbound vid
      s1 = putReg xn var (s { wsVarCounter = vid + 1 })
  in addToBuilder var s1

step !ctx s (SetConstant c) =
  addToBuilder c s

-- Fast path: call has been pre-resolved to a target PC at load time.
-- No string lookup, no foreign/indexed dispatch — just a jump.
step !ctx s (CallResolved pc _arity) =
  Just (s { wsPC = pc, wsCP = wsPC s + 1 })

-- Foreign call: compile-time resolved. executeForeign is the sole dispatch
-- path — Nothing means no solutions (backtrack), never fallthrough.
step !ctx s (CallForeign pred _arity) =
  executeForeign ctx pred (s { wsCP = wsPC s + 1 })

-- Phase F2: FactStream call. Dispatches to streamFacts which iterates
-- inline fact tuples from wcInlineFacts. Nothing = no matching facts.
step !ctx s (CallFactStream pred _arity) =
  streamFacts ctx pred (s { wsCP = wsPC s + 1 })

-- Call dispatch for non-foreign, non-resolved predicates. Foreign predicates
-- are handled by CallForeign (resolved at compile time), so executeForeign
-- is NOT checked here — no ambiguity between "unhandled" and "no solutions".
step !ctx s (Call pred _arity) =
  let sc = s { wsCP = wsPC s + 1 }
  in case Map.lookup pred (wcLoweredPredicates ctx) of
    Just fn -> fn ctx sc
    Nothing -> case callIndexedFact2 ctx pred sc of
      Just sr -> Just sr
      Nothing -> case Map.lookup pred (wcLabels ctx) of
        Just pc -> Just (s { wsPC = pc, wsCP = wsPC s + 1 })
        Nothing -> Nothing

-- Jump: unconditional jump to a label (used in if-then-else compilation)
step !ctx s (Jump label) =
  case Map.lookup label (wcLabels ctx) of
    Just pc -> Just (s { wsPC = pc })
    Nothing -> Nothing

-- JumpPc: pre-resolved jump (no label lookup)
step !ctx s (JumpPc pc) = Just (s { wsPC = pc })

-- ExecutePc: pre-resolved tail call (direct PC jump, no wsCP change)
step !ctx s (ExecutePc pc) = Just (s { wsPC = pc })

-- Execute: tail call, like Call but without setting wsCP
step !ctx s (Execute pred) =
  case Map.lookup pred (wcLoweredPredicates ctx) of
    Just fn -> fn ctx s
    Nothing -> case callIndexedFact2 ctx pred s of
      Just sr -> Just sr
      Nothing -> case Map.lookup pred (wcLabels ctx) of
        Just pc -> Just (s { wsPC = pc })
        Nothing -> Nothing

step !ctx s Proceed =
  let ret = wsCP s
  in if ret == 0 then Just (s { wsPC = 0 })
     else Just (s { wsPC = ret, wsCP = 0 })

step !ctx s Allocate =
  let frame = EnvFrame (wsCP s) IM.empty
  in Just (s { wsPC = wsPC s + 1
             , wsStack = frame : wsStack s
             , wsCutBar = wsCPsLen s
             })

step !ctx s Deallocate =
  case wsStack s of
    (EnvFrame oldCP _ : rest) -> Just (s { wsPC = wsPC s + 1, wsStack = rest, wsCP = oldCP })
    _ -> Nothing

step !ctx s (TryMeElse label) =
  let nextPC = fromMaybe 0 $ Map.lookup label (wcLabels ctx)
      cp = ChoicePoint
        { cpNextPC   = nextPC
        , cpRegs     = wsRegs s
        , cpStack    = wsStack s
        , cpCP       = wsCP s
        , cpTrailLen = wsTrailLen s
        , cpHeapLen  = wsHeapLen s
        , cpBindings = wsBindings s
        , cpCutBar   = wsCutBar s
        , cpAggFrame = Nothing, cpBuiltin = Nothing
        }
  in Just (s { wsPC = wsPC s + 1, wsCPs = cp : wsCPs s, wsCPsLen = wsCPsLen s + 1 })

step !ctx s TrustMe =
  case wsCPs s of
    (_ : rest) -> Just (s { wsPC = wsPC s + 1, wsCPs = rest, wsCPsLen = wsCPsLen s - 1 })
    [] -> Nothing

step !ctx s (RetryMeElse label) =
  case wsCPs s of
    (cp : rest) ->
      let nextPC = fromMaybe 0 $ Map.lookup label (wcLabels ctx)
      in Just (s { wsPC = wsPC s + 1, wsCPs = cp { cpNextPC = nextPC } : rest })
    [] -> Nothing

-- Pre-resolved variants: direct PC, no label lookup
step !ctx s (TryMeElsePc nextPC) =
  let cp = ChoicePoint
        { cpNextPC   = nextPC
        , cpRegs     = wsRegs s
        , cpStack    = wsStack s
        , cpCP       = wsCP s
        , cpTrailLen = wsTrailLen s
        , cpHeapLen  = wsHeapLen s
        , cpBindings = wsBindings s
        , cpCutBar   = wsCutBar s
        , cpAggFrame = Nothing, cpBuiltin = Nothing
        }
  in Just (s { wsPC = wsPC s + 1, wsCPs = cp : wsCPs s, wsCPsLen = wsCPsLen s + 1 })

step !ctx s (RetryMeElsePc nextPC) =
  case wsCPs s of
    (cp : rest) -> Just (s { wsPC = wsPC s + 1, wsCPs = cp { cpNextPC = nextPC } : rest })
    [] -> Nothing

-- Phase 4.1 parallel-forkable variants. For now they alias their
-- sequential counterparts — the instructions mark the predicate as
-- fork-safe but the runtime doesn't fork yet. Phase 4.2 will split
-- these handlers off to do actual parMap-based forking at the
-- surrounding aggregate boundary.
--
-- Phase 4.2: when a ParTryMeElse fires inside a fork-compatible
-- aggregate (sum / count), we collect all N alternative branch
-- entry PCs, run each branch in parallel via `parMap rdeepseq`, then
-- merge the accumulated values via the aggregate strategy. Each
-- branch's EndAggregate is intercepted so it appends to that
-- branch's local wsAggAccum without finalizing the outer aggregate.
-- Falls back to the sequential TryMeElse handler when the enclosing
-- aggregate is not fork-compatible (or when there is no aggregate at
-- all).
--
-- ParRetryMeElse / ParTrustMe still delegate to their sequential
-- counterparts — once ParTryMeElse has chosen to fork, the runtime
-- never walks through those; they're only reached if the fork
-- path bailed out to sequential.
step !ctx s (ParTryMeElse label)    = forkOrSequential ctx s (Left label)
step !ctx s (ParRetryMeElse label)  = step ctx s (RetryMeElse label)
step !ctx s ParTrustMe              = step ctx s TrustMe
step !ctx s (ParTryMeElsePc pc)     = forkOrSequential ctx s (Right pc)
step !ctx s (ParRetryMeElsePc pc)   = step ctx s (RetryMeElsePc pc)

step !ctx s (SwitchOnConstantPc table) =
  let val = derefVar (wsBindings s) <$> IM.lookup 1 (wsRegs s)
  in case val of
    Just (Unbound _) -> Just (s { wsPC = wsPC s + 1 })
    Just (Atom aid) -> case IM.lookup aid table of
      Just pc -> Just (s { wsPC = pc })
      Nothing -> Nothing
    Just (Integer n) -> case IM.lookup n table of
      Just pc -> Just (s { wsPC = pc })
      Nothing -> Nothing
    _ -> Nothing

step !ctx s (BuiltinCall "!/0" _) =
  -- Cut: truncate wsCPs to the barrier depth saved at clause Allocate.
  Just (s { wsPC = wsPC s + 1, wsCPs = take (wsCutBar s) (wsCPs s), wsCPsLen = wsCutBar s })

-- CutIte: soft cut for if-then-else — pops exactly the top choice point
-- (the one pushed by try_me_else for the Else branch). Unlike !/0 which
-- truncates to wsCutBar (clause-level), this only removes the immediately
-- enclosing if-then-else CP, preserving aggregate frames and outer CPs.
step !ctx s CutIte =
  case wsCPs s of
    (_cp : rest) -> Just (s { wsPC = wsPC s + 1, wsCPs = rest, wsCPsLen = wsCPsLen s - 1 })
    [] -> Just (s { wsPC = wsPC s + 1 })  -- no CP to pop (shouldn't happen)

-- Type-checking builtins
step !ctx s (BuiltinCall "nonvar/1" _) =
  case derefVar (wsBindings s) <$> IM.lookup 1 (wsRegs s) of
    Just (Unbound _) -> Nothing
    Just _           -> Just (s { wsPC = wsPC s + 1 })
    Nothing          -> Nothing

step !ctx s (BuiltinCall "var/1" _) =
  case derefVar (wsBindings s) <$> IM.lookup 1 (wsRegs s) of
    Just (Unbound _) -> Just (s { wsPC = wsPC s + 1 })
    _                -> Nothing

step !ctx s (BuiltinCall "atom/1" _) =
  case derefVar (wsBindings s) <$> IM.lookup 1 (wsRegs s) of
    Just (Atom _) -> Just (s { wsPC = wsPC s + 1 })
    _             -> Nothing

step !ctx s (BuiltinCall "integer/1" _) =
  case derefVar (wsBindings s) <$> IM.lookup 1 (wsRegs s) of
    Just (Integer _) -> Just (s { wsPC = wsPC s + 1 })
    _                -> Nothing

step !ctx s (BuiltinCall "number/1" _) =
  case derefVar (wsBindings s) <$> IM.lookup 1 (wsRegs s) of
    Just (Integer _) -> Just (s { wsPC = wsPC s + 1 })
    Just (Float _)   -> Just (s { wsPC = wsPC s + 1 })
    _                -> Nothing

step !ctx s (BuiltinCall "is/2" _) =
  let expr = derefVar (wsBindings s) $ fromMaybe (Integer 0) (IM.lookup 2 (wsRegs s))
      result = evalArith (wcInternTable ctx) (wsBindings s) expr
      lhs = derefVar (wsBindings s) <$> IM.lookup 1 (wsRegs s)
  in case (lhs, result) of
    (Just (Unbound vid), Just r) ->
      let val = if fromIntegral (round r :: Int) == r then Integer (round r) else Float r
      in Just (s { wsPC = wsPC s + 1
                 , wsRegs = IM.insert 1 val (wsRegs s)
                 , wsBindings = IM.insert vid val (wsBindings s)
                 , wsTrail = TrailEntry vid (IM.lookup vid (wsBindings s)) : wsTrail s
                 , wsTrailLen = wsTrailLen s + 1
                 })
    (Just (Integer n), Just r) | fromIntegral n == r -> Just (s { wsPC = wsPC s + 1 })
    _ -> Nothing

step !ctx s (BuiltinCall "length/2" _) =
  let listVal = derefVar (wsBindings s) $ fromMaybe (VList []) (IM.lookup 1 (wsRegs s))
  in case listVal of
    VList items ->
      let len = length items
          lhs = derefVar (wsBindings s) <$> IM.lookup 2 (wsRegs s)
      in case lhs of
        Just (Unbound vid) ->
          let val = Integer len
          in Just (s { wsPC = wsPC s + 1
                     , wsRegs = IM.insert 2 val (wsRegs s)
                     , wsBindings = IM.insert vid val (wsBindings s)
                     , wsTrail = TrailEntry vid (IM.lookup vid (wsBindings s)) : wsTrail s
                     , wsTrailLen = wsTrailLen s + 1
                     })
        Just (Integer n) | n == len -> Just (s { wsPC = wsPC s + 1 })
        _ -> Nothing
    _ -> Nothing

step !ctx s (BuiltinCall "</2" _) =
  let v1 = evalArith (wcInternTable ctx) (wsBindings s) =<< (derefVar (wsBindings s) <$> IM.lookup 1 (wsRegs s))
      v2 = evalArith (wcInternTable ctx) (wsBindings s) =<< (derefVar (wsBindings s) <$> IM.lookup 2 (wsRegs s))
  in case (v1, v2) of
    (Just a, Just b) | a < b -> Just (s { wsPC = wsPC s + 1 })
    _ -> Nothing

step !ctx s (BuiltinCall "\\+/1" _) =
  let goal = IM.lookup 1 (wsRegs s) >>= derefHeap (wsHeap s)
      tbl = wcInternTable ctx
  in case goal of
    -- Fast path: \+ member(X, L) — check functor name via reverse lookup
    Just (Str fnId [needle, haystack]) | "member" `isPrefixOf` lookupAtom tbl fnId ->
      let n = derefVar (wsBindings s) needle
          h = derefVar (wsBindings s) haystack
          found = case h of
            VList items -> any (\item -> derefVar (wsBindings s) item == n) items
            _ -> False
      in if found then Nothing else Just (s { wsPC = wsPC s + 1 })
    -- Fast path: \+ true always fails, \+ fail always succeeds
    Just (Atom aid) | aid == atomTrue -> Nothing
    Just (Atom aid) | aid == atomFail -> Just (s { wsPC = wsPC s + 1 })
    -- General path: resolve the goal, snapshot-and-run.
    -- Phase 4.4: if the goal's entry instruction is ParTryMeElse,
    -- fork branches in parallel via runNegationParallel.
    Just (Str fnId args) ->
      let goalKey = lookupAtom tbl fnId ++ "/" ++ show (length args)
          dArgs = map (derefVar (wsBindings s)) args
      in case Map.lookup goalKey (wcLabels ctx) of
           Just pc ->
             let snap = s { wsRegs = IM.fromList (zip [1..] dArgs) }
                 (lo, hi) = bounds (wcCode ctx)
             in if pc >= lo && pc <= hi
                then case wcCode ctx ! pc of
                  ParTryMeElse elseLabel ->
                    let elsePC = fromMaybe (-1) (Map.lookup elseLabel (wcLabels ctx))
                    in if runNegationParallel ctx snap pc elsePC
                       then Nothing
                       else Just (s { wsPC = wsPC s + 1 })
                  ParTryMeElsePc elsePC ->
                    if runNegationParallel ctx snap pc elsePC
                    then Nothing
                    else Just (s { wsPC = wsPC s + 1 })
                  _ -> -- Sequential: just run normally
                    let snapshot = snap { wsPC = pc, wsCP = 0, wsCutBar = 0 }
                    in case run ctx snapshot of
                         Just _  -> Nothing
                         Nothing -> Just (s { wsPC = wsPC s + 1 })
                else Just (s { wsPC = wsPC s + 1 })
           Nothing -> Just (s { wsPC = wsPC s + 1 })  -- unknown pred; treat as failing goal
    -- Atom as 0-arity goal (e.g. \+ some_pred)
    Just (Atom fnId) ->
      let goalKey = lookupAtom tbl fnId ++ "/0"
      in case Map.lookup goalKey (wcLabels ctx) of
           Just pc ->
             let snapshot = s { wsPC = pc
                              , wsRegs = wsRegs s
                              , wsCP = 0
                              , wsCutBar = 0 }
             in case run ctx snapshot of
                  Just _  -> Nothing
                  Nothing -> Just (s { wsPC = wsPC s + 1 })
           Nothing -> Just (s { wsPC = wsPC s + 1 })
    _ -> Nothing

-- SwitchOnConstant: dispatch on A1 value via O(log n) Map lookup
step !ctx s (SwitchOnConstant table) =
  let val = derefVar (wsBindings s) <$> IM.lookup 1 (wsRegs s)
  in case val of
    Just (Unbound _) -> Just (s { wsPC = wsPC s + 1 })  -- unbound: skip
    Just v -> case Map.lookup v table of
      Just label -> case Map.lookup label (wcLabels ctx) of
        Just pc -> Just (s { wsPC = pc })
        Nothing -> Nothing
      Nothing -> Nothing  -- no match: fail
    Nothing -> Nothing

step !ctx s (BuiltinCall ">/2" _) =
  let v1 = evalArith (wcInternTable ctx) (wsBindings s) =<< (derefVar (wsBindings s) <$> IM.lookup 1 (wsRegs s))
      v2 = evalArith (wcInternTable ctx) (wsBindings s) =<< (derefVar (wsBindings s) <$> IM.lookup 2 (wsRegs s))
  in case (v1, v2) of
    (Just a, Just b) | a > b -> Just (s { wsPC = wsPC s + 1 })
    _ -> Nothing

-- member/2 builtin: A1=Elem, A2=List. Creates choice points for backtracking.
step !ctx s (BuiltinCall "member/2" _) =
  let elem_ = derefVar (wsBindings s) $ fromMaybe (Unbound (-1)) (IM.lookup 1 (wsRegs s))
      list_ = derefVar (wsBindings s) $ fromMaybe (VList []) (IM.lookup 2 (wsRegs s))
  in case list_ of
    VList (x:_) -> unifyVal elem_ x s
    _ -> Nothing

-- begin_aggregate: push aggregate frame CP, initialize accumulator, continue to goal body
step !ctx s (BeginAggregate typ valReg resReg) =
  let cp = ChoicePoint
        { cpNextPC   = wsPC s
        , cpRegs     = wsRegs s
        , cpStack    = wsStack s
        , cpCP       = wsCP s
        , cpTrailLen = wsTrailLen s
        , cpHeapLen  = wsHeapLen s
        , cpBindings = wsBindings s
        , cpCutBar   = wsCutBar s
        , cpAggFrame = Just (AggFrame typ valReg resReg 0
                                      (inferMergeStrategy typ))
        , cpBuiltin = Nothing
        }
  in Just (s { wsPC = wsPC s + 1
             , wsCPs = cp : wsCPs s
             , wsCPsLen = wsCPsLen s + 1
             , wsAggAccum = []
             })

-- end_aggregate: collect value, store returnPC in nearest aggregate frame, force backtrack
step !ctx s (EndAggregate valReg) =
  let val = derefVar (wsBindings s) $ fromMaybe (Integer 0) (getReg valReg s)
      returnPC = wsPC s + 1
      -- Update only the nearest (first) aggregate frame CP, not all CPs
      updatedCPs = updateNearestAggFrame returnPC (wsCPs s)
      s1 = s { wsAggAccum = val : wsAggAccum s, wsCPs = updatedCPs }
  in case backtrackInner returnPC s1 of
    Just s2 -> Just s2
    Nothing -> finalizeAggregate returnPC s1

-- functor/3: A1 = T, A2 = N, A3 = A. Read and construct modes
-- are dispatched on A1's tag after dereferencing.
step !_ctx s (BuiltinCall "functor/3" _) =
  let t = derefVar (wsBindings s) <$> IM.lookup 1 (wsRegs s)
  in case t of
    Just (Unbound vid) ->
      -- Construct mode: need A2 (atom name) and A3 (integer arity).
      let nArg = derefVar (wsBindings s) <$> IM.lookup 2 (wsRegs s)
          aArg = derefVar (wsBindings s) <$> IM.lookup 3 (wsRegs s)
      in case (nArg, aArg) of
        (Just nameVal, Just (Integer arity)) | arity >= 0 ->
          let mBuilt = if arity == 0
                then Just (nameVal, wsVarCounter s)
                else case nameVal of
                  Atom fname ->
                    let c0 = wsVarCounter s
                        newArgs = [Unbound (c0 + i) | i <- [0 .. arity - 1]]
                    in Just (Str fname newArgs, c0 + arity)
                  _ -> Nothing
          in case mBuilt of
            Nothing -> Nothing
            Just (built, newCounter) -> Just (s
              { wsPC = wsPC s + 1
              , wsRegs = IM.insert 1 built (wsRegs s)
              , wsBindings = IM.insert vid built (wsBindings s)
              , wsTrail = TrailEntry vid (IM.lookup vid (wsBindings s)) : wsTrail s
              , wsTrailLen = wsTrailLen s + 1
              , wsVarCounter = newCounter
              })
        _ -> Nothing
    Just tVal ->
      -- Read mode: extract functor name and arity.
      let mInfo = case tVal of
            Str fnId args -> Just (Atom fnId, length args)
            VList [] -> Just (Atom atomNil, 0)
            VList _ -> Just (Atom atomDot, 2)
            Atom _ -> Just (tVal, 0)
            Integer _ -> Just (tVal, 0)
            Float _ -> Just (tVal, 0)
            _ -> Nothing
      in case mInfo of
        Nothing -> Nothing
        Just (name, arity) ->
          case bindOutput 2 name s of
            Nothing -> Nothing
            Just s1 -> case bindOutput 3 (Integer arity) s1 of
              Nothing -> Nothing
              Just s2 -> Just (s2 { wsPC = wsPC s2 + 1 })
    Nothing -> Nothing

-- arg/3: A1 = N (integer, 1-based), A2 = T (compound/list),
-- A3 = output unified with the selected argument.
step !_ctx s (BuiltinCall "arg/3" _) =
  let n = derefVar (wsBindings s) <$> IM.lookup 1 (wsRegs s)
      t = derefVar (wsBindings s) <$> IM.lookup 2 (wsRegs s)
  in case (n, t) of
    (Just (Integer idx), Just tVal) | idx >= 1 ->
      let mArg = case tVal of
            Str _ args | idx <= length args -> Just (args !! (idx - 1))
            VList (x : _) | idx == 1 -> Just x
            VList (_ : xs) | idx == 2 -> Just (VList xs)
            _ -> Nothing
      in case mArg of
        Nothing -> Nothing
        Just a -> case bindOutput 3 a s of
          Nothing -> Nothing
          Just s1 -> Just (s1 { wsPC = wsPC s1 + 1 })
    _ -> Nothing

-- Specialized arg lowering: Arg N tReg aReg
-- Compile-time N (positive integer), runtime T from tReg, output to
-- aReg. Skips the put_constant/put_value/builtin_call dispatch chain
-- that the generic arg/3 builtin requires. Emitted by the WAM compiler
-- when binding-state analysis proves T is bound and N is a literal int.
step !_ctx s (Arg n tReg aReg) | n >= 1 =
  let mT = derefVar (wsBindings s) <$> IM.lookup tReg (wsRegs s)
  in case mT of
    Just tVal ->
      let mElem = case tVal of
            Str _ args | n <= length args -> Just (args !! (n - 1))
            VList (x : _) | n == 1 -> Just x
            VList (_ : xs) | n == 2 -> Just (VList xs)
            _ -> Nothing
      in case mElem of
        Nothing -> Nothing
        Just elem ->
          let mA = derefVar (wsBindings s) <$> IM.lookup aReg (wsRegs s)
          in case mA of
            Nothing ->
              Just (s { wsPC = wsPC s + 1
                      , wsRegs = IM.insert aReg elem (wsRegs s)
                      })
            Just (Unbound vid) ->
              Just (s { wsPC = wsPC s + 1
                      , wsRegs = IM.insert aReg elem (wsRegs s)
                      , wsBindings = IM.insert vid elem (wsBindings s)
                      , wsTrail = TrailEntry vid (IM.lookup vid (wsBindings s)) : wsTrail s
                      , wsTrailLen = wsTrailLen s + 1
                      })
            Just a | a == elem -> Just (s { wsPC = wsPC s + 1 })
            _ -> Nothing
    Nothing -> Nothing
step !_ctx _ (Arg _ _ _) = Nothing

-- Specialized \+ member(X, L) lowering: NotMemberList xReg lReg.
-- Skips the put_structure member/2 + set_value + set_value +
-- builtin_call \+/1 chain (4 dispatches, plus the heap allocation
-- for the goal term) by reading X and L directly from registers and
-- walking L inline. Emitted by the WAM compiler when binding-state
-- analysis proves both X and L are bound at the goal site.
step !_ctx s (NotMemberList xReg lReg) =
  let mX = derefVar (wsBindings s) <$> IM.lookup xReg (wsRegs s)
      mL = derefVar (wsBindings s) <$> IM.lookup lReg (wsRegs s)
  in case (mX, mL) of
    (Just x, Just l) ->
      let found = case l of
            VList items -> any (\item -> derefVar (wsBindings s) item == x) items
            _ -> False
      in if found then Nothing else Just (s { wsPC = wsPC s + 1 })
    _ -> Nothing

-- Specialized \+ member(X, [a, b, c]) for compile-time-ground lists.
-- Atoms are interned at codegen, so the instruction carries [Int] of
-- atom IDs. Single dispatch, zero heap allocation. Beats both the
-- unlowered builtin path (N PutStructures + dispatch + N unifications)
-- and the IntSet inline-build path (N SetInserts allocate Patricia
-- nodes per call) at small N typical of source-literal lists.
--
-- Semantics: succeed when X cannot unify with any atom in the baked-in
-- list. For an Atom X, that is "aid notElem atomIds". For a non-Atom
-- ground value (Integer, Float, Str, VList, VSet) X can never unify
-- with any atom, so the check trivially succeeds. For an Unbound X,
-- it COULD unify with some atom (Prolog would succeed via
-- unification), so the check must fail — matches Prolog
-- \+ member(X, [a,b,c]) semantics when X is unbound.
step !_ctx s (NotMemberConstAtoms xReg atomIds) =
  let mX = derefVar (wsBindings s) <$> IM.lookup xReg (wsRegs s)
  in case mX of
    Just (Atom aid)    -> if aid `elem` atomIds
                            then Nothing
                            else Just (s { wsPC = wsPC s + 1 })
    Just (Unbound _)   -> Nothing  -- could unify with some atom
    Just (Ref _)       -> Nothing  -- unresolved chain — treat as could-unify
    Just _             -> Just (s { wsPC = wsPC s + 1 })  -- non-atom ground: never in list
    Nothing            -> Nothing  -- register not set

-- IntSet visited support: write an empty set into the named register.
-- Used by the WAM compiler at the bootstrap site of a visited-set
-- argument (e.g. the `[Cat]` literal flowing into category_ancestor)
-- so the recursive call sees a VSet rather than a VList.
step !_ctx s (BuildEmptySet r) =
  Just (s { wsPC = wsPC s + 1
          , wsRegs = IM.insert r (VSet IS.empty) (wsRegs s) })

-- IntSet insert: read element from elemReg (must deref to Atom), read
-- the input VSet from inReg, write the inserted set to outReg. Returns
-- Nothing if the element is not an atom or the input is not a VSet.
step !_ctx s (SetInsert eReg inReg outReg) =
  let mE  = derefVar (wsBindings s) <$> IM.lookup eReg (wsRegs s)
      mIn = derefVar (wsBindings s) <$> IM.lookup inReg (wsRegs s)
  in case (mE, mIn) of
    (Just (Atom aid), Just (VSet s0)) ->
      Just (s { wsPC = wsPC s + 1
              , wsRegs = IM.insert outReg (VSet (IS.insert aid s0)) (wsRegs s) })
    _ -> Nothing

-- IntSet membership: succeed when elemReg (an Atom) is NOT in setReg
-- (a VSet). O(log N) lookup, replacing the O(N) walk of NotMemberList
-- on the visited-set hot path.
step !_ctx s (NotMemberSet eReg setReg) =
  let mE   = derefVar (wsBindings s) <$> IM.lookup eReg (wsRegs s)
      mSet = derefVar (wsBindings s) <$> IM.lookup setReg (wsRegs s)
  in case (mE, mSet) of
    (Just (Atom aid), Just (VSet s0)) ->
      if IS.member aid s0 then Nothing else Just (s { wsPC = wsPC s + 1 })
    _ -> Nothing

-- =../2 (univ): A1 = T, A2 = L. Decompose (instantiated A1) or
-- compose (unbound A1, list in A2).
step !_ctx s (BuiltinCall "=../2" _) =
  let t = derefVar (wsBindings s) <$> IM.lookup 1 (wsRegs s)
  in case t of
    Just (Unbound vid) ->
      -- Compose mode: read proper list from A2.
      let l = derefVar (wsBindings s) <$> IM.lookup 2 (wsRegs s)
      in case l of
        Just (VList items) ->
          let mBuilt = case items of
                [] -> Nothing
                [x] -> Just x
                (Atom fname : rest) -> Just (Str fname rest)
                _ -> Nothing
          in case mBuilt of
            Nothing -> Nothing
            Just built -> Just (s
              { wsPC = wsPC s + 1
              , wsRegs = IM.insert 1 built (wsRegs s)
              , wsBindings = IM.insert vid built (wsBindings s)
              , wsTrail = TrailEntry vid (IM.lookup vid (wsBindings s)) : wsTrail s
              , wsTrailLen = wsTrailLen s + 1
              })
        _ -> Nothing
    Just tVal ->
      -- Decompose mode: build list from T.
      let mList = case tVal of
            Str fnId args -> Just (VList (Atom fnId : args))
            Atom _ -> Just (VList [tVal])
            Integer _ -> Just (VList [tVal])
            Float _ -> Just (VList [tVal])
            VList [] -> Just (VList [Atom atomNil])
            VList (x : xs) -> Just (VList [Atom atomDot, x, VList xs])
            _ -> Nothing
      in case mList of
        Nothing -> Nothing
        Just lv -> case bindOutput 2 lv s of
          Nothing -> Nothing
          Just s1 -> Just (s1 { wsPC = wsPC s1 + 1 })
    Nothing -> Nothing

-- copy_term/2: A1 = T, A2 = Copy. Walks T with a var map to
-- preserve sharing, bumps wsVarCounter, binds A2 to the fresh copy.
step !_ctx s (BuiltinCall "copy_term/2" _) =
  let t = derefVar (wsBindings s) <$> IM.lookup 1 (wsRegs s)
  in case t of
    Just tVal ->
      let (copy, newCounter, _) = copyTermWalk (wsVarCounter s) IM.empty tVal
          s0 = s { wsVarCounter = newCounter }
      in case bindOutput 2 copy s0 of
        Nothing -> Nothing
        Just s1 -> Just (s1 { wsPC = wsPC s1 + 1 })
    Nothing -> Nothing

-- Fallback for unhandled instructions
step _ _ _ = Nothing

-- | Restore state from the top choice point.
-- Dispatches: aggregate frame -> finalize, builtin -> resumeBuiltin, normal -> restore.
backtrack :: WamState -> Maybe WamState
backtrack s = case wsCPs s of
  [] -> Nothing
  (cp : rest) ->
    -- 1. Aggregate frame: finalize
    case cpAggFrame cp of { Just af -> finalizeAggregate (afReturnPC af) s; Nothing ->
    -- 2. Builtin state: resume (fact_retry etc.)
    case cpBuiltin cp of { Just bs -> resumeBuiltin bs cp rest s; Nothing ->
    -- 3. Normal: restore from CP
    let trailLen = cpTrailLen cp
        diff = wsTrailLen s - trailLen
        newEntries = reverse $ take diff (wsTrail s)
        restoredBindings = foldl' undoBinding (cpBindings cp) newEntries
    in Just s { wsPC       = cpNextPC cp
              , wsRegs     = cpRegs cp
              , wsStack    = cpStack cp
              , wsCP       = cpCP cp
              , wsTrail    = drop diff (wsTrail s)
              , wsTrailLen = trailLen
              , wsHeap     = take (cpHeapLen cp) (wsHeap s)
              , wsHeapLen  = cpHeapLen cp
              , wsBindings = restoredBindings
              , wsCutBar   = cpCutBar cp
              } } }
  where
    undoBinding bindings (TrailEntry vid mOld) =
      case mOld of
        Just old -> IM.insert vid old bindings
        Nothing  -> IM.delete vid bindings

-- | Resume a builtin choice point. Tries next match, updates or pops CP.
resumeBuiltin :: BuiltinState -> ChoicePoint -> [ChoicePoint] -> WamState -> Maybe WamState
resumeBuiltin (FactRetry _ [] _) _ rest s =
  backtrack (s { wsCPs = rest, wsCPsLen = wsCPsLen s - 1 })
resumeBuiltin (FactRetry vid (vId:vs) retPC) cp rest s =
  let newBindings = IM.insert vid (Atom vId) (cpBindings cp)
      newRegs = IM.insert 2 (Atom vId) (cpRegs cp)
      newCPs = case vs of
        [] -> rest
        _  -> cp { cpBuiltin = Just (FactRetry vid vs retPC) } : rest
      diff = wsTrailLen s - cpTrailLen cp
  in Just s { wsPC = retPC, wsRegs = newRegs, wsStack = cpStack cp
            , wsCP = cpCP cp
            , wsTrail = drop diff (wsTrail s)
            , wsTrailLen = cpTrailLen cp
            , wsHeap = take (cpHeapLen cp) (wsHeap s)
            , wsHeapLen = cpHeapLen cp
            , wsBindings = newBindings, wsCutBar = cpCutBar cp, wsCPs = newCPs }
resumeBuiltin (HopsRetry _ [] _) _ rest s =
  backtrack (s { wsCPs = rest, wsCPsLen = wsCPsLen s - 1 })
resumeBuiltin (HopsRetry vid (h:hs) retPC) cp rest s =
  let newBindings = IM.insert vid (Integer (fromIntegral h)) (cpBindings cp)
      newRegs = IM.insert 3 (Integer (fromIntegral h)) (cpRegs cp)
      newCPs = case hs of
        [] -> rest
        _  -> cp { cpBuiltin = Just (HopsRetry vid hs retPC) } : rest
      diff = wsTrailLen s - cpTrailLen cp
  in Just s { wsPC = retPC, wsRegs = newRegs, wsStack = cpStack cp
            , wsCP = cpCP cp
            , wsTrail = drop diff (wsTrail s)
            , wsTrailLen = cpTrailLen cp
            , wsHeap = take (cpHeapLen cp) (wsHeap s)
            , wsHeapLen = cpHeapLen cp
            , wsBindings = newBindings, wsCutBar = cpCutBar cp, wsCPs = newCPs }

-- Multi-output FFI retry: each remaining tuple is already a list of
-- wrapped Values matching outRegs/outVars in order.
resumeBuiltin (FFIStreamRetry _ _ [] _) _ rest s =
  backtrack (s { wsCPs = rest, wsCPsLen = wsCPsLen s - 1 })
resumeBuiltin (FFIStreamRetry outRegs outVars (tuple:rest_tuples) retPC) cp rest s =
  let -- Insert each (reg, value) from the tuple into the registers.
      newRegs = foldr (\(rN, v) m -> IM.insert rN v m) (cpRegs cp)
                      (zip outRegs tuple)
      -- Insert each (varId, value) into bindings, skipping varId = -1
      -- (meaning the output was originally bound, so no binding update).
      newBindings = foldr (\(vid, v) m ->
                             if vid == -1 then m else IM.insert vid v m)
                          (cpBindings cp)
                          (zip outVars tuple)
      newCPs = case rest_tuples of
        [] -> rest
        _  -> cp { cpBuiltin = Just (FFIStreamRetry outRegs outVars rest_tuples retPC) } : rest
      diff = wsTrailLen s - cpTrailLen cp
  in Just s { wsPC = retPC, wsRegs = newRegs, wsStack = cpStack cp
            , wsCP = cpCP cp
            , wsTrail = drop diff (wsTrail s)
            , wsTrailLen = cpTrailLen cp
            , wsHeap = take (cpHeapLen cp) (wsHeap s)
            , wsHeapLen = cpHeapLen cp
            , wsBindings = newBindings, wsCutBar = cpCutBar cp, wsCPs = newCPs }

-- Phase F2: FactStream resume — iterate through inline fact tuples.
-- Each (a1, a2) pair is unified against registers A1/A2. Variable IDs
-- var1/var2 indicate which bindings to update (-1 = skip, already bound).
resumeBuiltin (FactStream _ _ [] _) _ rest s =
  backtrack (s { wsCPs = rest, wsCPsLen = wsCPsLen s - 1 })
resumeBuiltin (FactStream var1 var2 ((a1,a2):rows) retPC) cp rest s =
  let newRegs0 = IM.insert 1 (Atom a1) (cpRegs cp)
      newRegs  = IM.insert 2 (Atom a2) newRegs0
      newBindings0 = if var1 == -1 then cpBindings cp
                     else IM.insert var1 (Atom a1) (cpBindings cp)
      newBindings  = if var2 == -1 then newBindings0
                     else IM.insert var2 (Atom a2) newBindings0
      newCPs = case rows of
        [] -> rest
        _  -> cp { cpBuiltin = Just (FactStream var1 var2 rows retPC) } : rest
      diff = wsTrailLen s - cpTrailLen cp
  in Just s { wsPC = retPC, wsRegs = newRegs, wsStack = cpStack cp
            , wsCP = cpCP cp
            , wsTrail = drop diff (wsTrail s)
            , wsTrailLen = cpTrailLen cp
            , wsHeap = take (cpHeapLen cp) (wsHeap s)
            , wsHeapLen = cpHeapLen cp
            , wsBindings = newBindings, wsCutBar = cpCutBar cp, wsCPs = newCPs }

-- | Backtrack skipping past the aggregate_frame CP. If the top CP is
-- an aggregate frame, return Nothing (inner solutions exhausted).
-- Otherwise, normal backtrack.
backtrackInner :: Int -> WamState -> Maybe WamState
backtrackInner returnPC s = case wsCPs s of
  (cp : _)
    | Just _ <- cpAggFrame cp -> Nothing  -- reached aggregate frame = done
    | otherwise -> backtrack s
  [] -> Nothing

-- | Finalize an aggregate: pop CPs to the aggregate frame, apply the
-- aggregation function, bind the result register.
-- | Update only the nearest aggregate frame CP with returnPC. O(k) where
-- k is the number of inner CPs above the aggregate frame, not O(n) over all CPs.
updateNearestAggFrame :: Int -> [ChoicePoint] -> [ChoicePoint]
updateNearestAggFrame _ [] = []
updateNearestAggFrame rpc (cp:rest) = case cpAggFrame cp of
  Just af -> cp { cpAggFrame = Just af { afReturnPC = rpc } } : rest
  Nothing -> cp : updateNearestAggFrame rpc rest

finalizeAggregate :: Int -> WamState -> Maybe WamState
finalizeAggregate returnPC s = go (wsCPs s)
  where
    go [] = Nothing
    go (cp : rest) = case cpAggFrame cp of
      Just (AggFrame typ _valReg resReg _ _) ->
        let accum = reverse (wsAggAccum s)
            result = applyAggregation typ accum
            -- Restore the CP snapshot state so we can read Y-registers
            -- from the stack (cpRegs only has A/X registers).
            cpState = s { wsRegs = cpRegs cp, wsStack = cpStack cp
                        , wsBindings = cpBindings cp }
            resVal = derefVar (cpBindings cp) <$> getReg resReg cpState
            -- Restore trail to the CP snapshot (drop entries added since)
            diff = wsTrailLen s - cpTrailLen cp
            restoredTrail = drop diff (wsTrail s)
            (finalRegs, finalBindings, finalStack, finalTrail, finalTrailLen) = case resVal of
              Just (Unbound vid) ->
                ( IM.insert resReg result (cpRegs cp)
                , IM.insert vid result (cpBindings cp)
                , putRegStack resReg result (cpStack cp)
                , TrailEntry vid (IM.lookup vid (cpBindings cp)) : restoredTrail
                , cpTrailLen cp + 1
                )
              _ -> (cpRegs cp, cpBindings cp, cpStack cp, restoredTrail, cpTrailLen cp)
        in Just s { wsPC = returnPC
                  , wsRegs = finalRegs
                  , wsStack = finalStack
                  , wsBindings = finalBindings
                  , wsTrail = finalTrail
                  , wsTrailLen = finalTrailLen
                  , wsHeap = take (cpHeapLen cp) (wsHeap s)
                  , wsHeapLen = cpHeapLen cp
                  , wsCP = cpCP cp
                  , wsCPs = rest
                  , wsCPsLen = wsCPsLen s - 1
                  , wsAggAccum = []
                  }
      Nothing -> go rest  -- skip non-aggregate CPs
    putRegStack rid val [] = []
    putRegStack rid val (EnvFrame ecp yregs : rest) =
      EnvFrame ecp (IM.insert rid val yregs) : rest
    putRegStack rid val (x : rest) = x : putRegStack rid val rest

-- ============================================================================
-- Phase 4.2: Intra-query parallel fork at ParTryMeElse
-- ============================================================================

-- | Entry point for ParTryMeElse execution. Picks fork vs sequential.
-- Accepts either a Left label (pre-resolution) or Right targetPC
-- (post-resolution) for the else branch. The "this branch" always
-- starts at wsPC + 1 regardless of which variant fired.
-- | Phase 4.5: minimum branch count below which the fork is not worth
-- the spark overhead. With fewer than this many branches, the fork
-- falls back to sequential TryMeElse. Default 3: a 2-clause predicate
-- (like category_ancestor base+recursive) stays sequential; a
-- multi-clause predicate with 3+ alternatives forks.
--
-- Rationale: parMap rdeepseq has fixed overhead per spark (~5-10μs on
-- GHC 9.x). With 2 branches where one is trivial, the overhead exceeds
-- the benefit. With 3+ balanced branches, the amortized overhead per
-- branch drops below the per-branch work.
forkMinBranches :: Int
forkMinBranches = 3

forkOrSequential :: WamContext -> WamState -> Either String Int -> Maybe WamState
forkOrSequential !ctx s elseTarget =
  case currentAggMergeStrategy s of
    Just ms | isForkableStrategy ms ->
      let elsePC = case elseTarget of
            Right pc -> pc
            Left lbl -> fromMaybe (-1) (Map.lookup lbl (wcLabels ctx))
      in if elsePC > 0
         then let branches = enumerateParBranches ctx (wsPC s) elsePC
              in if length branches >= forkMinBranches
                 then Just (forkParBranches ctx s ms elsePC)
                 else fallback  -- too few branches; overhead > benefit
         else fallback
    _ -> fallback
  where
    fallback = case elseTarget of
      Left lbl -> step ctx s (TryMeElse lbl)
      Right pc -> step ctx s (TryMeElsePc pc)

-- | Locate the nearest surrounding aggregate frame and return its
-- merge strategy. Returns Nothing when no aggregate frame is active.
currentAggMergeStrategy :: WamState -> Maybe MergeStrategy
currentAggMergeStrategy s = go (wsCPs s)
  where
    go [] = Nothing
    go (cp : rest) = case cpAggFrame cp of
      Just af -> Just (afMergeStrategy af)
      Nothing -> go rest

-- | Forkable merge strategies: sum/count (Phase 4.2) +
-- findall/bag/set (Phase 4.3). Race/negation (4.4) are handled
-- outside the aggregate path and do not appear here.
isForkableStrategy :: MergeStrategy -> Bool
isForkableStrategy MergeSumInt    = True
isForkableStrategy MergeSumDouble = True
isForkableStrategy MergeCount     = True
isForkableStrategy MergeFindall   = True
isForkableStrategy MergeBag       = True
isForkableStrategy MergeSet       = True
isForkableStrategy _              = False

-- | Enumerate the entry PCs of every branch in a Par* choice-point
-- chain. The chain is laid out as:
--
--     ParTryMeElse  L1   <-- starting at parPC (wsPC s)
--     <branch 0 body>
--   L1:
--     ParRetryMeElse L2
--     <branch 1 body>
--   L2:
--     ParTrustMe
--     <branch N body>
--
-- Each branch's body begins at `chainOpPC + 1`. We walk the chain by
-- following the else-label of each non-terminal op. Returns the entry
-- PC of every branch in chain order.
enumerateParBranches :: WamContext -> Int -> Int -> [Int]
enumerateParBranches ctx parPC elsePC =
    (parPC + 1) : collectRest elsePC
  where
    (lo, hi) = bounds (wcCode ctx)
    collectRest pc
      | pc < lo || pc > hi = []  -- safety: malformed chain
      | otherwise = case wcCode ctx ! pc of
          ParRetryMeElse nextLabel ->
            (pc + 1) : collectRest (fromMaybe (-1) (Map.lookup nextLabel (wcLabels ctx)))
          ParRetryMeElsePc nextPC ->
            (pc + 1) : collectRest nextPC
          ParTrustMe -> [pc + 1]
          -- Pre-Par variants can appear if someone mixed sequential
          -- and parallel chain entries. Treat them as chain
          -- terminators for safety — the fork still covers everything
          -- up to that point.
          RetryMeElse _   -> []
          RetryMeElsePc _ -> []
          TrustMe         -> []
          _ -> []

-- | Run one branch of a forked Par* chain. Starts from the given
-- branch entry PC with a fresh wsAggAccum, reuses the parent's
-- bindings / CPs / trail. Runs until the branch's own sub-solutions
-- exhaust. Returns the values the branch contributed to the
-- aggregate — the parent merges these across branches.
--
-- Key invariant: when the branch's EndAggregate would fire
-- finalizeAggregate (i.e. the outer aggregate CP is next to pop), we
-- instead *stop* and return wsAggAccum. The fork driver then merges
-- all branches' contributions and calls finalizeAggregate once at
-- the outer level.
runBranchForFork :: WamContext -> WamState -> Int -> [Value]
runBranchForFork !ctx !parent !branchPC =
    let branchInit = parent
          { wsPC = branchPC
          , wsAggAccum = []
          -- Protect parent CPs from being removed by !/0 inside the
          -- branch. The branch's wsCutBar is set to the parent's
          -- current CP depth so only CPs the branch itself creates
          -- can be cut. Without this, a clause like
          --   p(…) :- max_depth(M), length(V,D), D<M, !, …
          -- would pop the parent's aggregate-frame CP.
          , wsCutBar = wsCPsLen parent
          }
    in runBranchLoop branchInit
  where
    runBranchLoop !s
      | wsPC s < fst (bounds (wcCode ctx)) = wsAggAccum s
      | wsPC s > snd (bounds (wcCode ctx)) = wsAggAccum s
      | otherwise =
          let instr = wcCode ctx ! wsPC s
          in case instr of
               EndAggregate valReg ->
                 let val = derefVar (wsBindings s) $
                           fromMaybe (Integer 0) (getReg valReg s)
                     s1 = s { wsAggAccum = val : wsAggAccum s
                            , wsPC = wsPC s + 1 }
                 -- Prefer backtrackInner: if the branch has more
                 -- internal solutions, keep exploring. Otherwise the
                 -- branch is exhausted — return its contribution
                 -- without finalizing the outer aggregate.
                 in case backtrackInner (wsPC s + 1) s1 of
                      Just s2 -> runBranchLoop s2
                      Nothing -> wsAggAccum s1
               -- Suppress nested forks: redirect Par* to sequential
               -- equivalents inside a branch. Only the OUTERMOST
               -- ParTryMeElse (the one that triggered forkParBranches)
               -- actually forks; inner recursive calls to the same
               -- predicate use sequential choice points. Without this,
               -- recursion depth D with branching factor B creates
               -- B^D nested parMap sparks — exponential explosion.
               _ -> let seqInstr = case instr of
                         ParTryMeElse lbl   -> TryMeElse lbl
                         ParTryMeElsePc p   -> TryMeElsePc p
                         ParRetryMeElse lbl -> RetryMeElse lbl
                         ParRetryMeElsePc p -> RetryMeElsePc p
                         ParTrustMe         -> TrustMe
                         other              -> other
                    in case step ctx s seqInstr of
                         Just s2 -> runBranchLoop s2
                         Nothing ->
                           -- Custom backtrack: if the top CP has an
                           -- aggregate frame, DON'T call finalizeAggregate
                           -- (which would clear wsAggAccum). Instead,
                           -- return our accumulated values — the branch
                           -- is done. Without this, the standard
                           -- backtrack function wipes wsAggAccum by
                           -- calling finalizeAggregate when it hits the
                           -- aggregate-frame CP.
                           case wsCPs s of
                             (cp : _) | Just _ <- cpAggFrame cp ->
                               wsAggAccum s
                             _ -> case backtrack s of
                               Just s3 -> runBranchLoop s3
                               Nothing -> wsAggAccum s

-- | Fork every branch of a Par* chain and merge their aggregate
-- contributions via the outer aggregate's strategy. Returns the
-- post-finalize state (ready to resume after the outer EndAggregate).
forkParBranches :: WamContext -> WamState -> MergeStrategy -> Int -> WamState
forkParBranches !ctx !s _strategy !elsePC =
  let branchPCs = enumerateParBranches ctx (wsPC s) elsePC
      branchResults = parMap rdeepseq (runBranchForFork ctx s) branchPCs
      allValues = concat branchResults
      -- Combine into the current state's accumulator before
      -- finalizing. finalizeAggregate's applyAggregation folds
      -- these per the aggregate's afType (which matches the
      -- strategy — sum folds as sum, count counts, etc.).
      combined = s { wsAggAccum = allValues ++ wsAggAccum s }
      -- finalizeAggregate wants the returnPC. For the outer aggregate
      -- this is the PC just after the matching EndAggregate. We
      -- locate it by scanning forward from wsPC s (the ParTryMeElse)
      -- for the first EndAggregate. All Par* branches share this
      -- returnPC because they share the enclosing BeginAggregate.
      retPC = findOuterEndAggregate ctx (wsPC s)
  in case finalizeAggregate retPC combined of
       Just sf -> sf
       Nothing -> combined  -- shouldn't happen; defensive

-- | Scan forward from the given PC looking for the first EndAggregate.
-- Returns the PC immediately after it (the aggregate's return
-- target). Returns 0 on overrun; finalizeAggregate handles that
-- gracefully via its CP walk.
findOuterEndAggregate :: WamContext -> Int -> Int
findOuterEndAggregate !ctx !startPC =
    let (_, hi) = bounds (wcCode ctx)
    in go (startPC + 1) hi
  where
    go !pc !hi
      | pc > hi   = 0
      | otherwise = case wcCode ctx ! pc of
          EndAggregate _ -> pc + 1
          _              -> go (pc + 1) hi

-- | Phase 4.4: parallel negation check with race-to-cancel.
-- Spawn each branch as an async action; the first to return True
-- (goal succeeded → negation fails) wins and all others are cancelled.
-- If all branches return False, negation succeeds. Uses
-- unsafePerformIO to keep the WAM run loop pure — safe because the
-- branches are purity-certified and the only IO effect is thread
-- management.
-- {-# NOINLINE runNegationParallel #-}
runNegationParallel :: WamContext -> WamState -> Int -> Int -> Bool
runNegationParallel !ctx !s !entryPC !elsePC =
    let branchPCs = enumerateParBranches ctx entryPC elsePC
        branchAction pc = evaluate $
          let snapshot = s { wsPC = pc + 1  -- skip past the Par* instruction
                           , wsCP = 0
                           , wsCutBar = 0 }
          in case run ctx snapshot of
               Just _  -> True
               Nothing -> False
    in if length branchPCs >= forkMinBranches
       then unsafePerformIO (raceToTrue (map branchAction branchPCs))
       else case run ctx (s { wsPC = entryPC, wsCP = 0, wsCutBar = 0 }) of
              Just _  -> True
              Nothing -> False

-- | Run a list of IO Bool actions concurrently. Returns True as soon
-- as any action returns True, cancelling all others. If every action
-- returns False, returns False. Uses waitAny to poll completed
-- asyncs and cancel to clean up.
raceToTrue :: [IO Bool] -> IO Bool
raceToTrue [] = return False
raceToTrue actions = do
    asyncs <- mapM async actions
    result <- go asyncs
    mapM_ cancel asyncs
    return result
  where
    go [] = return False
    go as = do
      (completed, val) <- waitAny as
      if val
        then return True
        else go (filter (/= completed) as)

-- | Apply aggregation function to collected values.
applyAggregation :: String -> [Value] -> Value
applyAggregation "sum" vals =
  let toNum (Integer n) = fromIntegral n
      toNum (Float f) = f
      toNum _ = 0
      s = sum (map toNum vals)
  in if fromIntegral (round s :: Int) == s then Integer (round s) else Float s
applyAggregation "count" vals = Integer (length vals)
applyAggregation "collect" vals = VList vals
applyAggregation "bag" vals = VList vals
applyAggregation "set" vals = VList (nub vals)
applyAggregation _ vals = VList vals

-- ============================================================================
-- Foreign Function Interface: native Haskell implementations of expensive
-- recursive predicates. Auto-generated from kernel detection.
-- ============================================================================

-- | Native depth-bounded DFS for the category_ancestor kernel.
-- Auto-generated from kernel detection. Edge predicate: category_parent.
-- Atoms are interned at the FFI boundary (see executeForeign) so the
-- hot loop uses IntMap + Int comparison instead of HashMap String hashing.
--
-- The public signature accepts visited as [Int] (the FFI marshalling
-- format extracted from VList). On entry we convert once to IntSet for
-- O(log N) membership checks during the recursive descent — replaces
-- the prior O(N) list `elem`. Bang patterns on the loop variables force
-- strict evaluation so each recursive call commits to a concrete
-- (acc, cat, depth, visited-set) tuple instead of building thunks.
--
-- Hop counts are accumulated by passing an `acc` argument down the
-- recursion — each call returns absolute hop counts directly instead of
-- relying on `map (+1)` over the result list. This eliminates one list
-- traversal per recursion level: at deep paths the cumulative
-- traversal cost was O(N²) without fusion, and Haskell list fusion
-- doesn't fire reliably across `(++)` boundaries (baseHits ++ recHits
-- breaks the build/foldr pattern). Accumulator-passing avoids that
-- dependency on optimisation luck.
{-# INLINE nativeKernel_category_ancestor #-}
nativeKernel_category_ancestor :: EdgeLookup -> Int -> Int -> Int -> Int -> [Int] -> [Int]
nativeKernel_category_ancestor parents cat root maxDepth depth visited =
  go 0 cat depth (IS.fromList visited)
  where
    go !acc !c !d !v =
      let directParents = parents c
          hop = acc + 1
          baseHits = [hop | p <- directParents, p == root]
          recHits = if d >= maxDepth then [] else
            concatMap (\mid ->
              if mid `IS.member` v then []
              else go hop mid (d+1) (IS.insert mid v)
            ) directParents
      in baseHits ++ recHits


-- | Execute a foreign predicate call. Computes all results natively,
-- returns first result with CPs for the rest.
-- | Indexed fact dispatch for 2-arg facts via BuiltinState CP.
-- O(1) Map lookup, first match returned, FactRetry CP for the rest.
callIndexedFact2 :: WamContext -> String -> WamState -> Maybe WamState
callIndexedFact2 !ctx pred s =
  let basePred = takeWhile (/= '/') pred
      retPC = wsCP s
  in case Map.lookup basePred (wcForeignFacts ctx) of
    Nothing -> Nothing
    Just factIndex ->
      let tbl = wcInternTable ctx
          a1 = derefVar (wsBindings s) $ fromMaybe (Atom atomEmpty) (IM.lookup 1 (wsRegs s))
          a2 = derefVar (wsBindings s) $ fromMaybe (Unbound (-1)) (IM.lookup 2 (wsRegs s))
      in case a1 of
        Atom aid -> case Map.lookup (lookupAtom tbl aid) factIndex of
          Just (v:rest) -> case a2 of
            Unbound vid ->
              let vId = internAtomPure tbl v
                  newRegs = IM.insert 2 (Atom vId) (wsRegs s)
                  newBindings = IM.insert vid (Atom vId) (wsBindings s)
                  newTrail = TrailEntry vid (IM.lookup vid (wsBindings s)) : wsTrail s
                  restIds = map (internAtomPure tbl) rest
                  newCPs = case restIds of
                    [] -> wsCPs s  -- single match, no CP
                    _  -> ChoicePoint
                            { cpNextPC = retPC, cpRegs = wsRegs s, cpStack = wsStack s
                            , cpCP = wsCP s, cpTrailLen = wsTrailLen s
                            , cpHeapLen = wsHeapLen s, cpBindings = wsBindings s
                            , cpCutBar = wsCutBar s, cpAggFrame = Nothing
                            , cpBuiltin = Just (FactRetry vid restIds retPC)
                            } : wsCPs s
                  newCPsLen = case restIds of { [] -> wsCPsLen s; _ -> wsCPsLen s + 1 }
              in Just (s { wsPC = retPC, wsRegs = newRegs, wsBindings = newBindings
                         , wsTrail = newTrail, wsTrailLen = wsTrailLen s + 1
                         , wsCPs = newCPs, wsCPsLen = newCPsLen })
            Atom existingId ->
              let vId2 = internAtomPure tbl v
                  restIds2 = map (internAtomPure tbl) rest
              in if existingId == vId2 then Just (s { wsPC = retPC })
              else case filter (== existingId) restIds2 of
                (_:_) -> Just (s { wsPC = retPC })
                [] -> Nothing
            _ -> Nothing
          _ -> Nothing
        _ -> Nothing

-- | Phase F2/F4: Stream through fact tuples for a 2-arg predicate.
-- Checks wcInlineFacts first (Phase F3 compiled literals), then falls
-- back to wcFactSources (Phase F4 external sources via FactSource).
-- The FactSource path uses unsafePerformIO to bridge IO into the pure
-- WAM interpreter — safe because fact sources are read-only after the
-- force barrier in Main.hs.
streamFacts :: WamContext -> String -> WamState -> Maybe WamState
streamFacts !ctx pred s =
  case Map.lookup pred (wcInlineFacts ctx) of
    Just rows@(_:_) -> streamFactRows rows s
    _ -> case Map.lookup pred (wcFactSources ctx) of
      Nothing -> Nothing
      Just fs ->
        let a1val = derefVar (wsBindings s) $ fromMaybe (Unbound (-1)) (IM.lookup 1 (wsRegs s))
            rows = unsafePerformIO $ case a1val of
              Atom aid -> fsLookupArg1 fs aid
              _        -> fsScan fs
        in streamFactRows rows s

-- | Core row-streaming logic shared by inline and external paths.
streamFactRows :: [(Int, Int)] -> WamState -> Maybe WamState
streamFactRows [] _ = Nothing
streamFactRows rows s =
  let retPC = wsCP s
      a1val = derefVar (wsBindings s) $ fromMaybe (Unbound (-1)) (IM.lookup 1 (wsRegs s))
      a2val = derefVar (wsBindings s) $ fromMaybe (Unbound (-1)) (IM.lookup 2 (wsRegs s))
      var1 = case a1val of { Unbound vid -> vid; _ -> -1 }
      var2 = case a2val of { Unbound vid -> vid; _ -> -1 }
      -- Filter rows by any bound arguments
      filtered = case (a1val, a2val) of
        (Atom aid, Atom bid) -> filter (\(a, b) -> a == aid && b == bid) rows
        (Atom aid, _)       -> filter (\(a, _) -> a == aid) rows
        (_, Atom bid)       -> filter (\(_, b) -> b == bid) rows
        _                   -> rows
  in case filtered of
    [] -> Nothing
    ((a1,a2):rest) ->
      let newRegs = IM.insert 2 (Atom a2) $ IM.insert 1 (Atom a1) (wsRegs s)
          newBindings0 = if var1 == -1 then wsBindings s
                         else IM.insert var1 (Atom a1) (wsBindings s)
          newBindings  = if var2 == -1 then newBindings0
                         else IM.insert var2 (Atom a2) newBindings0
          newTrail0 = if var1 == -1 then wsTrail s
                      else TrailEntry var1 (IM.lookup var1 (wsBindings s)) : wsTrail s
          newTrail  = if var2 == -1 then newTrail0
                      else TrailEntry var2 (IM.lookup var2 newBindings0) : newTrail0
          trailAdded = (if var1 == -1 then 0 else 1) + (if var2 == -1 then 0 else 1)
          newCPs = case rest of
            [] -> wsCPs s
            _  -> ChoicePoint
                    { cpNextPC = retPC, cpRegs = wsRegs s, cpStack = wsStack s
                    , cpCP = wsCP s, cpTrailLen = wsTrailLen s
                    , cpHeapLen = wsHeapLen s, cpBindings = wsBindings s
                    , cpCutBar = wsCutBar s, cpAggFrame = Nothing
                    , cpBuiltin = Just (FactStream var1 var2 rest retPC)
                    } : wsCPs s
          newCPsLen = case rest of { [] -> wsCPsLen s; _ -> wsCPsLen s + 1 }
      in Just (s { wsPC = retPC, wsRegs = newRegs, wsBindings = newBindings
                 , wsTrail = newTrail, wsTrailLen = wsTrailLen s + trailAdded
                 , wsCPs = newCPs, wsCPsLen = newCPsLen })

-- | Phase F4: Build a FactSource from a TSV file with lazy IO.
-- The file is parsed lazily on first fsScan/fsLookupArg1 call.
-- The index (IntMap grouping by arg1) is built on first demand.
tsvFactSource :: InternTable -> FilePath -> IO FactSource
tsvFactSource tbl path = do
    content <- readFile path  -- lazy IO
    let ls = drop 1 (lines content)  -- skip header
        rows = [ (internAtomPure tbl a, internAtomPure tbl b)
               | l <- ls, not (null l)
               , let (a, rest) = break (== '\t') l
               , not (null rest)
               , let b = drop 1 rest  -- skip the tab
               , not (null b)
               ]
        index = IM.fromListWith (++) [(k, [(k, v)]) | (k, v) <- rows]
    return FactSource
      { fsScan       = return rows
      , fsLookupArg1 = \key -> return $ IM.findWithDefault [] key index
      , fsClose      = return ()
      }

-- | Phase F4: Wrap an existing strict IntMap as a FactSource.
-- Used to bridge the current eager-load path into the FactSource interface.
-- | Phase B1: Wrap an IntMap as an EdgeLookup (default, always available).
intMapEdgeLookup :: IM.IntMap [Int] -> EdgeLookup
intMapEdgeLookup im key = IM.findWithDefault [] key im

intMapFactSource :: IM.IntMap [Int] -> FactSource
intMapFactSource im = FactSource
  { fsScan       = return [(k, v) | (k, vs) <- IM.toList im, v <- vs]
  , fsLookupArg1 = \key -> return $ map (\v -> (key, v)) $ IM.findWithDefault [] key im
  , fsClose      = return ()
  }

executeForeign :: WamContext -> String -> WamState -> Maybe WamState
executeForeign !ctx "category_ancestor/4" s =
  let r1 = derefVar (wsBindings s) $ fromMaybe (Atom atomEmpty) (IM.lookup 1 (wsRegs s))
      r2 = derefVar (wsBindings s) $ fromMaybe (Atom atomEmpty) (IM.lookup 2 (wsRegs s))
      r4 = derefVar (wsBindings s) $ fromMaybe (VList []) (IM.lookup 4 (wsRegs s))
      category_parent_facts = case Map.lookup "category_parent" (wcEdgeLookups ctx) of
        Just lkp -> lkp
        Nothing  -> intMapEdgeLookup (fromMaybe IM.empty $ Map.lookup "category_parent" (wcFfiFacts ctx))
      max_depth_cfg = fromMaybe 10 $ Map.lookup "max_depth" (wcForeignConfig ctx)
  in case (r1, r2, r4) of
    (Atom r1I, Atom r2I, VList r4L) ->
      let results = nativeKernel_category_ancestor category_parent_facts r1I r2I max_depth_cfg (length [v | Atom v <- r4L]) [v | Atom v <- r4L]
          retPC = wsCP s
          outReg_3 = derefVar (wsBindings s) $ fromMaybe (Unbound (-1)) (IM.lookup 3 (wsRegs s))
          bindResult rv_1 =
            let w_1 = Integer (fromIntegral rv_1)
            in s { wsPC = retPC
               , wsRegs = IM.insert 3 w_1 $ wsRegs s
               , wsBindings = (case outReg_3 of { Unbound v -> IM.insert v w_1; _ -> id }) $ wsBindings s
               , wsTrail = (case outReg_3 of { Unbound v -> (TrailEntry v (IM.lookup v (wsBindings s)) :); _ -> id }) $ wsTrail s
               , wsTrailLen = wsTrailLen s + 1
               }
      in case results of
        [] -> Nothing
        [h] -> Just (bindResult h)
        (h:restResults) ->
          let s1 = bindResult h
              outVars = [case outReg_3 of { Unbound v -> v; _ -> -1 }]
              restWrapped = map (\rv_1 -> [Integer (fromIntegral rv_1)]) restResults
              cp = ChoicePoint
                { cpNextPC = retPC, cpRegs = wsRegs s, cpStack = wsStack s
                , cpCP = wsCP s, cpTrailLen = wsTrailLen s
                , cpHeapLen = wsHeapLen s, cpBindings = wsBindings s
                , cpCutBar = wsCutBar s, cpAggFrame = Nothing
                , cpBuiltin = Just (FFIStreamRetry [3] outVars restWrapped retPC)
                }
          in Just (s1 { wsCPs = cp : wsCPs s, wsCPsLen = wsCPsLen s + 1 })
    _ -> Nothing

executeForeign _ _ _ = Nothing


-- | Bind an output register to a value WITHOUT advancing PC.
-- Used by term-inspection builtins that need to bind multiple
-- output positions in sequence before a single PC advance at the
-- end of the case. If the register is already bound to an equal
-- value, succeeds without side-effects; if it's bound to an
-- unequal value, fails. Otherwise binds and trails.
bindOutput :: Int -> Value -> WamState -> Maybe WamState
bindOutput reg val s = case derefVar (wsBindings s) <$> IM.lookup reg (wsRegs s) of
  Just (Unbound vid) -> Just (s
    { wsRegs = IM.insert reg val (wsRegs s)
    , wsBindings = IM.insert vid val (wsBindings s)
    , wsTrail = TrailEntry vid (IM.lookup vid (wsBindings s)) : wsTrail s
    , wsTrailLen = wsTrailLen s + 1
    })
  Just existing | existing == val -> Just s
  _ -> Nothing

-- | copy_term/2 walker: recursively copies a Value, mapping each
-- distinct source variable id to exactly one fresh destination
-- variable id to preserve sharing within the copy. Threaded state
-- is (counter, varMap). Atomic values clone as-is.
copyTermWalk :: Int -> IM.IntMap Int -> Value -> (Value, Int, IM.IntMap Int)
copyTermWalk !c !m (Unbound vid) = case IM.lookup vid m of
  Just nv -> (Unbound nv, c, m)
  Nothing -> (Unbound c, c + 1, IM.insert vid c m)
copyTermWalk !c !m (Str fn args) =
  let (newArgs, c1, m1) = copyTermArgs c m args
  in (Str fn newArgs, c1, m1)
copyTermWalk !c !m (VList items) =
  let (newItems, c1, m1) = copyTermArgs c m items
  in (VList newItems, c1, m1)
copyTermWalk !c !m v = (v, c, m)

copyTermArgs :: Int -> IM.IntMap Int -> [Value] -> ([Value], Int, IM.IntMap Int)
copyTermArgs !c !m [] = ([], c, m)
copyTermArgs !c !m (x : xs) =
  let (x1, c1, m1) = copyTermWalk c m x
      (xs1, c2, m2) = copyTermArgs c1 m1 xs
  in (x1 : xs1, c2, m2)

-- | Unify two values, binding unbound variables.
unifyVal :: Value -> Value -> WamState -> Maybe WamState
unifyVal (Unbound vid) val s =
  Just (s { wsPC = wsPC s + 1
          , wsBindings = IM.insert vid val (wsBindings s)
          , wsTrail = TrailEntry vid (IM.lookup vid (wsBindings s)) : wsTrail s
          , wsTrailLen = wsTrailLen s + 1
          })
unifyVal val (Unbound vid) s =
  Just (s { wsPC = wsPC s + 1
          , wsBindings = IM.insert vid val (wsBindings s)
          , wsTrail = TrailEntry vid (IM.lookup vid (wsBindings s)) : wsTrail s
          , wsTrailLen = wsTrailLen s + 1
          })
unifyVal a b s | a == b = Just (s { wsPC = wsPC s + 1 })
               | otherwise = Nothing

-- | Main execution loop. Runs until halt (pc=0) or failure.
-- Uses unsafeFetchInstr to avoid Maybe wrapping in the hot path.
-- Bounds are guaranteed by the WAM compiler: PC=0 is halt, otherwise PC
-- always points to a valid instruction within the code array.
-- The WamContext is read-only and threaded through (no per-step alloc).
run :: WamContext -> WamState -> Maybe WamState
run !ctx !s
  | wsPC s == 0 = Just s  -- halt
  | otherwise =
      let !instr = unsafeFetchInstr (wsPC s) (wcCode ctx)
      in case step ctx s instr of
           Just !s' -> run ctx s'
           Nothing -> case backtrack s of
             Just !s' -> run ctx s'
             Nothing -> Nothing

-- | Dispatch a Call to another predicate, trying all resolution paths.
-- Used by lowered predicate functions for inter-predicate calls.
-- Non-foreign call dispatch for lowered functions. Foreign predicates
-- are dispatched via callForeign (compile-time resolved), so
-- executeForeign is NOT checked here.
{-# NOINLINE dispatchCall #-}
dispatchCall :: WamContext -> String -> WamState -> Maybe WamState
dispatchCall !ctx pred !sc =
  case Map.lookup pred (wcLoweredPredicates ctx) of
    Just fn -> fn ctx sc
    Nothing -> case callIndexedFact2 ctx pred sc of
      Just sr -> Just sr
      Nothing -> case Map.lookup pred (wcLabels ctx) of
        Just pc -> run ctx (sc { wsPC = pc })
        Nothing -> Nothing

-- Foreign call for lowered functions. Calls executeForeign directly;
-- Nothing means no solutions (backtrack). No fallthrough.
{-# INLINE callForeign #-}
callForeign :: WamContext -> String -> WamState -> Maybe WamState
callForeign !ctx pred !sc = executeForeign ctx pred sc
-- | LMDB-backed edge lookup.  Two layout variants are supported,
-- selected at codegen time via the lmdb_layout option:
--
--   - default (no lmdb_dupsort flag): WAM-native ingest format.
--     Single unnamed default db with MDB_INTEGERKEY.  Each key has a
--     single value: a packed array of Int32s holding all parents for
--     that key.  Reader: one mdb_get' returns the whole array.
--
--   - dupsort (lmdb_dupsort flag set): streaming-pipeline ingest format.
--     Named "main" subdb with MDB_DUPSORT + MDB_INTEGERKEY.  Multiple
--     entries per key, each holding one Int32 parent value.  Reader:
--     cursor MDB_SET + MDB_NEXT_DUP iteration.
--
-- Both variants are zero-copy: mdb_get'/peekElemOff read directly from
-- the mmap'd region, no per-lookup allocation or deserialization.

-- | Open an LMDB environment and database handle for edge lookups.
openLmdbEdgeStore :: FilePath -> IO (MDB_env, MDB_dbi')
openLmdbEdgeStore path = do
    env <- mdb_env_create
    mdb_env_set_mapsize env (2 * 1024 * 1024 * 1024)  -- 2 GB

    mdb_env_set_maxdbs env 4


    mdb_env_set_maxreaders env 126

    mdb_env_open env path [MDB_RDONLY]


    txn <- mdb_txn_begin env Nothing

                          True  -- read-only for dupsort layout
    dbi <- mdb_dbi_open' txn (Just "main") [MDB_DUPSORT]


    mdb_txn_commit txn
    return (env, dbi)

-- | Open the raw packed Int32 store read-only.
-- In dupsort layout the store is a named subdb with MDB_DUPSORT (one
-- entry per (key, value) pair), built externally by the streaming
-- pipeline.  Caller passes the subdb name (e.g. "category_parent" for
-- the Phase 1 ingester output, or "main" for legacy streaming-pipeline
-- ingest); `Nothing` falls back to the unnamed default db.
-- In the default layout it is the unnamed default db with MDB_INTEGERKEY,
-- packed Int32 array per key.
openLmdbRawReadonlyStore :: FilePath -> Maybe String -> IO (MDB_env, MDB_dbi')
openLmdbRawReadonlyStore dbPath dbName = do
    env <- mdb_env_create
    mdb_env_set_mapsize env (1024 * 1024 * 1024)
    mdb_env_set_maxdbs env 4
    -- Maxreaders: each parMap-driven Haskell thread that does a
    -- cache miss opens a new read txn (one slot).  GHC's spark
    -- scheduler under -N>1 with rdeepseq can produce many distinct
    -- ThreadIds per spark batch (well above the OS thread count),
    -- so we size generously.  LMDB hard cap is 32767.
    mdb_env_set_maxreaders env 4096
    mdb_env_open env dbPath [MDB_RDONLY]
    txn <- mdb_txn_begin env Nothing True

    dbi <- mdb_dbi_open' txn dbName [MDB_DUPSORT]


    -- Commit (not abort) the bootstrap txn so the dbi handle persists
    -- for subsequent transactions opened against the same env.
    mdb_txn_commit txn
    return (env, dbi)

-- | Create an EdgeLookup backed by raw LMDB (zero-copy mmap reads).
-- Uses a single long-lived read-only transaction opened at startup.
-- The pointer from mdb_get' points directly into the mmap'd region
-- and stays valid for the lifetime of the transaction. For a read-only
-- database (facts don't change during queries), this is safe and
-- eliminates per-lookup transaction overhead entirely.


-- | Per-thread cursor cache for dupsort lookups.  LMDB cursors and
-- read transactions are pinned to the thread that created them, so
-- a parMap-driven workload needs one (txn, cursor) pair per worker
-- thread.  The cache is populated lazily on first lookup from each
-- thread; subsequent lookups from the same thread reuse the cached
-- cursor.  Cursors leak at shutdown (acceptable for batch jobs).
type DupsortCursorCache = IORef (Map.HashMap ThreadId (MDB_txn, MDB_cursor'))

newDupsortCursorCache :: IO DupsortCursorCache
newDupsortCursorCache = newIORef Map.empty

-- | Look up or lazily allocate the cursor for the calling thread.
getOrOpenDupsortCursor :: MDB_env -> MDB_dbi' -> DupsortCursorCache
                       -> IO MDB_cursor'
getOrOpenDupsortCursor env dbi cache = do
    tid <- myThreadId
    m <- readIORef cache
    case Map.lookup tid m of
      Just (_, c) -> return c
      Nothing -> do
        txn <- mdb_txn_begin env Nothing True
        c <- mdb_cursor_open' txn dbi
        atomicModifyIORef' cache
          (\m' -> (Map.insert tid (txn, c) m', ()))
        return c

-- | Dupsort EdgeLookup IO body.  The pure EdgeLookup variant wraps
-- this in unsafePerformIO; callers like the L1 cache that are
-- already in IO can call this directly to avoid the nested
-- unsafePerformIO unwrap on the hot path.
{-# INLINE lmdbRawEdgeLookupIO #-}
lmdbRawEdgeLookupIO :: MDB_env -> MDB_dbi' -> DupsortCursorCache -> Int -> IO [Int]
lmdbRawEdgeLookupIO env dbi cache key = do
    cursor <- getOrOpenDupsortCursor env dbi cache
    allocaBytes 4 $ \kp -> do
      poke (castPtr kp :: Ptr Int32) (fromIntegral key :: Int32)
      alloca $ \kvPtr -> alloca $ \dvPtr -> do
        poke kvPtr (MDB_val 4 kp)
        found <- mdb_cursor_get' MDB_SET cursor kvPtr dvPtr
        if not found
          then return []
          else collectDups cursor kvPtr dvPtr
  where
    collectDups cursor kvPtr dvPtr = do
      v <- readInt32Val dvPtr
      more <- mdb_cursor_get' MDB_NEXT_DUP cursor kvPtr dvPtr
      if more
        then do rest <- collectDups cursor kvPtr dvPtr
                return (v : rest)
        else return [v]

    readInt32Val ptr = do
      MDB_val sz dataPtr <- peek ptr
      if sz >= 4
        then fromIntegral <$> peekElemOff (castPtr dataPtr :: Ptr Int32) 0
        else return 0

-- | Dupsort EdgeLookup: cursor-based iteration over duplicate values.
-- Thread-safe: each calling thread gets its own (txn, cursor) from
-- the per-thread cache.  parMap workloads see no shared mutable state
-- and run without the shared-cursor assertion failure.
lmdbRawEdgeLookup :: MDB_env -> MDB_dbi' -> DupsortCursorCache -> EdgeLookup
lmdbRawEdgeLookup env dbi cache key =
    unsafePerformIO (lmdbRawEdgeLookupIO env dbi cache key)







-- | Phase 2 L2 cache (shared, lock-free).
--
-- A single IOArray shared across all HECs.  Lookups and writes are
-- non-atomic but racy-safe: GHC's IOArray pointer writes are
-- word-aligned and atomic on x86_64, so a reader sees either the
-- old or the new pointer (no torn writes).  Different threads may
-- race to write the same slot; the "winner" overwrites, but every
-- cached value equals the truth (LMDB is read-only), so any winner
-- is correct.  This avoids both atomicModifyIORef contention and
-- the complexity of sharded locking.
--
-- The single shared IOArray means cross-HEC cache hits are possible
-- (vs L1 which is strictly per-HEC).  At the cost of a slightly
-- longer hot path (one extra array indexing per call vs L1) and
-- more potential cache-line bouncing under heavy contention.
data L2Cache = L2Cache
    { l2Arr      :: {-# UNPACK #-} !(IOA.IOArray Int L2Entry)
    , l2Capacity :: {-# UNPACK #-} !Int  -- must be a power of two
    }

data L2Entry
    = L2Empty
    | L2Entry {-# UNPACK #-} !Int [Int]   -- stored key + values


-- | Read /proc/meminfo MemAvailable on Linux.  Falls back to 1 GB if
-- the file is unreadable (non-Linux or sandboxed environment).  This
-- is best-effort sizing; the user can override via
-- lmdb_cache_l2_capacity_bytes(N) in the Prolog options.
readMemAvailableBytes :: IO Int
readMemAvailableBytes = do
    eContents <- E.try (readFile "/proc/meminfo")
                   :: IO (Either E.SomeException String)
    case eContents of
      Left _ -> return defaultBytes
      Right contents ->
        case dropWhile notMemAvailable (lines contents) of
          (line:_) -> case words line of
            (_ : kb : _) | [(n, "")] <- reads kb -> return (n * 1024)
            _ -> return defaultBytes
          _ -> return defaultBytes
  where
    defaultBytes = 1024 * 1024 * 1024  -- 1 GB
    notMemAvailable l = not ("MemAvailable:" `isPrefixOf` l)

-- | L2 memory budget: min(half of available, available - 500 MB).
-- Floors at 0 if RAM is too tight to leave the safety buffer.  The
-- user's design intent: don't use more than half the available RAM,
-- and always leave a 500 MB headroom for the rest of the process.
l2MemoryBudgetBytes :: IO Int
l2MemoryBudgetBytes = do
    available <- readMemAvailableBytes
    let bufferBytes = 500 * 1024 * 1024
        halfBytes   = available `div` 2
        budget      = min halfBytes (available - bufferBytes)
    return $! max 0 budget


-- | Round @n@ down to the nearest power of two (≥ 1).
roundDownPow2 :: Int -> Int
roundDownPow2 n
  | n <= 1    = 1
  | otherwise = go 1
  where
    go x | x * 2 > n = x
         | otherwise = go (x * 2)



-- | Default L2 capacity in entries: derived from the memory budget,
-- ~32 bytes per L2Entry, rounded down to a power of two, bounded
-- [1024, 1M] entries.  Can be overridden by the Prolog option
-- lmdb_cache_l2_capacity_bytes/1.
defaultL2Capacity :: IO Int
defaultL2Capacity = do
    budget <- l2MemoryBudgetBytes
    let bytesPerEntry = 32 :: Int
        rawEntries    = budget `div` bytesPerEntry
        bounded       = max 1024 (min 1048576 rawEntries)
    return $! roundDownPow2 bounded


-- | Allocate the shared L2 cache.  Capacity must be a power of two
-- (caller's responsibility; defaultL2Capacity guarantees this, and
-- the user override should also enforce it).
newL2Cache :: Int -> IO L2Cache
newL2Cache cap = do
    arr <- IOA.newArray (0, cap - 1) L2Empty
    return (L2Cache arr cap)

-- | L2 lookup: O(1) shared array read with key-match check.  Reads
-- are not synchronised; we may see another thread's just-written
-- pointer or its predecessor.  Both are valid cached values.
{-# INLINE l2Lookup #-}
l2Lookup :: L2Cache -> Int -> IO (Maybe [Int])
l2Lookup (L2Cache arr cap) key = do
    let !idx = key .&. (cap - 1)
    e <- IOA.readArray arr idx
    case e of
      L2Entry k vs | k == key -> return (Just vs)
      _                       -> return Nothing

-- | L2 insert: O(1) shared array write.  Two threads racing on the
-- same slot is harmless — both stored values are correct, only the
-- "winning" pointer survives.  Pointer writes are atomic on x86_64;
-- weaker ISAs may need a memory barrier (out of scope for now).
{-# INLINE l2Insert #-}
l2Insert :: L2Cache -> Int -> [Int] -> IO ()
l2Insert (L2Cache arr cap) key vs = do
    let !idx = key .&. (cap - 1)
    IOA.writeArray arr idx (L2Entry key vs)

-- | L2-only EdgeLookup.  On hit, return the cached neighbours.  On
-- miss, fall through to the dupsort lookup and populate the L2
-- entry.  Hot path is lighter than the previous shared-IntMap
-- (memoize) cache because the IOArray write is unsynchronised.
{-# INLINE lmdbL2EdgeLookup #-}
lmdbL2EdgeLookup
    :: MDB_env -> MDB_dbi' -> DupsortCursorCache -> L2Cache
    -> EdgeLookup
lmdbL2EdgeLookup env dbi cursorCache l2 key = unsafePerformIO $ do
    h <- l2Lookup l2 key
    case h of
      Just vs -> return vs
      Nothing -> do
        !vs <- lmdbRawEdgeLookupIO env dbi cursorCache key
        l2Insert l2 key vs
        return vs




-- | Convenience: open LMDB and return an EdgeLookup with a long-lived
-- read transaction. The transaction (and thus the mmap snapshot) stays
-- open for the process lifetime. This is correct because the fact
-- database is read-only during queries.
openLmdbEdgeLookup :: FilePath -> String -> IO EdgeLookup

openLmdbEdgeLookup dbPath dbName = do
    -- Dupsort layout: data lives in a named subdb.  Caller passes the
    -- name (e.g. "category_parent" for Phase 1 ingester output).
    (env, dbi) <- openLmdbRawReadonlyStore dbPath (Just dbName)



    -- Per-thread (txn, cursor) cache; entries are populated lazily
    -- on the first lookup from each parMap worker thread.
    cursorCache <- newDupsortCursorCache





    -- L2-only: shared lock-free IOArray cache.  Cross-HEC sharing
    -- without atomicModifyIORef contention.
    l2cap <- defaultL2Capacity
    l2    <- newL2Cache l2cap
    return (lmdbL2EdgeLookup env dbi cursorCache l2)







-- | Scan all rows from the raw Int32 LMDB store.
scanRawIntPairs :: MDB_txn -> MDB_dbi' -> IO [(Int, Int)]
scanRawIntPairs txn dbi = do
    cursor <- mdb_cursor_open' txn dbi
    rows <- alloca $ \kvPtr -> alloca $ \vvPtr -> do
      let loop step acc = do
            found <- mdb_cursor_get' step cursor kvPtr vvPtr
            if not found
              then return (reverse acc)
              else do
                MDB_val keySz keyPtr <- peek kvPtr
                MDB_val valSz valPtr <- peek vvPtr
                if keySz < 4
                  then loop MDB_NEXT acc
                  else do
                    key <- fromIntegral <$> peekElemOff (castPtr keyPtr :: Ptr Int32) 0
                    let count = fromIntegral valSz `div` 4
                    vals <- mapM (\i -> fromIntegral <$> peekElemOff (castPtr valPtr :: Ptr Int32) i) [0..count-1]
                    loop MDB_NEXT (reverse [(key, v) | v <- vals] ++ acc)
      loop MDB_FIRST []
    mdb_cursor_close' cursor
    return rows

-- | Create a FactSource backed by raw LMDB.
-- fsScan walks the packed values via a cursor; fsLookupArg1 uses the
-- zero-copy EdgeLookup.
lmdbFactSource :: FilePath -> String -> IO FactSource

lmdbFactSource dbPath dbName = do
    (env, dbi) <- openLmdbRawReadonlyStore dbPath (Just dbName)


    txn <- mdb_txn_begin env Nothing True  -- long-lived read txn (used by fsScan)

    -- Lookups go through a per-thread cursor cache (independent of
    -- the scan txn above so parMap workers can run in parallel).
    cursorCache <- newDupsortCursorCache





    -- L2-only: shared lock-free IOArray cache.
    l2cap <- defaultL2Capacity
    l2    <- newL2Cache l2cap
    let lookup' = lmdbL2EdgeLookup env dbi cursorCache l2






    return FactSource
      { fsScan       = scanRawIntPairs txn dbi
      , fsLookupArg1 = \key -> return $ map (\v -> (key, v)) (lookup' key)
      , fsClose      = mdb_txn_abort txn >> mdb_env_close env
      }

-- | Read the minimal manifest fields needed by the prototype relation
-- artifact contract.
readLmdbArtifactManifest :: FilePath -> IO (String, Bool)
readLmdbArtifactManifest artifactDir = do
    content <- readFile (artifactDir ++ "/manifest.json")
    let ls = lines content
        dbName = fromMaybe "main" $ firstManifestField "db_name" ls
        dupsort = case firstManifestField "dupsort" ls of
          Just "true" -> True
          _ -> False
    return (dbName, dupsort)

firstManifestField :: String -> [String] -> Maybe String
firstManifestField field ls =
    case [parseManifestScalar (drop (length prefix) trimmed)
         | line <- ls
         , let trimmed = dropWhile (`elem` [' ', '\t']) line
         , let prefix = "\"" ++ field ++ "\":"
         , prefix `isPrefixOf` trimmed
         ] of
      (v:_) -> Just v
      _ -> Nothing

parseManifestScalar :: String -> String
parseManifestScalar raw =
    case dropWhile (`elem` [' ', '\t']) raw of
      ('"':rest) -> takeWhile (/= '"') rest
      rest -> takeWhile (`notElem` [',', '}', ' ']) rest

-- | Open a UTF-8 LMDB relation artifact from its manifest metadata.
openLmdbUtf8StoreFromManifest :: FilePath -> IO (MDB_env, MDB_txn, MDB_dbi', Bool)
openLmdbUtf8StoreFromManifest artifactDir = do
    (dbName, dupsort) <- readLmdbArtifactManifest artifactDir
    env <- mdb_env_create
    mdb_env_set_mapsize env (1024 * 1024 * 1024)
    mdb_env_set_maxdbs env 16
    mdb_env_set_maxreaders env 126
    mdb_env_open env artifactDir [MDB_RDONLY]
    txn <- mdb_txn_begin env Nothing True
    let dbFlags = if dupsort then [MDB_DUPSORT] else []
    dbi <- mdb_dbi_open' txn (Just dbName) dbFlags
    return (env, txn, dbi, dupsort)

lookupUtf8Values :: MDB_txn -> MDB_dbi' -> Bool -> String -> IO [String]
lookupUtf8Values txn dbi dupsort key =
    withCStringLen key $ \(keyPtr, keyLen) -> do
      cursor <- mdb_cursor_open' txn dbi
      values <- alloca $ \kvPtr -> alloca $ \vvPtr -> do
        poke kvPtr (MDB_val (fromIntegral keyLen) (castPtr keyPtr))
        let collect acc = do
              MDB_val valSz valPtr <- peek vvPtr
              value <- peekCStringLen (castPtr valPtr, fromIntegral valSz)
              if dupsort
                then do
                  more <- mdb_cursor_get' MDB_NEXT_DUP cursor kvPtr vvPtr
                  if more then collect (value:acc) else return (reverse (value:acc))
                else return (reverse (value:acc))
        found <- mdb_cursor_get' MDB_SET cursor kvPtr vvPtr
        if found then collect [] else return []
      mdb_cursor_close' cursor
      return values

scanUtf8Pairs :: MDB_txn -> MDB_dbi' -> InternTable -> IO [(Int, Int)]
scanUtf8Pairs txn dbi tbl = do
    cursor <- mdb_cursor_open' txn dbi
    rows <- alloca $ \kvPtr -> alloca $ \vvPtr -> do
      let loop step acc = do
            found <- mdb_cursor_get' step cursor kvPtr vvPtr
            if not found
              then return (reverse acc)
              else do
                MDB_val keySz keyPtr <- peek kvPtr
                MDB_val valSz valPtr <- peek vvPtr
                key <- peekCStringLen (castPtr keyPtr, fromIntegral keySz)
                value <- peekCStringLen (castPtr valPtr, fromIntegral valSz)
                let pair = (internAtomPure tbl key, internAtomPure tbl value)
                loop MDB_NEXT (pair : acc)
      loop MDB_FIRST []
    mdb_cursor_close' cursor
    return rows

-- | Create a FactSource backed by the LMDB relation-artifact prototype.
-- Reads UTF-8 key/value rows using the manifest to find the named DB.
lmdbFactSourceFromManifest :: InternTable -> FilePath -> IO FactSource
lmdbFactSourceFromManifest tbl artifactDir = do
    (env, txn, dbi, dupsort) <- openLmdbUtf8StoreFromManifest artifactDir
    return FactSource
      { fsScan       = scanUtf8Pairs txn dbi tbl
      , fsLookupArg1 = \key -> do
            let atomKey = lookupAtom tbl key
            values <- lookupUtf8Values txn dbi dupsort atomKey
            return $ map (\v -> (key, internAtomPure tbl v)) values
      , fsClose      = mdb_txn_abort txn >> mdb_env_close env
      }



-- | Dupsort layout uses the streaming pipeline's ingester (Rust/Python
-- mysql_stream + ingest_to_lmdb).  No in-process ingest function in the
-- WAM target — the LMDB is built externally before the WAM runs.
ingestTsvToLmdb :: InternTable -> FilePath -> FilePath -> String -> IO ()
ingestTsvToLmdb _ _ _ _ = error
    "ingestTsvToLmdb: not supported for dupsort layout (LMDB built externally)"


-- ===========================================================================
-- Phase 2b.2 LMDB-resident loaders (called from int_atom_seeds(lmdb) mode)
-- ===========================================================================
--
-- These functions populate runtime state directly from the LMDB sub-dbs
-- written by the Phase 1 ingester (PR #1905):
--
--   s2i              : UTF-8 string  -> int32_le      (forward intern map)
--   i2s              : int32_le      -> UTF-8 string  (reverse intern map)
--   article_category : int32_le      -> int32_le      (dupsort)
--   category_parent  : int32_le      -> int32_le      (dupsort)
--
-- ASCII / UTF-8 caveat
-- --------------------
-- These loaders treat byte-for-byte equality as the equality semantics for
-- atoms. For ASCII-only inputs (English Wikipedia category names, which
-- are URL-encoded ASCII) the resulting Haskell `String` is byte-identical
-- to what `decodeUtf8 . readFile` would produce, so atom equality is
-- preserved across TSV and LMDB regimes.
--
-- For inputs containing multi-byte UTF-8 sequences (e.g. simplewiki's
-- "Geografía"), the byte-by-byte conversion produces the Latin-1
-- interpretation, NOT the Unicode codepoint. This will cause atom
-- inequality with the TSV path's `decodeUtf8` reader. If/when we
-- exercise an internationalized fixture, the fix is to add `bytestring`
-- + `text` deps and use `Data.Text.Encoding.decodeUtf8`. Filed as a
-- follow-up; documented here so a future failure mode is recoverable.

-- | Open an LMDB env read-only for the int_atom_seeds(lmdb) loaders.
-- Independent of {{lmdb_setup}}'s env (which is held by the FFI kernel
-- path); opens its own handle so the loaders can run before the FFI
-- setup block. LMDB safely supports multiple MDB_env handles per
-- process; the OS shares the underlying mmap pages.
--
-- maxdbs=4 covers the four named sub-dbs the loaders touch: s2i, i2s,
-- article_category, category_parent. mapsize matches the ingester's
-- 4 GB ceiling (sufficient for enwiki).
openLmdbInternEnvReadonly :: FilePath -> IO MDB_env
openLmdbInternEnvReadonly dbPath = do
    env <- mdb_env_create
    mdb_env_set_mapsize env (4 * 1024 * 1024 * 1024)
    mdb_env_set_maxdbs env 4
    mdb_env_set_maxreaders env 126
    mdb_env_open env dbPath [MDB_RDONLY]
    return env

-- | Decode bytes pointed to by a CChar pointer as a Haskell String.
-- Treats the bytes as UTF-8, so non-ASCII (e.g. accented categorylinks
-- like "Caf\xc3\xa9s") round-trips correctly. Invalid byte sequences
-- get U+FFFD via lenientDecode rather than throwing — the LMDB store
-- is upstream-controlled (Phase 1 ingester), so a decode failure means
-- the ingester wrote bad bytes, not a runtime fault to abort on.
peekStringBytes :: Ptr CChar -> Int -> IO String
peekStringBytes p len
    | len <= 0  = return []
    | otherwise = do
        bs <- BS.packCStringLen (p, len)
        return $! T.unpack (TE.decodeUtf8With TEE.lenientDecode bs)

-- | Iterate every (key, value) pair in a sub-db with a single read txn,
-- applying a decoder to each pair. Returns results in iteration order.
-- The cursor is opened on the supplied txn and discarded when the txn
-- aborts at the end. Caller is responsible for managing the txn lifetime
-- (typically: bracket with mdb_txn_begin/mdb_txn_abort).
iterateAllPairs :: MDB_txn -> MDB_dbi'
                -> (MDB_val -> MDB_val -> IO a)
                -> IO [a]
iterateAllPairs txn dbi decode = do
    cursor <- mdb_cursor_open' txn dbi
    alloca $ \kvPtr -> alloca $ \dvPtr -> do
      poke kvPtr (MDB_val 0 nullPtr)
      poke dvPtr (MDB_val 0 nullPtr)
      found0 <- mdb_cursor_get' MDB_FIRST cursor kvPtr dvPtr
      go cursor kvPtr dvPtr found0 []
  where
    go _ _ _ False acc = return (Prelude.reverse acc)
    go cursor kvPtr dvPtr True acc = do
      kv <- peek kvPtr
      dv <- peek dvPtr
      x <- decode kv dv
      more <- mdb_cursor_get' MDB_NEXT cursor kvPtr dvPtr
      go cursor kvPtr dvPtr more (x : acc)

-- | Read s2i and i2s sub-dbs into the existing InternTable record shape.
-- Skips TSV parsing + intern rebuild on warm runs of the LMDB-resident
-- pipeline. Memory cost: O(unique strings * average length) — same as
-- today, just loaded faster from binary instead of from text TSV.
loadInternTableFromLmdb :: MDB_env -> String -> String -> IO InternTable
loadInternTableFromLmdb env s2iName i2sName = do
    txn <- mdb_txn_begin env Nothing True
    s2iDb <- mdb_dbi_open' txn (Just s2iName) []
    i2sDb <- mdb_dbi_open' txn (Just i2sName) []
    forwardPairs <- iterateAllPairs txn s2iDb decodeStrInt
    reversePairs <- iterateAllPairs txn i2sDb decodeIntStr
    mdb_txn_abort txn
    let !forward = Map.fromList forwardPairs
        !reverse_ = IM.fromList reversePairs
        !sz = if null reversePairs then 0 else 1 + maximum (map fst reversePairs)
    return $! InternTable forward reverse_ sz
  where
    decodeStrInt (MDB_val ksz kp) (MDB_val vsz vp) = do
      keyStr <- peekStringBytes (castPtr kp) (fromIntegral ksz)
      val <- if vsz >= 4
              then fromIntegral <$> peekElemOff (castPtr vp :: Ptr Int32) 0
              else return 0
      return (keyStr, val)
    decodeIntStr (MDB_val ksz kp) (MDB_val vsz vp) = do
      key <- if ksz >= 4
              then fromIntegral <$> peekElemOff (castPtr kp :: Ptr Int32) 0
              else return 0
      valStr <- peekStringBytes (castPtr vp) (fromIntegral vsz)
      return (key, valStr)

-- | Read the article_category dupsort sub-db into a [(article_id,
-- category_id)] list. Used in place of TSV-derived `articleCategories`
-- when int_atom_seeds(lmdb) is active.
loadArticleCategoriesFromLmdb :: MDB_env -> String -> IO [(Int, Int)]
loadArticleCategoriesFromLmdb env dbName = do
    txn <- mdb_txn_begin env Nothing True
    db <- mdb_dbi_open' txn (Just dbName) [MDB_DUPSORT]
    pairs <- iterateAllPairs txn db decodeIntIntPair
    mdb_txn_abort txn
    return pairs
  where
    decodeIntIntPair (MDB_val ksz kp) (MDB_val vsz vp) = do
      k <- if ksz >= 4
            then fromIntegral <$> peekElemOff (castPtr kp :: Ptr Int32) 0
            else return 0
      v <- if vsz >= 4
            then fromIntegral <$> peekElemOff (castPtr vp :: Ptr Int32) 0
            else return 0
      return (k, v)

-- | Read the category_parent dupsort sub-db into an in-memory IntMap
-- (child_id -> [parent_ids]). This builds the same shape as today's
-- `parentsIndexInterned` but populated from LMDB binary instead of
-- parsed TSV. The size threshold guards against materialising an
-- enwiki-scale (~28M edge) graph in memory: callers above the
-- threshold should switch to LMDB-cursor BFS (Phase 2b.3 follow-up)
-- or pass --with-reverse-edges to the ingester.
--
-- Threshold convention: 0 means unlimited (returns whatever fits in
-- RAM); a positive number panics with a clear message above that
-- count of edges loaded so far.
loadForwardEdgesFromLmdb :: MDB_env -> String -> Int -> IO (IM.IntMap [Int])
loadForwardEdgesFromLmdb env dbName threshold = do
    txn <- mdb_txn_begin env Nothing True
    db <- mdb_dbi_open' txn (Just dbName) [MDB_DUPSORT]
    !edges <- iterateAndAccumulate txn db threshold IM.empty 0
    mdb_txn_abort txn
    return edges
  where
    iterateAndAccumulate txn db lim acc count = do
      cursor <- mdb_cursor_open' txn db
      alloca $ \kvPtr -> alloca $ \dvPtr -> do
        poke kvPtr (MDB_val 0 nullPtr)
        poke dvPtr (MDB_val 0 nullPtr)
        found0 <- mdb_cursor_get' MDB_FIRST cursor kvPtr dvPtr
        go cursor kvPtr dvPtr found0 acc count
      where
        go _ _ _ False a _ = return a
        go cursor kvPtr dvPtr True a c = do
          when (lim > 0 && c >= lim) $
            error $ "loadForwardEdgesFromLmdb: edge count exceeded threshold "
                 ++ show lim ++ " (loaded " ++ show c ++ " so far). "
                 ++ "Switch to LMDB-cursor BFS (Phase 2b.3) or use the "
                 ++ "ingester's --with-reverse-edges follow-up."
          MDB_val ksz kp <- peek kvPtr
          MDB_val vsz vp <- peek dvPtr
          k <- if ksz >= 4
                then fromIntegral <$> peekElemOff (castPtr kp :: Ptr Int32) 0
                else return 0
          v <- if vsz >= 4
                then fromIntegral <$> peekElemOff (castPtr vp :: Ptr Int32) 0
                else return 0
          let !a' = IM.insertWith (++) k [v] a
          more <- mdb_cursor_get' MDB_NEXT cursor kvPtr dvPtr
          go cursor kvPtr dvPtr more a' (c + 1)

-- | Phase 2b.3 cursor-based demand-set BFS: walk root → descendants
-- via the `category_child` reverse-edge sub-db using a single LMDB
-- cursor; never materialises the full reverse adjacency in memory.
--
-- Compared to `loadForwardEdgesFromLmdb` + the pure
-- `computeDemandSetHopLimited` traversal, this is:
--
--   * O(|demand_set|) RAM instead of O(|edges|) — only the visited
--     set lives in the heap; edges are streamed through the cursor.
--   * Removes the 5_000_000-edge threshold guard. Scales to enwiki.
--   * Does not benefit from L1/L2 caching (one-shot traversal, every
--     edge is visited at most once). The OS page cache is the only
--     cache layer that matters; warm runs after a cold cache may
--     show 2-5x improvement on the BFS phase.
--
-- The reverse-edge sub-db must exist (use the fixture ingester with
-- `category_child` written). If absent, returns just {rootId}.

computeDemandSetCursorBFS :: MDB_env -> String -> Int -> Maybe Int -> IO IS.IntSet
computeDemandSetCursorBFS env childDbName rootId maxHops = do
    numCaps <- getNumCapabilities
    if numCaps <= 1
      then sequentialBFS
      else parallelBFS numCaps
  where
    -- -N1 fast path: open dbi + cursor in one txn, walk inline, abort
    -- the txn at the end. No dbi commit, no cache, no bound-thread
    -- fork/join, no cross-thread coordination. Matches the pre-parallel
    -- (PR #1956) sequential implementation's overhead profile exactly.
    sequentialBFS = do
        txn <- mdb_txn_begin env Nothing True
        dbi <- mdb_dbi_open' txn (Just childDbName) [MDB_DUPSORT]
        cursor <- mdb_cursor_open' txn dbi
        let go !depth !frontier !visited
              | IS.null frontier = return visited
              | exceededHopLimit depth = return visited
              | otherwise = do
                  newNodes <- foldM (stepNode cursor visited) IS.empty
                                    (IS.toList frontier)
                  if IS.null newNodes
                    then return visited
                    else go (depth + 1) newNodes (IS.union visited newNodes)
        result <- go 0 (IS.singleton rootId) (IS.singleton rootId)
        mdb_txn_abort txn
        return result
    -- Parallel path: open dbi in its own txn and commit so the dbi
    -- handle persists across per-worker read txns. Each worker is a
    -- bound thread with its own (txn, cursor) pair from a shared
    -- DupsortCursorCache. At -N>=2 the parMap-style parallelism more
    -- than pays for the cache + fork/join overhead.
    parallelBFS nWorkers = do
        dbiTxn <- mdb_txn_begin env Nothing True
        dbi <- mdb_dbi_open' dbiTxn (Just childDbName) [MDB_DUPSORT]
        mdb_txn_commit dbiTxn
        cache <- newDupsortCursorCache
        let go !depth !frontier !visited
              | IS.null frontier = return visited
              | exceededHopLimit depth = return visited
              | otherwise = do
                  let frontierList = IS.toList frontier
                      chunks = chunkInto nWorkers frontierList
                  chunkResults <- mapConcurrently
                    (processChunk env dbi cache visited)
                    chunks
                  let !newNodes = IS.unions chunkResults
                  if IS.null newNodes
                    then return visited
                    else go (depth + 1) newNodes (IS.union visited newNodes)
        go 0 (IS.singleton rootId) (IS.singleton rootId)
    exceededHopLimit !d = case maxHops of
      Nothing -> False
      Just n  -> d >= n
    -- Split a list into n roughly-equal chunks. Empty chunks are
    -- dropped so workers aren't spawned with no work.
    chunkInto !n xs =
      let len = length xs
          sz  = max 1 ((len + n - 1) `div` n)
          go' [] = []
          go' ys = let (h, t) = splitAt sz ys in h : go' t
      in filter (not . null) (go' xs)
    -- Process one chunk of frontier nodes with a per-thread LMDB
    -- cursor. runInBoundThread pins the worker to one OS thread so
    -- the cursor (and its parent read-txn) don't migrate mid-traversal,
    -- which LMDB forbids.
    processChunk !env' !dbi' cache' !visited' !chunk =
      runInBoundThread $ do
        cursor <- getOrOpenDupsortCursor env' dbi' cache'
        foldM (stepNode cursor visited') IS.empty chunk
    stepNode cursor visited !acc node =
      allocaBytes 4 $ \kp -> do
        poke (castPtr kp :: Ptr Int32) (fromIntegral node :: Int32)
        alloca $ \kvPtr -> alloca $ \dvPtr -> do
          poke kvPtr (MDB_val 4 kp)
          found <- mdb_cursor_get' MDB_SET cursor kvPtr dvPtr
          if not found
            then return acc
            else collectDups cursor kvPtr dvPtr visited acc
    collectDups cursor kvPtr dvPtr visited !acc = do
      MDB_val sz dataPtr <- peek dvPtr
      v <- if sz >= 4
            then fromIntegral <$> peekElemOff (castPtr dataPtr :: Ptr Int32) 0
            else return 0
      let !acc' = if IS.member v visited then acc else IS.insert v acc
      more <- mdb_cursor_get' MDB_NEXT_DUP cursor kvPtr dvPtr
      if more
        then collectDups cursor kvPtr dvPtr visited acc'
        else return acc'





-- | Dereference an Unbound variable through the binding table.
{-# INLINE derefVar #-}
derefVar :: IM.IntMap Value -> Value -> Value
derefVar bindings (Unbound vid) =
  case IM.lookup vid bindings of
    Just val -> derefVar bindings val
    Nothing  -> Unbound vid
derefVar _ v = v

-- | Evaluate arithmetic expression. InternTable needed for reverse
-- lookup of atom strings (numeric atom parsing, operator names).
evalArith :: InternTable -> IM.IntMap Value -> Value -> Maybe Double
evalArith _ _ (Integer n) = Just (fromIntegral n)
evalArith _ _ (Float f) = Just f
evalArith tbl _ (Atom aid) = case reads (lookupAtom tbl aid) of
  [(n, "")] -> Just n
  _ -> Nothing
evalArith tbl bindings (Str opId [a]) = do
  va <- evalArith tbl bindings (derefVar bindings a)
  let bareOp = takeWhile (/= '/') (lookupAtom tbl opId)
  case bareOp of
    "-" -> Just (negate va)
    "abs" -> Just (abs va)
    _ -> Nothing
evalArith tbl bindings (Str opId [a, b]) = do
  va <- evalArith tbl bindings (derefVar bindings a)
  vb <- evalArith tbl bindings (derefVar bindings b)
  let bareOp = takeWhile (/= '/') (lookupAtom tbl opId)
  case bareOp of
    "+" -> Just (va + vb)
    "-" -> Just (va - vb)
    "*" -> Just (va * vb)
    "**" -> Just (va ** vb)
    "^" -> Just (va ** vb)
    "/" -> if vb /= 0 then Just (va / vb) else Nothing
    "//" -> if vb /= 0 then Just (fromIntegral (truncate va `div` truncate vb :: Int)) else Nothing
    "mod" -> if vb /= 0 then Just (fromIntegral (truncate va `mod` truncate vb :: Int)) else Nothing
    _ -> Nothing
evalArith _ _ _ = Nothing

-- | Get register value. Y-registers (id >= 200) come from the env frame.
{-# INLINE getReg #-}
getReg :: Int -> WamState -> Maybe Value
getReg rid s
  | rid >= 200 = findYReg rid (wsStack s)
  | otherwise = derefVar (wsBindings s) <$> IM.lookup rid (wsRegs s)
  where
    findYReg _ [] = Nothing
    findYReg r (EnvFrame _ yregs : _) = derefVar (wsBindings s) <$> IM.lookup r yregs
    findYReg r (_ : rest) = findYReg r rest

-- | Set register value. Y-registers go to the topmost env frame.
{-# INLINE putReg #-}
putReg :: Int -> Value -> WamState -> WamState
putReg rid val s
  | rid >= 200 = s { wsStack = updateTopEnv rid val (wsStack s) }
  | otherwise = s { wsRegs = IM.insert rid val (wsRegs s) }
  where
    updateTopEnv _ _ [] = []
    updateTopEnv r v (EnvFrame cp yregs : rest) =
      EnvFrame cp (IM.insert r v yregs) : rest
    updateTopEnv r v (x : rest) = x : updateTopEnv r v rest

-- | Dereference a heap reference.
derefHeap :: [Value] -> Value -> Maybe Value
derefHeap heap (Ref addr)
  | addr >= 0 && addr < length heap = Just (heap !! addr)
  | otherwise = Nothing
derefHeap _ (Str fn args) = Just (Str fn args)
derefHeap _ (Unbound n) = Just (Unbound n)
derefHeap _ v = Just v

-- | Add a value to the current structure/list builder.
addToBuilder :: Value -> WamState -> Maybe WamState
addToBuilder val s = case wsBuilder s of
  BuildStruct fn ai arity args ->
    -- Cons to front (O(1)) and reverse only on finalize. Track count via list length
    -- but only when finalizing — args grows from 0 to arity, max arity is small.
    let args' = val : args
    in if length args' == arity
       then Just (s { wsPC = wsPC s + 1
                    , wsRegs = IM.insert ai (Str fn (reverse args')) (wsRegs s)
                    , wsBuilder = NoBuilder
                    })
       else Just (s { wsPC = wsPC s + 1
                    , wsBuilder = BuildStruct fn ai arity args'
                    })
  BuildList ai args ->
    -- BuildList always has exactly 2 args [head, tail]
    let args' = val : args
    in if length args' == 2
       then let [tl, hd] = args'   -- reversed because we cons-built
                list = case tl of
                  VList items -> VList (hd : items)
                  Atom aid | aid == atomNil -> VList [hd]
                  _           -> VList [hd, tl]
            in Just (s { wsPC = wsPC s + 1
                       , wsRegs = IM.insert ai list (wsRegs s)
                       , wsBuilder = NoBuilder
                       })
       else Just (s { wsPC = wsPC s + 1
                    , wsBuilder = BuildList ai args'
                    })
  NoBuilder ->
    -- No builder active, just push to heap (fallback)
    Just (s { wsPC = wsPC s + 1, wsHeap = wsHeap s ++ [val], wsHeapLen = wsHeapLen s + 1 })

-- | Lookup a label in the label map (now in WamContext).
lookupLabel :: String -> WamContext -> Int
lookupLabel label ctx = fromMaybe 0 $ Map.lookup label (wcLabels ctx)

-- | Fetch instruction at PC (1-indexed). Bounds-checked, returns Maybe.
fetchInstr :: Int -> Array Int Instruction -> Maybe Instruction
fetchInstr pc code
  | let (lo, hi) = bounds code in pc < lo || pc > hi = Nothing
  | otherwise = Just (code ! pc)

-- | Unsafe fetch — no bounds check, no Maybe wrapping. Use only when the
-- caller can prove PC is in bounds (the run loop handles PC=0 as halt
-- separately, and a well-formed WAM program never jumps out of bounds).
{-# INLINE unsafeFetchInstr #-}
unsafeFetchInstr :: Int -> Array Int Instruction -> Instruction
unsafeFetchInstr pc code = code ! pc

-- | Resolve Call instructions at project load time:
--   - Foreign predicates (detected kernels) → CallForeign (direct FFI, Nothing = fail)
--   - Known labels → CallResolved (direct PC jump, no dispatch)
--   - Everything else → left as Call (runtime dispatch chain)
resolveCallInstrs :: Map.HashMap String Int -> [String] -> [Instruction] -> [Instruction]
resolveCallInstrs labels foreignPreds = map resolve
  where
    resolve (Call pred arity)
      | pred `elem` foreignPreds = CallForeign pred arity
      | otherwise = case Map.lookup pred labels of
          Just pc -> CallResolved pc arity
          Nothing -> Call pred arity
    resolve (Execute pred) = case Map.lookup pred labels of
      Just pc -> ExecutePc pc
      Nothing -> Execute pred
    resolve (Jump label) = case Map.lookup label labels of
      Just pc -> JumpPc pc
      Nothing -> Jump label
    resolve (TryMeElse label) = case Map.lookup label labels of
      Just pc -> TryMeElsePc pc
      Nothing -> TryMeElse label
    resolve (RetryMeElse label) = case Map.lookup label labels of
      Just pc -> RetryMeElsePc pc
      Nothing -> RetryMeElse label
    -- Phase 4.1 Par* variants resolve the same way as their sequential
    -- counterparts. The instruction carries forkability intent; label
    -- resolution is orthogonal.
    resolve (ParTryMeElse label) = case Map.lookup label labels of
      Just pc -> ParTryMeElsePc pc
      Nothing -> ParTryMeElse label
    resolve (ParRetryMeElse label) = case Map.lookup label labels of
      Just pc -> ParRetryMeElsePc pc
      Nothing -> ParRetryMeElse label
    resolve (SwitchOnConstant table) =
      let extractId (Atom aid) = aid
          extractId (Integer n) = n  -- integers use their value directly as key
          extractId _ = (-1)
      in SwitchOnConstantPc (IM.fromList [(extractId v, pc) | (v, label) <- Map.toList table,
                                                               Just pc <- [Map.lookup label labels]])
    resolve i = i
