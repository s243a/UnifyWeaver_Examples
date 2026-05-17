module WamTypes where

import qualified Data.HashMap.Strict as Map
import Data.Hashable (Hashable(..))
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.Array (Array, listArray, (!), bounds)
-- Phase 4.2: NFData is needed for parMap rdeepseq to fully evaluate
-- each forked branch's contribution before the merge step.
import Control.DeepSeq (NFData(..))

-- | Core value type. Atoms and Str functor names are interned as Ints
-- for O(1) equality. Use lookupAtom/internAtom via the InternTable in
-- WamContext for String conversion.
data Value = Atom !Int          -- interned atom ID
           | Integer !Int
           | Float !Double
           | VList [Value]
           | VSet !IS.IntSet    -- visited-set: IntSet of interned atom IDs
           | Str !Int [Value]   -- interned functor name, args
           | Unbound !Int       -- variable ID (interned via wsVarCounter)
           | Ref Int
           deriving (Eq, Ord, Show)

instance Hashable Value where
  hashWithSalt salt (Atom n) = hashWithSalt salt (0 :: Int, n)
  hashWithSalt salt (Integer n) = hashWithSalt salt (1 :: Int, n)
  hashWithSalt salt (Float f) = hashWithSalt salt (2 :: Int, f)
  hashWithSalt salt (VList xs) = hashWithSalt salt (3 :: Int, xs)
  hashWithSalt salt (VSet s) = hashWithSalt salt (4 :: Int, IS.toList s)
  hashWithSalt salt (Str n args) = hashWithSalt salt (5 :: Int, n, args)
  hashWithSalt salt (Unbound n) = hashWithSalt salt (6 :: Int, n)
  hashWithSalt salt (Ref n) = hashWithSalt salt (7 :: Int, n)

-- | Atom intern table. Built at compile time, extended at load time
-- with runtime atoms (e.g., from TSV fact data). Read-only after
-- construction — safe for parallelism (shared via WamContext).
data InternTable = InternTable
  { itForward :: !(Map.HashMap String Int)   -- String -> atom ID
  , itReverse :: !(IM.IntMap String)     -- atom ID -> String
  , itSize    :: !Int                    -- next available ID
  } deriving (Show)

emptyInternTable :: InternTable
emptyInternTable = InternTable Map.empty IM.empty 0

internAtom :: InternTable -> String -> (Int, InternTable)
internAtom tbl s = case Map.lookup s (itForward tbl) of
  Just aid -> (aid, tbl)
  Nothing  -> let aid = itSize tbl
              in (aid, tbl { itForward = Map.insert s aid (itForward tbl)
                           , itReverse = IM.insert aid s (itReverse tbl)
                           , itSize = aid + 1 })

internAtomPure :: InternTable -> String -> Int
internAtomPure tbl s = case Map.lookup s (itForward tbl) of
  Just aid -> aid
  Nothing  -> (-1)  -- should not happen for well-formed programs

lookupAtom :: InternTable -> Int -> String
lookupAtom tbl aid = IM.findWithDefault ("?atom_" ++ show aid) aid (itReverse tbl)

-- | Well-known atom IDs. Reserved during compile-time atom collection.
-- These MUST match the IDs assigned by the Prolog codegen.
atomTrue, atomFail, atomNil, atomDot, atomEmpty :: Int
atomTrue  = 0
atomFail  = 1
atomNil   = 2   -- "[]"
atomDot   = 3   -- "."
atomEmpty = 4   -- ""

data EnvFrame = EnvFrame {-# UNPACK #-} !Int !(IM.IntMap Value)
              deriving (Show)

data TrailEntry = TrailEntry {-# UNPACK #-} !Int !(Maybe Value)
                deriving (Show)

data ChoicePoint = ChoicePoint
  { cpNextPC   :: {-# UNPACK #-} !Int
  , cpRegs     :: !(IM.IntMap Value)
  , cpStack    :: ![EnvFrame]
  , cpCP       :: {-# UNPACK #-} !Int
  , cpTrailLen :: {-# UNPACK #-} !Int
  , cpHeapLen  :: {-# UNPACK #-} !Int
  , cpBindings :: !(IM.IntMap Value)
  , cpCutBar   :: {-# UNPACK #-} !Int
  , cpAggFrame :: !(Maybe AggFrame)
  , cpBuiltin  :: !(Maybe BuiltinState)
  } deriving (Show)

-- | Builtin state for choice points that need custom retry logic.
data BuiltinState
  = FactRetry !Int ![Int] !Int     -- variable ID, remaining interned atom IDs, returnPC
  | HopsRetry !Int ![Int] !Int     -- variable ID, remaining Hops values, returnPC
    -- Multi-output FFI kernel retry. Each remaining tuple is already
    -- wrapped as a list of Values (pre-interned / wrapped at call site).
    -- outRegs and outVars are parallel lists (same length as each tuple).
    -- outVars contains -1 for originally-bound outputs (no binding update).
  | FFIStreamRetry ![Int] ![Int] ![[Value]] !Int  -- outRegs, outVars, remaining tuples, returnPC
    -- Phase F2: FactStream for inline_data fact predicates.
    -- Iterates interned (arg1, arg2) tuples via backtracking.
    -- var1/var2 are variable IDs for binding (-1 = already bound, skip).
  | FactStream !Int !Int ![(Int, Int)] !Int  -- var1, var2, remaining rows, returnPC
  deriving (Show)

-- | Aggregate frame for begin_aggregate/end_aggregate.
data AggFrame = AggFrame
  { afType      :: !String         -- "sum", "count", "collect", etc.
  , afValueReg  :: !Int            -- register ID holding value per solution
  , afResultReg :: !Int            -- register ID for final result
  , afReturnPC  :: !Int            -- PC after end_aggregate
  , afMergeStrategy :: !MergeStrategy  -- Phase 4.2: derived from afType;
                                       -- carried on the frame so inner
                                       -- ParTryMeElse choice points can
                                       -- decide whether to fork without
                                       -- re-parsing the type string.
  } deriving (Show)

-- | Phase 4.2: how to combine per-branch aggregate values when forking.
-- Commutative-and-associative strategies (sum/count) go in Phase 4.2;
-- findall/bag/set arrive in Phase 4.3; race/negation in 4.4. Unknown
-- aggregate types yield MergeSequential, which disables forking.
data MergeStrategy
  = MergeSumInt
  | MergeSumDouble
  | MergeCount
  | MergeFindall       -- Phase 4.3
  | MergeBag           -- Phase 4.3
  | MergeSet           -- Phase 4.3
  | MergeRace          -- Phase 4.4
  | MergeNegation      -- Phase 4.4
  | MergeSequential    -- Default fallback; fork disabled
  deriving (Show, Eq)

-- | Phase 4.2: fork context threaded from BeginAggregate through to the
-- inner ParTryMeElse choice point that does the actual fork.
data ForkContext = ForkContext
  { fcMergeStrategy :: !MergeStrategy
  , fcWorkEstimate  :: !(Maybe Double)  -- microseconds, Phase 4.5
  } deriving (Show)

-- | Parse a MergeStrategy from the aggregate type string (as stored in
-- AggFrame.afType). Returns MergeSequential for anything we don't
-- recognize, which causes the ParTryMeElse fork to fall back to the
-- sequential TryMeElse handler.
inferMergeStrategy :: String -> MergeStrategy
inferMergeStrategy "sum"   = MergeSumDouble
inferMergeStrategy "count" = MergeCount
inferMergeStrategy "bag"   = MergeBag
inferMergeStrategy "set"   = MergeSet
inferMergeStrategy "findall" = MergeFindall
inferMergeStrategy "collect" = MergeFindall
inferMergeStrategy _       = MergeSequential

-- | Phase 4.2: NFData instances so parMap rdeepseq can spark forked
-- branches. We need this only for the types that end up in a parMap
-- result — for us that's `[Value]` (each branch's contributed
-- aggregate values). Everything is first-order / strict already;
-- these definitions just walk the structure to force evaluation.
instance NFData Value where
  rnf (Atom n)         = rnf n
  rnf (Integer n)      = rnf n
  rnf (Float f)        = rnf f
  rnf (VList xs)       = rnf xs
  rnf (VSet s)         = rnf (IS.size s)  -- IntSet has no NFData; force size
  rnf (Str n args)     = rnf n `seq` rnf args
  rnf (Unbound n)      = rnf n
  rnf (Ref n)          = rnf n

-- | Builder for PutStructure/PutList + SetValue/SetConstant sequences.
data Builder = BuildStruct !Int !Int !Int ![Value]  -- interned functor ID, target reg ID, arity, collected args
             | BuildList !Int ![Value]               -- target reg ID, collected [head, tail]
             | NoBuilder
             deriving (Show)

-- | Phase F4: FactSource abstraction for external fact data.
-- Mirrors the Elixir FactSource behaviour (fsScan, fsLookupArg1, fsClose).
-- Concrete implementations: TsvFactSource (lazy IO), IntMapFactSource
-- (wraps existing strict IntMap). MmapFactSource deferred to Phase F6.
data FactSource = FactSource
  { fsScan       :: IO [(Int, Int)]         -- full scan (lazy)
  , fsLookupArg1 :: Int -> IO [(Int, Int)]  -- indexed by first arg
  , fsClose      :: IO ()                   -- release resources
  }

-- | Read-only context. Threaded through the run loop / step function as
-- a separate argument so it doesn't pay the per-step record-update cost
-- on the mutable WamState. Built once at startup, never modified.
data WamContext = WamContext
  { wcCode          :: !(Array Int Instruction)
  , wcLabels        :: !(Map.HashMap String Int)
  , wcForeignFacts  :: !(Map.HashMap String (Map.HashMap String [String]))
  , wcForeignConfig :: !(Map.HashMap String Int)
  , wcLoweredPredicates :: !(Map.HashMap String (WamContext -> WamState -> Maybe WamState))
  -- | System-wide atom intern table. Built at compile time, extended
  -- at load time with runtime atoms. Used for Value → String display,
  -- evalArith reverse lookup, and FFI boundary interning.
  , wcInternTable   :: !InternTable
  -- | Fact indexes keyed by interned Int atoms. Used exclusively by the
  -- FFI kernel path. Populated per-kernel from edge_pred config.
  , wcFfiFacts      :: !(Map.HashMap String (IM.IntMap [Int]))
  -- | Weighted fact indexes for kernels that need (target, weight)
  -- pairs per edge (e.g., weighted_shortest_path3 / Dijkstra). Used
  -- exclusively by the FFI kernel path. Populated from 3-column fact
  -- sources — not wired into the default Main.hs template yet, so
  -- standalone benchmarks build this directly.
  , wcFfiWeightedFacts :: !(Map.HashMap String (IM.IntMap [(Int, Double)]))
  -- | Phase F2: inline fact data for FactStream predicates. Keyed by
  -- predicate name (e.g., "category_parent"). Each entry is a list of
  -- interned (arg1, arg2) tuples. Populated by Phase F3 code generation;
  -- empty until then.
  , wcInlineFacts :: !(Map.HashMap String [(Int, Int)])
  -- | Phase F4: external fact sources keyed by predicate name.
  -- Each entry is a FactSource adaptor (TsvFactSource, IntMapFactSource,
  -- etc.) that provides scan and indexed lookup. Populated at startup
  -- from fact_layout declarations.
  -- Phase F5 strictness: the strict (!) annotation forces the Map spine
  -- at construction. FactSource records are WHNF (IO actions are values).
  -- The !ctx bang pattern in Main.hs ensures the entire WamContext is
  -- evaluated before parMap. Within each spark, streamFacts uses
  -- unsafePerformIO per-call — no cross-spark lazy IO sharing.
  , wcFactSources :: !(Map.HashMap String FactSource)
  -- | Phase B1: LMDB-backed edge lookups for FFI kernels.
  -- When populated, kernels use these instead of wcFfiFacts IntMaps.
  -- Each entry is an EdgeLookup function (Int -> [Int]) that may be
  -- backed by an IntMap (intMapEdgeLookup) or LMDB (lmdbEdgeLookup).
  , wcEdgeLookups :: !(Map.HashMap String EdgeLookup)
  }
-- Note: no `deriving (Show)` because wcLoweredPredicates,
-- wcFactSources, and wcEdgeLookups are function-valued.
-- and functions have no Show instance. Add a manual instance if needed.

-- | Mutable state. Updated on every WAM step. Held separate from WamContext
-- so each step transition only allocates a record with the fields that
-- actually change.
data WamState = WamState
  { wsPC       :: {-# UNPACK #-} !Int
  , wsRegs     :: !(IM.IntMap Value)
  , wsStack    :: ![EnvFrame]
  , wsHeap     :: ![Value]
  , wsHeapLen  :: {-# UNPACK #-} !Int
  , wsTrail    :: ![TrailEntry]
  , wsTrailLen :: {-# UNPACK #-} !Int
  , wsCP       :: {-# UNPACK #-} !Int
  , wsCPs      :: ![ChoicePoint]
  , wsCPsLen   :: {-# UNPACK #-} !Int
  , wsBindings :: !(IM.IntMap Value)
  , wsCutBar   :: {-# UNPACK #-} !Int
  , wsBuilder  :: !Builder
  , wsVarCounter :: {-# UNPACK #-} !Int
  , wsAggAccum :: ![Value]
  } deriving (Show)

-- | Instruction type for the WAM.
-- | Register IDs are pre-interned at compile time as Ints to avoid string
-- hashing on register access. Encoding:
--   A1-A99: 1-99
--   X1-X99: 101-199
--   Y1-Y99: 201-299
type RegId = Int

-- | Phase B1: abstract edge lookup function. Kernels use this instead
-- of IM.IntMap [Int] directly, allowing the backing store to be either
-- an in-memory IntMap or an LMDB database.
type EdgeLookup = Int -> [Int]

data Instruction
  = GetConstant Value !RegId
  | GetVariable !RegId !RegId
  | GetValue !RegId !RegId
  | PutConstant Value !RegId
  | PutVariable !RegId !RegId
  | PutValue !RegId !RegId
  | PutStructure !Int !RegId !Int     -- interned functor ID, target reg, arity
  | PutStructureDyn !RegId !RegId !RegId  -- nameReg, arityReg, targetReg (runtime-parsed)
  | PutList !RegId
  | SetValue !RegId
  | SetVariable !RegId                  -- builder slot = fresh unbound; also write to register
  | SetConstant Value
  | Allocate
  | Deallocate
  | Call String !Int                  -- pre-resolution form (string-keyed)
  | CallResolved !Int !Int            -- post-resolution: target PC + arity
  | CallForeign String !Int           -- compile-time resolved foreign pred (Nothing = fail)
  | Execute String
  | ExecutePc !Int                      -- post-resolution: direct PC jump (tail call)
  | Jump String                         -- unconditional jump to label
  | JumpPc !Int                         -- post-resolution: direct PC jump
  | CutIte                              -- soft cut: pop one CP (if-then-else)
  | Proceed
  | TryMeElsePc !Int                   -- post-resolution: direct PC for else branch
  | RetryMeElsePc !Int                 -- post-resolution: direct PC for next branch
  | SwitchOnConstantPc !(IM.IntMap Int)      -- post-resolution: interned atom ID -> PC
  | BuiltinCall String !Int
  | Arg !Int !RegId !RegId            -- specialized arg/3: literal N, term reg, output reg
  | NotMemberList !RegId !RegId       -- specialized \+ member(X, L): X reg, L reg
  | NotMemberConstAtoms !RegId ![Int] -- \+ member(X, [a,b,c,...]): X reg, baked-in interned atom IDs
  | BuildEmptySet !RegId              -- write VSet IS.empty into the named register
  | SetInsert !RegId !RegId !RegId    -- elemReg, inSetReg, outSetReg
  | NotMemberSet !RegId !RegId        -- elemReg, setReg: O(log N) member check
  | TryMeElse String
  | RetryMeElse String
  | TrustMe
  -- Phase 4.1: parallel-forkable variants. Emitted by the compiler
  -- when the predicate has a pure purity certificate. At Phase 4.1
  -- they dispatch to the same sequential handlers as the non-Par
  -- variants — the instructions carry intent, not behavior. Phase 4.2
  -- will add the runtime fork. See
  -- docs/design/WAM_HASKELL_INTRA_QUERY_SPEC.md §2.
  | ParTryMeElse String
  | ParRetryMeElse String
  | ParTrustMe
  | ParTryMeElsePc !Int
  | ParRetryMeElsePc !Int
  | SwitchOnConstant (Map.HashMap Value String)   -- pre-built Map for O(log n) dispatch
  | BeginAggregate String !RegId !RegId   -- type, valueReg, resultReg
  | EndAggregate !RegId                   -- valueReg
  | CallFactStream String !Int            -- Phase F2: predicate name, arity
  deriving (Show, Eq)

-- | Build the read-only context from compiled code and labels. Called
-- once at project startup. The context is then threaded into runLoop
-- and step as a separate argument.
mkContext :: [Instruction] -> Map.HashMap String Int -> WamContext
mkContext codeList labels =
  let n = length codeList
      code = listArray (1, n) codeList
  in WamContext
    { wcCode          = code
    , wcLabels        = labels
    , wcForeignFacts  = Map.empty
    , wcForeignConfig = Map.empty
    , wcLoweredPredicates = Map.empty
    , wcInternTable   = emptyInternTable
    , wcFfiFacts      = Map.empty
    , wcFfiWeightedFacts = Map.empty
    , wcInlineFacts   = Map.empty
    , wcFactSources   = Map.empty
    , wcEdgeLookups   = Map.empty
    }

-- | Create initial empty mutable state. The cold fields (code, labels,
-- foreign facts/config) live in WamContext now.
emptyState :: WamState
emptyState = WamState
  { wsPC       = 1
  , wsRegs     = IM.empty
  , wsStack    = []
  , wsHeap     = []
  , wsHeapLen  = 0
  , wsTrail    = []
  , wsTrailLen = 0
  , wsCP       = 0
  , wsCPs      = []
  , wsCPsLen   = 0
  , wsBindings = IM.empty
  , wsCutBar   = 0
  , wsBuilder  = NoBuilder
  , wsVarCounter = 0
  , wsAggAccum = []
  }
