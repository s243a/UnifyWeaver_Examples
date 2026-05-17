{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.HashMap.Strict as Map
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (group, sort, isPrefixOf, intercalate, foldl')
import qualified Numeric
import Data.Maybe (fromMaybe, mapMaybe)
import System.Environment (getArgs, lookupEnv)
import System.IO (hPutStrLn, stderr, hFlush, stdout)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.Parallel.Strategies (parMap, rdeepseq)
import Control.DeepSeq (NFData(..), deepseq)
import WamTypes
import WamRuntime
import Predicates
import qualified Lowered
import System.Directory (doesDirectoryExist, createDirectory)
import Database.LMDB.Raw (MDB_env)

-- | Load a TSV file, skip header, return pairs.
loadTsvPairs :: FilePath -> IO [(String, String)]
loadTsvPairs path = do
    content <- readFile path
    let ls = drop 1 (lines content)  -- skip header
    return [(a, b) | l <- ls, let ws = splitOn '\t' l, length ws >= 2, let [a, b] = take 2 ws]

-- | Load single-column TSV.
-- | Load a column of newline-separated integer IDs (one per line).
--   Used by the int_atom_seeds path for seed and root files; ignores
--   blank lines and lines that don't parse as Int.
loadIntColumn :: FilePath -> IO [Int]
loadIntColumn path = do
    content <- readFile path
    return [ n
           | l <- lines content
           , let s = dropWhile (== ' ') l
           , not (null s)
           , (n, "") : _ <- [reads s :: [(Int, String)]]
           ]

loadSingleColumn :: FilePath -> IO [String]
loadSingleColumn path = do
    content <- readFile path
    return [l | l <- drop 1 (lines content), not (null l)]

-- | Simple tab split.
splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn d (c:cs)
  | c == d    = "" : splitOn d cs
  | otherwise = let (w:ws) = splitOn d cs in (c:w) : ws

-- | Apply the same seed-subset controls as the Rust WAM benchmark driver.
applySeedControls :: [String] -> IO ([String], Maybe String, Maybe String)
applySeedControls seedCats0 = do
    seedFilter <- lookupEnv "WAM_SEED_FILTER"
    seedLimit <- lookupEnv "WAM_SEED_LIMIT"
    let filtered = case seedFilter of
          Nothing -> seedCats0
          Just raw ->
            let wanted = filter (not . null) $ map trim $ splitOn '|' raw
            in if null wanted then seedCats0 else filter (`elem` wanted) seedCats0
    limited <- case seedLimit of
      Nothing -> return filtered
      Just raw -> case reads raw :: [(Int, String)] of
        [(limit, "")] | limit > 0 -> return (take limit filtered)
        [(limit, "")] | limit <= 0 -> return filtered
        _ -> do
          hPutStrLn stderr $ "[WAM-Haskell] WARNING: WAM_SEED_LIMIT=" ++ raw ++ " is not a valid Int, ignoring"
          return filtered
    return (limited, seedLimit, seedFilter)
  where
    trim = f . f
      where f = reverse . dropWhile (`elem` [' ', '\t', '\r', '\n'])

-- | Build SwitchOnConstant dispatch table from grouped facts.
buildFactIndex :: [(String, String)] -> Map.HashMap String [(String, String)]
buildFactIndex pairs = foldl (\m (a, b) -> Map.insertWith (++) a [(a, b)] m) Map.empty pairs

-- | Build WAM instructions for indexed fact predicate.
-- Returns (instructions, labels) to append to the code vector.
-- The iAtom function interns a String to an atom ID at load time.
buildFact2Code :: (String -> Int) -> String -> [(String, String)] -> Int -> ([Instruction], [(String, Int)])
buildFact2Code iAtom predName pairs startPC =
    let groups = Map.toList (buildFactIndex pairs)
        -- SwitchOnConstant dispatch table
        (dispatchList, groupCode, groupLabels, _) = foldl buildGroup ([], [], [], startPC + 1) groups
        switchInstr = SwitchOnConstant (Map.fromList dispatchList)
    in (switchInstr : groupCode, (predName ++ "/2", startPC) : groupLabels)
  where
    buildGroup (disp, code, labels, pc) (key, facts) =
      let groupLabel = predName ++ "_g_" ++ key
          (fcode, flabels, nextPC) = buildFactGroup predName key facts pc
      in (disp ++ [(Atom (iAtom key), groupLabel)], code ++ fcode, labels ++ [(groupLabel, pc)] ++ flabels, nextPC)

    buildFactGroup _ _ [] pc = ([], [], pc)
    buildFactGroup pn key facts pc =
      let n = length facts
          buildFact i (a, b) curPC =
            let choiceInstr = if n == 1 then [] else
                  if i == 0 then [TryMeElse (pn ++ "_g_" ++ key ++ "_" ++ show (i+1))]
                  else if i == n - 1 then [TrustMe]
                  else [RetryMeElse (pn ++ "_g_" ++ key ++ "_" ++ show (i+1))]
                factInstrs = [GetConstant (Atom (iAtom a)) 1, GetConstant (Atom (iAtom b)) 2, Proceed]
                label = (pn ++ "_g_" ++ key ++ "_" ++ show i, curPC)
            in (choiceInstr ++ factInstrs, [label], curPC + length choiceInstr + length factInstrs)
          (allCode, allLabels, finalPC) = foldl (\(c, l, p) (i, f) ->
              let (fc, fl, np) = buildFact i f p in (c ++ fc, l ++ fl, np))
            ([], [], pc) (zip [0..] facts)
      in (allCode, allLabels, finalPC)

-- | Build WAM instructions for 1-column indexed fact predicate.
buildFact1Code :: (String -> Int) -> String -> [String] -> Int -> ([Instruction], [(String, Int)])
buildFact1Code iAtom predName vals startPC =
    let disp = [(Atom (iAtom v), predName ++ "_" ++ show i) | (i, v) <- zip [0..] vals]
        switchInstr = SwitchOnConstant (Map.fromList disp)
        factCode = concatMap (\(i, v) -> [GetConstant (Atom (iAtom v)) 1, Proceed]) (zip [0..] vals)
        factLabels = [(predName ++ "_" ++ show i, startPC + 1 + i * 2) | i <- [0..length vals - 1]]
    in (switchInstr : factCode, (predName ++ "/1", startPC) : factLabels)

main :: IO ()
main = do
    args <- getArgs
    let factsDir = if null args then "." else head args

    t0 <- getCurrentTime


    -- Phase 2b.2b: int_atom_seeds(lmdb) mode — LMDB-resident interning.
    -- Reads the s2i / i2s / article_category / category_parent sub-dbs
    -- written by the Phase 1 ingester
    -- (src/unifyweaver/runtime/python/lmdb_ingest/ingest_to_lmdb.py).
    -- Skips TSV parsing + string interning entirely; the intern table
    -- and edge index are loaded from binary LMDB pages.
    --
    -- The 5_000_000-edge threshold caps in-memory parentsIndex growth
    -- at ~80 MB; enwiki-scale inputs (~28M edges) need to switch to
    -- LMDB-cursor BFS (Phase 2b.3 follow-up). See loader docstring.
    --
    -- See docs/design/WAM_LMDB_RESIDENT_INTERNING_*.md.
    let lmdbInternDir = factsDir ++ "/lmdb"
    internEnv <- openLmdbInternEnvReadonly lmdbInternDir
    !lmdbInternTable <- loadInternTableFromLmdb internEnv "s2i" "i2s"
    !lmdbArticleCategories <- loadArticleCategoriesFromLmdb internEnv "article_category"


    -- cursor demand-BFS mode: skip the pre-load entirely. Edges live
    -- in LMDB (cpEdgeLookup for kernel walks; category_child cursor
    -- for demand-set BFS). parentsIndexInterned will be IM.empty.
    let !lmdbParentsIndex = IM.empty :: IM.IntMap [Int]





    -- Int-atom-seeds mode: skip TSV loading entirely.  Seeds and roots
    -- are raw int32 IDs from a file (typically generated from the LMDB
    -- key set).  Edges live in LMDB; no in-memory category_parent map.
    seedIds <- loadIntColumn (factsDir ++ "/seed_ids.txt")
    rootIds <- loadIntColumn (factsDir ++ "/root_ids.txt")
    -- Stub bindings so downstream code compiles uniformly across modes.
    let categoryParents = [] :: [(String, String)]
        articleCategories = [] :: [(String, String)]
        roots = [] :: [String]


    t1 <- getCurrentTime
    let loadMs = round (diffUTCTime t1 t0 * 1000) :: Int





    -- Int-atom-seeds(lmdb) mode: the intern table comes from LMDB
    -- (s2i / i2s sub-dbs) — already loaded into lmdbInternTable above.
    -- iAtom is identity since seed/edge IDs are pre-interned int32.
    let !fullInternTable = lmdbInternTable
        iAtom :: Int -> Int
        iAtom = id



    -- Build merged code: compiled predicates + runtime facts.
    -- When FFI handles fact lookups, the WAM-compiled fact code is pure
    -- waste (42% of time, 85% of allocation). The generator injects the
    -- appropriate block here based on which predicates are FFI-owned.
    let mergedCodeRaw = allCode
        mergedLabels = allLabels
        -- Pre-resolve Call instructions to CallResolved at startup so the
        -- hot path skips wsLabels string lookups. Foreign/indexed predicates
        -- are kept as Call so runtime dispatch (executeForeign/callIndexedFact2)
        -- still applies.
        foreignPreds = ["category_ancestor/4"]
        mergedCode = resolveCallInstrs mergedLabels foreignPreds mergedCodeRaw





    -- Int-atom-seeds(lmdb): seeds and root still come from int-id
    -- config files (seed_ids.txt / root_ids.txt). Edges populate
    -- parentsIndexInterned directly from the LMDB category_parent
    -- sub-db (loaded above into lmdbParentsIndex).
    let !seedCats = seedIds
        !root = case rootIds of
                  []      -> 0
                  (r:_)   -> r
        n = fromIntegral (5 :: Int) :: Double
        negN = -n
        seedLimitMetric  = Nothing :: Maybe String
        seedFilterMetric = Nothing :: Maybe String
        !parentsIndexInterned = lmdbParentsIndex



    -- Demand-driven pruning: compute the backward-reachable set from root,
    -- then filter parentsIndex to only include edges to reachable parents.
    -- This prunes branches that can never reach root, reducing DFS work.
    -- On sparse graphs this eliminates most exploration; on dense graphs
    -- (like Wikipedia categories, ~95% reachable) the effect is small.
    let !rootId = iAtom root
        !demandFilterSpec = (HopLimit Nothing)
    !demandFilterResult <- runDemandBFSCursor demandFilterSpec internEnv "category_child" rootId
    let !demandSet = dfrInSet demandFilterResult
        !demandSize = IS.size demandSet
        !totalNodes = -1 :: Int  -- unknown without iterating LMDB
        !filteredSize = -1 :: Int  -- LMDB is the source of truth; no filteredParents map
        !filteredSeedCats = filter (\cat -> IS.member (iAtom cat) demandSet) seedCats
        !demandSkippedSeeds = length seedCats - length filteredSeedCats

    -- Phase B1: LMDB fact source setup (conditional, empty when not enabled)
    -- Phase B1: LMDB fact source setup (dupsort layout, externally ingested)
    let lmdbDir = factsDir ++ "/lmdb"
    lmdbExists <- doesDirectoryExist lmdbDir
    if not lmdbExists then
      error ("LMDB not found at " ++ lmdbDir ++ "; build it via the streaming pipeline first")
    else
      hPutStrLn stderr "LMDB database found (dupsort layout)."
    cpEdgeLookup <- openLmdbEdgeLookup lmdbDir "category_parent"
    cpFactSource <- lmdbFactSource lmdbDir "category_parent"

    -- Build the read-only WamContext ONCE before the seed loop. The hot
    -- WamState gets a fresh copy per seed; the cold context is shared.
    -- wcInternTable is the system-wide atom intern table (compile-time +
    -- runtime atoms). wcFfiFacts feeds the FFI kernel path.
    let !ctx = (mkContext mergedCode mergedLabels)
            { wcForeignConfig = Map.singleton "max_depth" 10
            , wcLoweredPredicates = Lowered.loweredPredicates
            , wcInternTable   = fullInternTable
            , wcFfiFacts      = Map.singleton "category_parent" parentsIndexInterned

            , wcFactSources   = Map.singleton "category_parent" cpFactSource
            , wcEdgeLookups   = Map.singleton "category_parent" cpEdgeLookup
            }

    t2 <- getCurrentTime

    -- Per-seed query, parallelized with parMap rdeepseq.
    -- Each seed gets an independent WamState (via the query body closure)
    -- but they all share the immutable WamContext. No locking required.
    -- Run with +RTS -N to use multiple cores.
    --
    -- rdeepseq forces each (cat, weightSum) fully in parallel. Without
    -- deepseq, the tuple constructor would be WHNF but the Double inside
    -- could remain a thunk, deferring the actual work to later.
    -- parMap iterates over `filteredSeedCats`. With demand
    -- filtering active, that's `filteredSeedCats` (defined in the
    -- demand-filter block above) — seeds outside the root-bound demand
    -- set are removed BEFORE we spawn sparks, so we don't pay
    -- synchronization overhead for trivial work. With demand filtering
    -- off, this is `seedCats` and behaviour matches the pre-#1876 path.
    let !seedResultsForced = parMap rdeepseq (\cat ->
            let { wsVarId = 1000000 ; s0 = emptyState { wsPC = fromMaybe 1 $ Map.lookup "category_ancestor$effective_distance_sum_selected/3" mergedLabels, wsRegs = IM.fromList [ (1, Atom (iAtom cat)), (2, Atom (iAtom root)), (3, Unbound wsVarId) ], wsCP = 0 } ; !result = case run ctx s0 of { Just s1 -> case IM.lookup wsVarId (wsBindings s1) of { Just v -> case extractDouble fullInternTable (derefVar (wsBindings s1) v) of { Just ws -> ws ; Nothing -> 0.0 } ; Nothing -> 0.0 } ; Nothing -> 0.0 } } in (cat, result)
            ) filteredSeedCats
        -- Force the spine so parMap sparks all run before the timing
        -- endpoint. deepseq on the list forces every element too.
        !_ = seedResultsForced `deepseq` ()

    let !seedWeightSums = Map.fromList [(cat, ws) | (cat, ws) <- seedResultsForced, ws > 0]
        !forcedSize = Map.size seedWeightSums  -- force the map

    t3 <- getCurrentTime
    let queryMs = round (diffUTCTime t3 t2 * 1000) :: Int



    -- Int-atom-seeds: no article aggregation; emit per-seed effective
    -- distance directly so the output is comparable to the standalone
    -- enwiki-dfs-benchmark.
    let invN = -1.0 / n
        results = sort
            [ (ws ** invN, show seed)
            | (seed, ws) <- Map.toList seedWeightSums
            , ws > 0
            ]


    t4 <- getCurrentTime
    let aggMs = round (diffUTCTime t4 t3 * 1000) :: Int
        totalMs = round (diffUTCTime t4 t0 * 1000) :: Int

    -- Output TSV
    putStrLn "article\troot_category\teffective_distance"


    mapM_ (\(deff, art) ->
        putStrLn (art ++ "\t" ++ show root ++ "\t" ++ showFFloat6 deff)
        ) results

    hFlush stdout

    -- Metrics to stderr
    hPutStrLn stderr $ "mode=wam_haskell_accumulated"
    hPutStrLn stderr $ "load_ms=" ++ show loadMs
    hPutStrLn stderr $ "query_ms=" ++ show queryMs
    hPutStrLn stderr $ "aggregation_ms=" ++ show aggMs
    hPutStrLn stderr $ "total_ms=" ++ show totalMs
    hPutStrLn stderr $ "seed_count=" ++ show (length seedCats)
    maybe (return ()) (\v -> hPutStrLn stderr $ "seed_limit=" ++ v) seedLimitMetric
    maybe (return ()) (\v -> hPutStrLn stderr $ "seed_filter=" ++ v) seedFilterMetric
    hPutStrLn stderr $ "tuple_count=" ++ show (Map.size seedWeightSums)
    hPutStrLn stderr $ "article_count=" ++ show (length results)
    hPutStrLn stderr $ "demand_set_size=" ++ show demandSize
    hPutStrLn stderr $ "demand_total_nodes=" ++ show totalNodes
    hPutStrLn stderr $ "demand_filtered_nodes=" ++ show filteredSize
    hPutStrLn stderr $ "demand_skipped_seeds=" ++ show demandSkippedSeeds

-- | Format a Double to 6 decimal places.
showFFloat6 :: Double -> String
showFFloat6 x = Numeric.showFFloat (Just 6) x ""

-- | Collect all Hops solutions by looking up the query variable in wsBindings.
-- The seed loop creates an Unbound variable with hopsVarId and stores it in
-- A3. The WAM binds that variable as it runs (via clause 1's GetConstant or
-- clause 2's is/2). We must NOT read wsRegs[3] directly because the recursive
-- call's GetConstant 1 3 will clobber A3 with Integer 1, while the OUTER's
-- output variable is bound to the correct higher value via is/2 to wsBindings.
collectSolutions :: WamContext -> WamState -> Int -> [Double]
collectSolutions !ctx s0 hopsVarId =
    case run ctx s0 of
      Nothing -> []
      Just s1 ->
        let hopsVal = case IM.lookup hopsVarId (wsBindings s1) of
              Just v -> extractDouble (wcInternTable ctx) (derefVar (wsBindings s1) v)
              Nothing -> Nothing
            hops = fromMaybe 0 hopsVal
            rest = case backtrack s1 of
              Just s2 -> collectSolutions ctx s2 hopsVarId
              Nothing -> []
        in case hopsVal of
          Just _ -> hops : rest
          Nothing -> rest

-- | Collect all Hops solutions via executeForeign dispatch.
-- Used when the query predicate has an FFI kernel — bypasses WAM
-- code and calls the native kernel directly. The first result comes
-- from executeForeign; subsequent results via backtrack through the
-- ChoicePoints that executeForeign created. After backtrack, the
-- FFIStreamRetry handler binds the next result directly — no run needed.
collectForeignSolutions :: WamContext -> String -> WamState -> Int -> [Double]
collectForeignSolutions !ctx pred s0 hopsVarId =
    case executeForeign ctx pred s0 of
      Nothing -> []
      Just s1 -> extractFfiAndBacktrack ctx pred s1 hopsVarId

-- | Extract the hops value from an FFI result state, then backtrack for more.
-- FFIStreamRetry choice points bind the next result directly on backtrack,
-- so no run ctx is needed between results.
extractFfiAndBacktrack :: WamContext -> String -> WamState -> Int -> [Double]
extractFfiAndBacktrack !ctx pred s1 hopsVarId =
    let hopsVal = case IM.lookup hopsVarId (wsBindings s1) of
          Just v -> extractDouble (wcInternTable ctx) (derefVar (wsBindings s1) v)
          Nothing -> Nothing
        hops = fromMaybe 0 hopsVal
        rest = case backtrack s1 of
          Just s2 -> extractFfiAndBacktrack ctx pred s2 hopsVarId
          Nothing -> []
    in case hopsVal of
      Just _ -> hops : rest
      Nothing -> rest

-- | Extract a Double from a Value.
extractDouble :: InternTable -> Value -> Maybe Double
extractDouble _ (Integer h) = Just (fromIntegral h)
extractDouble _ (Float h) = Just h
extractDouble tbl (Atom aid) = case reads (lookupAtom tbl aid) of [(h, "")] -> Just h; _ -> Nothing
extractDouble _ _ = Nothing

-- | DemandFilterSpec selects which demand-filter strategy the runtime
-- runs. See docs/design/WAM_DEMAND_FILTER_SPECIFICATION.md.
--   HopLimit Nothing  — unbounded reverse BFS (legacy default, matches
--                       today's behaviour when no directive is declared).
--   HopLimit (Just n) — depth-bounded reverse BFS (sound when n ≥ kernel
--                       max_depth; misses unreachable seeds otherwise).
--   Flux _ _          — flux-weighted top-K with optional spark sort.
--                       Phase 2.5; emits panic at runtime in Phase 2.
--   DfNone            — no filter; demand set is the full universe of
--                       node IDs in the edge map.
data DemandFilterSpec
  = HopLimit { dfMaxHops :: !(Maybe Int) }
  | Flux     { dfCacheTopK :: !Int, dfSortSparks :: !Bool }
  | DfNone
  deriving (Show)

-- | Output of a single runDemandBFS call. The seed pre-filter consumes
-- dfrInSet; cache warming consumes dfrFluxScores when present;
-- spark scheduling consumes dfrSortedSeeds when present.
data DemandFilterResult = DemandFilterResult
  { dfrInSet       :: !IS.IntSet
  , dfrSortedSeeds :: !(Maybe [Int])
  , dfrFluxScores  :: !(Maybe (IM.IntMap Double))
  } deriving (Show)

-- | Dispatch on DemandFilterSpec and produce a DemandFilterResult.
-- HopLimit and DfNone are implemented in Phase 2; Flux is a panic stub
-- until Phase 2.5.
runDemandBFS :: DemandFilterSpec -> IM.IntMap [Int] -> Int -> DemandFilterResult
runDemandBFS spec parents rootId = case spec of
  HopLimit maxHops ->
    DemandFilterResult
      { dfrInSet       = computeDemandSetHopLimited parents rootId maxHops
      , dfrSortedSeeds = Nothing
      , dfrFluxScores  = Nothing
      }
  Flux {} ->
    error "runDemandBFS: Flux strategy not yet implemented (Phase 2.5). \
          \Use HopLimit or DfNone."
  DfNone ->
    DemandFilterResult
      { dfrInSet       = IS.fromList (IM.keys parents)
      , dfrSortedSeeds = Nothing
      , dfrFluxScores  = Nothing
      }

-- | Phase 2b.3 cursor-mode counterpart of runDemandBFS. Same dispatch
-- shape but the demand set is computed via LMDB cursor on the
-- category_child sub-db instead of an in-memory IntMap. No 5M-edge
-- ceiling. DfNone returns an empty set in this mode (the universe of
-- nodes is unknown without iterating LMDB; the seed pre-filter just
-- skips the membership test when the set is empty).
runDemandBFSCursor :: DemandFilterSpec -> MDB_env -> String -> Int
                   -> IO DemandFilterResult
runDemandBFSCursor spec env childDbName rootId = case spec of
  HopLimit maxHops -> do
    inSet <- computeDemandSetCursorBFS env childDbName rootId maxHops
    return DemandFilterResult
      { dfrInSet       = inSet
      , dfrSortedSeeds = Nothing
      , dfrFluxScores  = Nothing
      }
  Flux {} ->
    error "runDemandBFSCursor: Flux strategy not yet implemented (Phase 2.5)."
  DfNone ->
    return DemandFilterResult
      { dfrInSet       = IS.empty
      , dfrSortedSeeds = Nothing
      , dfrFluxScores  = Nothing
      }

-- | Compute the backward-reachable demand set from a root node, with an
-- optional hop limit. Nothing = unbounded (legacy behaviour). The bound
-- counts edges traversed, so HopLimit (Just 0) returns just {rootId}.
computeDemandSetHopLimited :: IM.IntMap [Int] -> Int -> Maybe Int -> IS.IntSet
computeDemandSetHopLimited parents rootId maxHops =
    bfs 0 (IS.singleton rootId) (IS.singleton rootId)
  where
    -- Build reverse adjacency: parent → [children]
    !reverseAdj = IM.fromListWith (++)
        [(p, [child]) | (child, ps) <- IM.toList parents, p <- ps]
    bfs !depth !frontier !visited
      | IS.null frontier = visited
      | exceededHopLimit depth = visited
      | otherwise =
          let newNodes = IS.fromList
                [ c | node <- IS.toList frontier
                    , c <- IM.findWithDefault [] node reverseAdj
                    , not (IS.member c visited) ]
          in bfs (depth + 1) newNodes (IS.union visited newNodes)
    exceededHopLimit !d = case maxHops of
      Nothing -> False
      Just n  -> d >= n

-- | Backward-compat alias for unbounded demand BFS. Pre-Phase-2 callers
-- and any test asserting this name keep working unchanged.
computeDemandSet :: IM.IntMap [Int] -> Int -> IS.IntSet
computeDemandSet parents rootId =
    computeDemandSetHopLimited parents rootId Nothing

-- | Filter a child→[parents] map to the structural demand set.
-- Iterates the usually-small demand set instead of scanning every graph node.
filterByDemand :: IS.IntSet -> IM.IntMap [Int] -> IM.IntMap [Int]
filterByDemand demandSet parents = IS.foldl' addChild IM.empty demandSet
  where
    addChild acc child =
      case IM.lookup child parents of
        Nothing -> acc
        Just ps ->
          case filter (`IS.member` demandSet) ps of
            [] -> acc
            ps' -> IM.insert child ps' acc
