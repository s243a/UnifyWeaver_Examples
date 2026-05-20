# Effective-Distance Benchmark: Haskell vs Rust at Wikipedia Scale

**Generated artefact**: this report accompanies the WAM-Haskell effective-distance code generated at [`../lmdb_l2_parallel_ffi/`](../lmdb_l2_parallel_ffi/) by the [UnifyWeaver](https://github.com/s243a/UnifyWeaver) generator.

**Scope**: cross-target measurement of the same `category_ancestor` kernel (with `max_depth(10)` and visited-set cycle detection) when generated as Haskell vs. Rust, run against real Wikipedia category graphs. Covers the algorithmic semantics, the hardware/toolchain configuration, and a discussion of why the relative performance reverses between scales.

**Snapshot date**: 2026-05-19.

**TL;DR**: At 297k edges (simplewiki) the Rust generated bench finishes in ~31 ms warm / ~127 ms cold per single-query process; the Haskell generated bench finishes the same workload in 226 ms at `-N1`. At 9.93M edges (enwiki) the relationship inverts: the Rust generated bench takes ~148 s per single-query process; the Haskell generated bench finishes in 733 ms at `-N4`. The 200× reversal is driven by **eager vs lazy demand-set materialisation**, not by anything about Rust vs Haskell as languages. The Rust generated bench builds an in-memory `Vec<(child, parent)>` of every demand-set edge up front; the Haskell generated bench streams from LMDB cursors lazily during the kernel walk. Below ~190 k queries per process at enwiki, the lazy design wins. Above that threshold, eager wins. The full discussion is in §3 and §4.

---

## Why publish this generated code at all?

The code under [`../lmdb_l2_parallel_ffi/`](../lmdb_l2_parallel_ffi/) is a **generated artefact** from UnifyWeaver, not a hand-written Haskell library, and not a stable public API. It's a specific effective-distance benchmark instance with a specific backend profile: generated Haskell, LMDB-backed facts, lowered helper functions, native kernels through FFI, an L2 cache, and parallelised outer work.

That makes it useful as:

- An **example** of what UnifyWeaver's WAM-Haskell target can produce end-to-end.
- A **performance target** for the generator — re-generating from the same source facts should produce the same shape of code, and regression tests can compare timings.
- A **starting point** for users who want to fork the generated form and customise it.

It is deliberately *not* packaged as a Hackage library. The reusable pieces — if any — would be one of:

- The UnifyWeaver generator / runtime support (in the main [UnifyWeaver](https://github.com/s243a/UnifyWeaver) repo).
- A fact-source or LMDB-adapter layer (the runtime parts of the generated code).
- A separately-designed graph-kernel package with a stable API.

None of those are factored out yet. The generated benchmark is the most concrete thing we have to look at; users can regenerate it from the source facts and compiler settings, and the generated code can change as the target improves without pretending to have package-level semver stability.

## 1. What the benchmark computes

The benchmark computes an *effective distance* over a category graph. Starting from article/category seeds, it walks upward through `category_parent` edges toward root categories. Each simple directed path contributes a decayed weight based on path length:

```text
weight += (hops + 1)^(-N)
d_eff = weight^(-1 / N)
```

Multiple distinct paths matter — each path contributes to the final weight. The kernel definition is in [`effective_distance.pl`](https://github.com/s243a/UnifyWeaver/blob/main/examples/benchmark/effective_distance.pl); the WAM-Haskell generator pipeline lives in [`src/unifyweaver/targets/wam_haskell_target.pl`](https://github.com/s243a/UnifyWeaver/blob/main/src/unifyweaver/targets/wam_haskell_target.pl).

## 2. Algorithm: where it is exact vs. an approximation

The `category_ancestor` kernel follows directed `category_parent` edges upward with a `Visited` set for cycle detection. The expected path shape is a caret (^) — ascending through parent edges to an apex root, possibly through multiple diverging parent chains. Whether the kernel is exact or an approximation depends on both the graph structure and the question being asked:

- On a *tree*: exact — one parent per node, one caret path to each root, nothing is missed.
- On a *DAG*, if the question is "enumerate all directed ancestor paths": exact — multiple parent chains (fan-in) are fully preserved, and all caret-shaped upward paths contribute to the weight sum.
- On a *DAG*, if the question is "general graph effective distance": an approximation — shortcuts exist that dip down through at least one child node before ascending to the apex. In graphs where depth is uneven across branches, these downward-then-upward routes can be fewer total hops than the direct caret-shaped path, even though they look like a detour. The kernel misses them because it only follows edges upward.
- On a *directed graph with cycles* or a general mixed graph: an approximation for the same reason, with the additional note that directed cycles are handled safely by the visited set rather than causing infinite loops.

Wikipedia's category graph falls into the last case — directed cycles exist alongside the DAG structure. So this is technically an approximation of general-graph effective distance. But for the specific question of *ancestor-reachability distance* in the category hierarchy, the directed-upward traversal is complete by design. Whether that's the right question for a given use case is worth being explicit about.

## 3. Performance: numbers + context

### 3.1 Hardware + toolchain

- **Host CPU**: Intel Core i7-10700KF @ 3.80 GHz (8C/16T)
- **Host RAM**: 16 GiB (Windows 11)
- **WSL2 allocation** (from `.wslconfig`):
  - `memory=5GB` (`MemTotal` reports 4.8 GiB)
  - `processors=8` (matches host physical cores; SMT not exposed to the VM)
  - `swap=5GB`
- **OS**: WSL2 (Linux 6.6.114.1-microsoft-standard-WSL2), `x86_64`
- **Haskell**: GHC 8.6.5, Cabal 2.4.0.0
- **Rust**: rustc 1.91.1 (`ed61e7d7e 2025-11-07`)
- **GHC RTS**: `-A64M` baked into the matrix bench's `with_rtsopts`, multi-capability via `+RTS -N{1,2,4}`

Note: 4.8 GiB total is tight for the larger fixtures. `100k_cats` (196,900 edges) fits comfortably but the full `enwiki` sweep (millions of edges) is right at the edge — `parMap` with high spark counts can push into swap, which is one reason `-N4` doesn't always improve over `-N2` on the larger scales.

### 3.2 Head-to-head at Wikipedia scale (Rust vs Haskell, LMDB, May 2026)

Both targets compile to native code, both read facts from the same LMDB layout (categorylinks edges + identity intern table). The bench measures full-process wall-clock from `main()` to exit — no harness, no cabal resolution. Five trials per cell; tables show trial-1 (cold OS cache) and warm median so readers can pick the framing.

#### `simplewiki` — 297,283 edges, 5,000 demand-set seeds

Both languages run against the same simplewiki LMDB. Rust uses lmdb-zero cursor BFS + eager `Vec<(child,parent)>` materialisation of demand-set edges. Haskell uses the [`lmdb`](https://hackage.haskell.org/package/lmdb) Hackage binding with lazy cursor reads in `resident_cursor` mode.

| Run | cold T1 | warm median | mode | -N |
| --- | ---: | ---: | --- | ---: |
| **Rust** (lmdb_zero cursor) | **58 ms** | **31 ms** | eager mat. | 1 |
| Haskell `resident` (IntMap) | (not measured cold) | 443 ms | eager IntMap pre-load | 1 |
| Haskell `resident_cursor` | (not measured cold) | **226 ms** | lazy cursor | 1 |
| Haskell `resident_cursor` | (not measured cold) | 207 ms | lazy cursor | 2 |
| Haskell `resident_cursor` | (not measured cold) | 234 ms | lazy cursor | 4 |

Haskell numbers are from Phase L#7 (medians of 3 trials, `+RTS -A64M`). Rust numbers are from May-2026 R5 (medians of 5 trials, single-threaded — the Rust bench doesn't have parallelism yet). The simplewiki LMDB is ~30 MB and fits entirely in OS page cache, which is why the warm/cold gap is large for Rust (3.7×) and why Haskell's medians benefit from re-runs too.

At this scale Rust wins on a single query (58 ms cold, 31 ms warm vs 226 ms Haskell `-N1`), and **Haskell's eager IntMap mode is actually 1.96× slower than its own lazy cursor mode** — building the index costs more than the time it saves.

#### `enwiki` — 9,932,244 edges, 1,000 demand-set seeds

Both targets run against the same enwiki LMDB (~448 MB). Single root (id 97,688,913) with 796,695 descendants in the demand set. Haskell's `resident` IntMap mode hits the 5M-edge guard and refuses to load — only the lazy cursor mode is viable. Five trials each.

| Run | cold T1 | warm median | -N | notes |
| --- | ---: | ---: | ---: | --- |
| **Rust** (lmdb_zero cursor + eager mat.) | **162 s** | **148 s** | 1 | `load_ms` ≈ 140 s, `query_ms` ≈ 4 ms |
| Haskell `resident_cursor` (sequential BFS) | (not measured cold) | 1,129 ms | 1 | `load_ms = 0` |
| Haskell `resident_cursor` (sequential BFS) | (not measured cold) | 1,066 ms | 2 | |
| Haskell `resident_cursor` (sequential BFS) | (not measured cold) | 1,138 ms (flat) | 4 | |
| Haskell `resident_cursor` (parallel BFS) | (not measured cold) | 928 ms | 2 | |
| Haskell `resident_cursor` (parallel BFS) | (not measured cold) | **733 ms** | 4 | best Haskell |
| Haskell `resident` (IntMap) | — | — | — | fails to load (5M-edge guard) |

**At enwiki Rust is ~200× slower than Haskell `-N4`.** That isn't a Rust language deficiency — both targets are native-compiled, both wrap the same LMDB C library. The difference is in the bench design: Rust eagerly materialises every (child, parent) edge in the demand set into a `Vec<(String,String)>` at startup. That's ~796 k LMDB cursor lookups + ~800 k String allocations, all before the kernel loop runs. Haskell's cursor mode does each parent lookup lazily during the kernel walk — only the seeds that actually need an edge pay for it.

The cost decomposition makes this concrete. Rust's wall-clock per process is:

```
total ≈ M (eager materialisation) + N × ε (per-seed kernel work)
```

For our enwiki run: M ≈ 140 s (depends on demand-set size = 796,695), ε ≈ 4 µs per seed. Haskell's wall-clock is essentially `N × p` with p ≈ 0.73 ms per seed at `-N4` and no fixed materialisation cost.

| Workload | Rust (1 process) | Haskell (`-N4`) |
| --- | ---: | ---: |
| 1 query | 140 s | ~1 ms |
| 1,000 queries (batched in one process) | 140.004 s | 733 ms |
| 1,000 queries (one process each) | 140,000 s ≈ 39 hours | 733 s ≈ 12 minutes |
| Crossover: how many queries per process for Rust to win? | ≈ 190,000 | — |

So **at enwiki, Rust's bench design only pays off if you can batch >190 k queries per process**. For one-shot interactive workloads — a user asking "how far is article X from root Y?" — Haskell's lazy cursor mode wins by 200× and the gap is structural, not a tuning issue.

#### Cross-scale shape

The relationship between the two targets reverses across scales:

| Fixture | edges | demand_set | Rust wins by | …because |
| --- | ---: | ---: | --- | --- |
| simplewiki | 297k | 14,661 | **7.3× at one query (warm)** | M is tiny (~17 ms), so eager mat. costs less than Haskell's demand-BFS setup |
| enwiki | 9.93M | 796,695 | Haskell wins by **200×** at one query | M scales linearly with demand-set, dominates total cost |

### 3.3 Why the relationship flips: eager vs lazy materialisation

The Rust-vs-Haskell story collapses into a more fundamental design choice. Both languages can implement either pattern; what was measured is two specific implementations:

- **Eager** (Rust bench's current shape, Haskell `resident` IntMap mode): build an in-memory index up front, then amortise across the query stream. Wins when the index fits in memory **and** you batch enough queries.
- **Lazy** (Haskell `resident_cursor`, hypothetical Rust cursor-only mode): read from LMDB on demand during the kernel loop. Wins for one-shot queries, mixed-root workloads, freshness-sensitive workloads, and anything past the memory wall.

Three observations from the data:

1. **At simplewiki, eager *loses inside Haskell too*.** The `resident` IntMap pre-load costs 183 ms — more than the 159 ms Haskell's cursor mode spends streaming. Cursor wins 1.96× at `-N1`. So even when the index fits comfortably, eager isn't a free win.
2. **At enwiki, eager loses by 200×.** The 140 s materialisation pays for itself only above ~190 k queries per process. For interactive workloads it's pure overhead. Haskell's IntMap mode doesn't even get to fail comparably — the 5M-edge guard rejects it before measurement is possible.
3. **Batching is structurally harder at scale.** Even if batching is viable, the prerequisites — knowing all queries upfront, grouping by root, fitting the materialisation in RAM, handling graph freshness — get harder as edge count grows. Memory pressure, multi-root query streams, and update propagation are real production constraints.

The natural next question is *why not give Rust lazy cursor mode, or Haskell eager*. The answer for Haskell is "we did" — `resident` (IntMap) is the eager variant and it loses at every measured scale. The answer for Rust is "future work": the current bench was designed around eager materialisation as a baseline, and a lazy-cursor Rust variant would likely close most of the enwiki gap. Whether it would *match* Haskell's `-N4` parallel cursor or *beat* it depends on how cleanly Rust's parallelism can be wired in. The hypothesis worth testing is: **with both targets running lazy cursor mode, the gap collapses to per-thunk allocation differences and FFI call overhead, neither of which is dramatic on the kernel's path.** That measurement is open work.

For workloads at "decent scales" — millions of edges, single-query or small-batch workflows — the cost model says lazy wins. That's what motivates the [scan-strategy design](https://github.com/s243a/UnifyWeaver/blob/main/docs/design/SCAN_STRATEGY_PHILOSOPHY.md) and the [`cache_strategy(auto)` resolver](https://github.com/s243a/UnifyWeaver/blob/main/docs/design/CACHE_COST_MODEL_PHILOSOPHY.md) — the project's direction is to pick the right mode from workload metadata rather than hand-tuning per experiment. The longer-form architectural framing is in [`QUERY_PLAN_RUNTIME_PHILOSOPHY.md`](https://github.com/s243a/UnifyWeaver/blob/main/docs/design/QUERY_PLAN_RUNTIME_PHILOSOPHY.md).

### 3.4 Branching distribution (Wikipedia category graph)

Captured from the same `category_parent.tsv` fixtures used above. Confirms the asymmetric branching pattern that motivates separate parent/child decay constants in the flux-style cost function:

| Metric | 1k | 100k_cats |
| --- | --- | --- |
| Edges | 5,933 | 196,900 |
| Parents-per-child median | 3 | 2 |
| Parents-per-child P99 | 8 | 6 |
| Parents-per-child max | 10 | 29 |
| Children-per-parent median | 1 | 2 |
| Children-per-parent P99 | 14 | 39 |
| Children-per-parent max | 587 | 1,137 |

Parents-per-child is bounded; children-per-parent has a long tail. For the cost-function design discussion see [`COST_FUNCTION_PHILOSOPHY.md`](https://github.com/s243a/UnifyWeaver/blob/main/docs/design/COST_FUNCTION_PHILOSOPHY.md).

## 4. Caveats

- **Rust trial-1 numbers are from cold OS page cache; warm medians are from re-runs.** Haskell numbers were originally reported as 3-trial medians so they include similar warm-cache benefit. At simplewiki (~30 MB LMDB) the warm/cold gap is large (3.7× for Rust); at enwiki (448 MB LMDB) the gap collapses because the working set doesn't reliably stay hot in the WSL2 page cache. The cold-T1 column reflects what a "one user, one query" workload actually pays.
- **Rust is single-threaded today.** All Rust numbers are equivalent to `-N1` in Haskell terms. The Haskell `-N2`/`-N4` figures benefit from parMap parallelism that Rust hasn't been wired for yet; a parallel Rust variant is open work. For the eager-vs-lazy framing this doesn't change the conclusion — parallelism would multiply both `ε` and Haskell's `p`, not the fixed `M` overhead.
- **Both targets use synthetic identity intern tables.** Real-world output (mapping `cl_target_id` → category title) would need a second-pass ingest of `simplewiki-latest-linktarget.sql.gz` / `enwiki-latest-linktarget.sql.gz`. Filed as future work; doesn't affect timing.
- **No `-N8` sweep on Haskell.** WSL2 has 8 processors allocated but Phase L showed `-N2 ≈ -N4` flattening from spark-pool / GC contention at 100k. `-N8` would amplify the same effect; the priority is reducing per-spark overhead first.

## 5. Provenance of the numbers

- **Rust simplewiki** (R5 arc, branch `feat/wam-rust-bench-simplewiki`, commit `bf32a3fa`): 5-trial sweep at 5,000 demand-set seeds against best_root id=2 (subtree size 14,680 — within 0.13% of Haskell Phase L#7's root). LMDB ingest via [`examples/streaming/simplewiki_category_ingest_text.pl`](https://github.com/s243a/UnifyWeaver/blob/main/examples/streaming/simplewiki_category_ingest_text.pl) (with the R5 `UW_VAL_COL=2→6` fix); post-ingest via [`examples/benchmark/simplewiki_post_ingest.py`](https://github.com/s243a/UnifyWeaver/blob/main/examples/benchmark/simplewiki_post_ingest.py). Bench is `examples/benchmark/generate_wam_rust_matrix_benchmark.pl` in `lmdb cursor lmdb_zero` mode.
- **Rust enwiki** (R6 arc, branch `feat/wam-rust-bench-enwiki`, commit `e26d33e7`): 5-trial sweep at 1,000 demand-set seeds against best_root id=97,688,913 (subtree size 796,695 — same root as Haskell Phase L#8/9). LMDB ingest via [`examples/streaming/enwiki_category_ingest.pl`](https://github.com/s243a/UnifyWeaver/blob/main/examples/streaming/enwiki_category_ingest.pl); post-ingest via [`examples/benchmark/enwiki_post_ingest.py`](https://github.com/s243a/UnifyWeaver/blob/main/examples/benchmark/enwiki_post_ingest.py). Same matrix-bench generator.
- **Haskell** simplewiki + enwiki: Phase L#7 / #8 / #9 in [`WAM_PERF_OPTIMIZATION_LOG.md`](https://github.com/s243a/UnifyWeaver/blob/main/docs/design/WAM_PERF_OPTIMIZATION_LOG.md), medians of 3 trials, `kernels_on`, `+RTS -A64M`.

## 6. Closing

So the position this report supports:

- **The generated Haskell effective-distance code** is good as an examples-submodule artefact — it's right here at [`../lmdb_l2_parallel_ffi/`](../lmdb_l2_parallel_ffi/);
- **Hackage**: not yet, unless a stable reusable runtime / kernel / fact-source layer gets factored out;
- **Semantics**: exact for tree and ancestor-reachability on a DAG; an approximation of general-graph effective distance that excludes non-upward routes, with cycle detection to prevent loops;
- **Performance**: the simple "Rust is faster" framing doesn't survive the at-scale data. At 297k edges (simplewiki), the Rust generated bench's eager-materialisation design wins one-shot queries (~31 ms warm / 58 ms cold vs Haskell's 226 ms `-N1`). At 9.93M edges (enwiki), the relationship inverts — Rust's bench eagerly materialises the full demand-set edge list (140 s fixed cost) while Haskell's lazy cursor mode streams from LMDB on demand (733 ms at `-N4` total, no fixed cost). Rust would only beat Haskell at enwiki if a workload could batch >190 k queries per process, which excludes most real workloads. Both languages can implement either pattern; the design choice is what dominates, not the language. A lazy-cursor Rust variant is open work and would likely close most of the enwiki gap;
- **Next**: scan-strategy P3 (warm-build core) will land the cost-function-driven tree builder + workload-aware eager-vs-lazy mode selection. Implementation plan in [`SCAN_STRATEGY_IMPLEMENTATION_PLAN.md`](https://github.com/s243a/UnifyWeaver/blob/main/docs/design/SCAN_STRATEGY_IMPLEMENTATION_PLAN.md). The runtime-planner generalisation is captured in [`QUERY_PLAN_RUNTIME_PHILOSOPHY.md`](https://github.com/s243a/UnifyWeaver/blob/main/docs/design/QUERY_PLAN_RUNTIME_PHILOSOPHY.md).

## Links

- Main project: [UnifyWeaver](https://github.com/s243a/UnifyWeaver)
- Examples submodule: [UnifyWeaver_Examples](https://github.com/s243a/UnifyWeaver_Examples)
- Generator pipeline: [`src/unifyweaver/targets/wam_haskell_target.pl`](https://github.com/s243a/UnifyWeaver/blob/main/src/unifyweaver/targets/wam_haskell_target.pl)
- Kernel definition: [`examples/benchmark/effective_distance.pl`](https://github.com/s243a/UnifyWeaver/blob/main/examples/benchmark/effective_distance.pl)
- Scan-strategy design: [`SCAN_STRATEGY_PHILOSOPHY.md`](https://github.com/s243a/UnifyWeaver/blob/main/docs/design/SCAN_STRATEGY_PHILOSOPHY.md)
- Cache cost-model: [`CACHE_COST_MODEL_PHILOSOPHY.md`](https://github.com/s243a/UnifyWeaver/blob/main/docs/design/CACHE_COST_MODEL_PHILOSOPHY.md)
- Cost function philosophy: [`COST_FUNCTION_PHILOSOPHY.md`](https://github.com/s243a/UnifyWeaver/blob/main/docs/design/COST_FUNCTION_PHILOSOPHY.md)
- Query-plan runtime (precursor): [`QUERY_PLAN_RUNTIME_PHILOSOPHY.md`](https://github.com/s243a/UnifyWeaver/blob/main/docs/design/QUERY_PLAN_RUNTIME_PHILOSOPHY.md)
- WAM perf log (Haskell Phase L #7-9, #14): [`WAM_PERF_OPTIMIZATION_LOG.md`](https://github.com/s243a/UnifyWeaver/blob/main/docs/design/WAM_PERF_OPTIMIZATION_LOG.md)
