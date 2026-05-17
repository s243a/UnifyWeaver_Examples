# Effective Distance Graph Benchmark

This area stores generated artifacts for UnifyWeaver's effective-distance graph
benchmark. The source benchmark remains in the main UnifyWeaver repository; this
submodule keeps generated target-language projects out of the main repo.

## Problem

The workload computes an effective distance from article seeds to root
categories over a category graph. Each seed can reach a root through multiple
simple category paths. For a dimension `n`, each path contributes
`(hops + 1)^(-n)` to a root-specific weight sum, and the final distance is:

```text
d_eff = weight_sum^(-1/n)
```

The benchmark is intentionally awkward for generic WAM execution:

- it has recursive graph traversal with cycle checks
- it has repeated ancestor/root lookups over large fact tables
- it benefits from native kernels at the recursive boundary
- it exposes storage layout costs once facts no longer fit comfortably in memory
- it has enough independent seed/root work to make parallel execution valuable

## Source Links

Main benchmark driver:

- <https://github.com/s243a/UnifyWeaver/blob/main/examples/benchmark/benchmark_effective_distance_matrix.py>

Haskell generator:

- <https://github.com/s243a/UnifyWeaver/blob/main/examples/benchmark/generate_wam_haskell_matrix_benchmark.pl>

Rust generator:

- <https://github.com/s243a/UnifyWeaver/blob/main/examples/benchmark/generate_wam_rust_matrix_benchmark.pl>

Rust MediaWiki parser and direct LMDB sink:

- <https://github.com/s243a/UnifyWeaver/tree/main/src/unifyweaver/runtime/rust/mysql_stream>

## Generated Projects

- `haskell/gen/lmdb_l2_parallel_ffi/` is the optimized generated Haskell
  profile: lowered functions, FFI kernels, resident LMDB mode, sharded L2 cache,
  and generated parallel WAM choice points.
- `rust/gen/lowered_parallel_ffi/` is the corresponding generated Rust lowered
  + FFI comparison project.

## Cache Note

The generated Haskell LMDB profile currently uses a sharded L2 cache. That is
the conservative default for spark-based parallel execution because spark work
does not have stable region affinity, so per-thread L1 caches can duplicate the
same hot edge lookups across workers. An L1 tier may still be valuable at higher
parallelism if work routing can reduce duplication and the parallelization
overhead is kept low. This has not been validated above four cores on the
current development hardware.
