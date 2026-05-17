# Generated Haskell Effective-Distance Benchmarks

This directory contains generated Haskell effective-distance benchmark artifacts.

The generated form is intentionally kept outside the main UnifyWeaver
repository because users can regenerate it from source, and the artifact is not
general enough to publish as a Hackage package.

## Included Profile

- `lmdb_l2_parallel_ffi/` - generated with:

```sh
swipl -q -s examples/benchmark/generate_wam_haskell_matrix_benchmark.pl -- \
  data/benchmark/10k/facts.pl \
  examples/more/graph/effect_dist/haskell/gen/lmdb_l2_parallel_ffi \
  accumulated functions kernels_on resident_auto
```

This is the optimized Haskell profile for this artifact set:

- `functions` selects lowered generated Haskell functions with WAM fallback.
- `kernels_on` keeps native FFI kernels enabled.
- `resident_auto` selects the LMDB-resident path and lets the cost model choose
  demand cursor versus in-memory loading.
- The generator resolved `lmdb_cache_mode(auto)` to sharded L2 cache for this
  generated project.
- The generated WAM code includes parallel choice-point emission where the
  Haskell target marks the work as forkable.

The benchmark driver and generator live in the main project:

- <https://github.com/s243a/UnifyWeaver/blob/main/examples/benchmark/benchmark_effective_distance_matrix.py>
- <https://github.com/s243a/UnifyWeaver/blob/main/examples/benchmark/generate_wam_haskell_matrix_benchmark.pl>
