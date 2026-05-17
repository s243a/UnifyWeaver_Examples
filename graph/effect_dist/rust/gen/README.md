# Generated Rust Effective-Distance Benchmarks

This directory contains generated Rust effective-distance benchmark artifacts
used as comparison points for the generated Haskell profile.

## Included Profile

- `lowered_parallel_ffi/` - generated with:

```sh
swipl -q -s examples/benchmark/generate_wam_rust_matrix_benchmark.pl -- \
  data/benchmark/10k/facts.pl \
  examples/more/graph/effect_dist/rust/gen/lowered_parallel_ffi \
  accumulated functions kernels_on
```

This profile uses lowered Rust functions with FFI kernels enabled. It is not an
LMDB backend artifact; the Haskell generated project is the LMDB-focused profile
in this submodule.

The Rust generator lives in the main project:

- <https://github.com/s243a/UnifyWeaver/blob/main/examples/benchmark/generate_wam_rust_matrix_benchmark.pl>

The Rust MediaWiki parser and direct LMDB sink used by larger fixture
preparation lives in the main project:

- <https://github.com/s243a/UnifyWeaver/tree/main/src/unifyweaver/runtime/rust/mysql_stream>
