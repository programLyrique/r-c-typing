# R C typing

[![CI](https://github.com/programLyrique/r-c-typing/actions/workflows/ci.yml/badge.svg)](https://github.com/programLyrique/r-c-typing/actions/workflows/ci.yml)

Type functions written in C using the R C API to R types.


## Dependencies

- OCaml >= 5.3.0
- dune
- mlsem
- sstt
- cmdliner

You can install the OCaml libraries using `opam`. 

`mlsem` and `sstt` are only on GitHub so far but can be installed with `opam pin`.


## Tree sitter 

You need to compile and install `https://github.com/E-Sh4rk/r-parser`. 
Set the following environment variables:

```bash
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/.../r-parser/core/tree-sitter/lib/
export TREESITTER_INCDIR=/.../r-parser/core/tree-sitter/include/
export TREESITTER_LIBDIR=/.../r-parser/core/tree-sitter/lib/
```

## Adding type inference tests

Use the provided helper script so the test list stays in sync.

```bash
./add_test.sh myfunction
```

This creates `test/cprogs/myfunction.c`, generates `test/cprogs/myfunction.expected` using the current inference output, and updates `test/dune.inc` with the corresponding rules. Then run the tests with:

```bash
dune runtest
```

Avoid editing `test/dune.inc` manually. Re-run `add_test.sh` if you need to refresh the expected output.

## CLI examples

Show the parsed AST for functions whose name contains "from":

```bash
dune exec r-c-typing -- --past -f from test/cprogs/string.c
```

Run inference for a single file:

```bash
dune exec r-c-typing -- test/cprogs/string.c
```
