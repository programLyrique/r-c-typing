# R C typing

[![CI](https://github.com/programLyrique/r-c-typing/actions/workflows/ci.yml/badge.svg)](https://github.com/programLyrique/r-c-typing/actions/workflows/ci.yml)

Type functions written in C using the R C API to R types.

## Dependencies

- OCaml >= 5.3.0
- dune
- mlsem
- sstt
- rstt
- cmdliner

You can install the OCaml libraries using `opam`.

`mlsem`, `sstt`, and `rstt` are only on GitHub so far but can be installed with `opam pin`.

## Tree sitter

You need to compile and install `https://github.com/E-Sh4rk/r-parser`.
Set the following environment variables:

```bash
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/.../r-parser/core/tree-sitter/lib/
export TREESITTER_INCDIR=/.../r-parser/core/tree-sitter/include/
export TREESITTER_LIBDIR=/.../r-parser/core/tree-sitter/lib/
```

## Common commands

The repository includes a `Makefile` that wraps the required environment setup:

```bash
make build
make test
make release
make coverage
```

- `make build` runs `dune build`
- `make test` runs `dune runtest`
- `make release` runs `dune build --profile release`
- `make coverage` runs the test suite with `bisect_ppx`, stores raw coverage data in `_coverage/data/`, and writes the HTML report to `_coverage/html/index.html`

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

The positional argument is a `PATH`: either a C source file or an R package directory.

Header lookup can also be extended with repeated `-I/--include-dir` flags or with the `C_INCLUDE_PATH` environment variable.

Show the parsed AST for functions whose name contains "from":

```bash
dune exec r-c-typing -- --past -f from test/cprogs/string.c
```

Run inference for a single file:

```bash
dune exec r-c-typing -- test/cprogs/string.c
```

Run inference for an R package directory:

```bash
dune exec r-c-typing -- test/packages/testpkg
```

## Useful flags

| Flag                                  | Description                                                                                                                                                                                                                                                                                                              |
| ------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `--timeout SECONDS`                   | Per-function timeout for body inference. The rest of the package continues after a timeout.                                                                                                                                                                                                                              |
| `--fallback-c-signature`              | When body inference fails (untypeable, internal error, or timeout), bind the function at its declared C signature — `params -> ret_ty` — so callers can still be typed instead of cascading as "unbound variable". The original error is still printed, followed by an indented `fallback: <type>` line. Off by default. |
| `--debug`                             | Print intermediate AST/inference information.                                                                                                                                                                                                                                                                            |
| `--past`, `--ast`, `--mlsem`, `--cst` | Print the corresponding intermediate form.                                                                                                                                                                                                                                                                               |
| `-f/--filter SUBSTRING`               | Restrict printed output to symbols whose name contains `SUBSTRING`.                                                                                                                                                                                                                                                      |

## Type definitions

Builtin and external signatures are currently described in both `types/base.ty` and `types/posix.ty`.

The rstt syntax supports, among others:

- Vectors: `v[length](element_type)`
- Lists with typed tails: `{label:type; tail}`
- Intersections and unions: `t1 & t2`, `t1 | t2`

## Type precedence

When several type sources exist for the same symbol, the runner keeps the following precedence:

1. Types loaded from `.ty` files.
2. Types inferred from a full function definition with a body.
3. Types inferred from a declaration or signature-only C function.

In practice, this means:

- A symbol already defined in `types/*.ty` is never overridden by inferred C information.
- A later full function definition overrides a previously inferred declaration for the same symbol.
- If no body is available, the declaration or simple C signature is kept and used as the symbol type.
