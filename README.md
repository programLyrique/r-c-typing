# R C typing

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