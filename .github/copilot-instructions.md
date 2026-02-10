# R C Typing - AI Coding Agent Instructions

## Project Overview

This OCaml project analyzes and types C functions that use the R C API. It parses C code with Tree-sitter, transforms it through multiple intermediate representations (CST → PAst → Ast → MLsem), and performs type inference using the MLsem framework with RSTT (R Set-Theoretic Types).

**Critical Architecture:** C code flows through 4 AST stages:

1. **CST** (Concrete Syntax Tree): Raw Tree-sitter output from [c-parser/Parse.ml](c-parser/Parse.ml)
2. **PAst** (Parsed AST): Position-annotated, untyped AST with string identifiers - see [lib/PAst.ml](lib/PAst.ml)
3. **Ast**: Typed AST with `Variable.t` instead of strings, translates R C API types (SEXP, etc.) - see [lib/ast.ml](lib/ast.ml)
4. **MLsem AST**: Final representation for type inference via `Ast.to_mlsem` in [lib/ast.ml](lib/ast.ml#L181)

## Critical Dependencies (Not in Public Registries)

The project requires **private GitHub repositories** that must be pinned with authenticated access:

- **MLsem**: Core type inference framework (`mlsem-types`, `mlsem-common`, `mlsem-system`, etc.) from `E-Sh4rk/MLsem`
- **SSTT/RSTT**: Set-theoretic type system (`sstt`, `sstt-repl`, `rstt`, `rstt-repl`) from `E-Sh4rk/sstt`
- **r-parser**: Tree-sitter C parser from `E-Sh4rk/r-parser`

Pin with: `opam pin add <pkg> git+https://<PAT>@github.com/E-Sh4rk/<repo>.git --yes`

**cmdliner version**: Must use exactly `1.0.4` due to API changes in later versions.

## Environment Setup (Required Before Build)

Tree-sitter requires these environment variables pointing to the r-parser installation:

```bash
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/.../r-parser/core/tree-sitter/lib/
export TREESITTER_INCDIR=/.../r-parser/core/tree-sitter/include/
export TREESITTER_LIBDIR=/.../r-parser/core/tree-sitter/lib/
```

See [.github/workflows/ci.yml](../.github/workflows/ci.yml#L117-L119) for CI setup patterns.

## Build & Test Workflow

```bash
dune build                    # Build all targets
dune runtest                  # Run all tests (compare typing_test.exe output vs .expected files)
dune exec bin/main.exe -- <file.c>  # Run type inference on a C file
```

**Test conventions** ([test/](../test/)):

- Each C file in `test/cprogs/*.c` has a matching `*.expected` file with the expected typed output
- Tests run via `typing_test.exe` (defined in [test/typing_test.ml](../test/typing_test.ml))
- `test/dune.inc` is auto-generated - use `./add_test.sh <name>` to add new tests (generates `.expected` and appends dune rules)
- Coverage reports use `bisect_ppx` - artifacts go to `_coverage/`

## Type System Integration

**Type definitions** live in [types/base.ty](../types/base.ty) using RSTT syntax:

- R vector types: `v(int)`, `v[1](dbl)` (vectors with length constraints)
- C types: `c_int`, `c_double`, `c_bool`, `c_true`, `c_false`, `c_int_na`
- R API functions: `allocVector`, `LENGTH`, `INTEGER`, `PROTECT`, etc.
- Set-theoretic operators: `&` (intersection), `|` (union), `~` (negation)

**Type parsing**: [lib/type_parser.ml](../lib/type_parser.ml) reads `.ty` files, distinguishes definitions (`:`) vs aliases (`=`)

**Initial environment**: [lib/defs.ml](../lib/defs.ml) loads base types and R C API function signatures into `Defs.initial_env`

## Key CLI Flags (bin/main.ml)

- `--cst`: Print concrete syntax tree
- `--past`: Print parsed (untyped) AST
- `--ast`: Print typed AST
- `--mlsem`: Print MLsem AST (final IR before inference)
- `--no-typing`: Skip type inference
- `--debug`: Enable debug output
- `-f <substring>`: Filter output to functions matching substring

## Inference Pipeline ([lib/runner.ml](../lib/runner.ml))

1. Parse C file → CST ([lib/parser.ml](../lib/parser.ml))
2. Convert CST → PAst (untyped, position-annotated)
3. Transform PAst → Ast via `PAst.transform` - converts string IDs to `Variable.t`, handles scoping
4. Convert Ast → MLsem AST via `Ast.to_mlsem`
5. Type inference via MLsem: `System.Refinement.refinements` → `System.Reconstruction.infer` → `System.Checker.typeof_def`
6. Output normalized type signature: `TyScheme.norm_and_simpl`

**Environment threading**: Functions accumulate typed signatures in `(idenv, env)` tuple - `idenv` maps function names to Variables, `env` maps Variables to types.

## Common Patterns

- **Variable creation**: Always use `MVariable.create Immut (Some <name>)` from MLsem for type-tracked identifiers
- **Position tracking**: Use `Position.t` everywhere for error reporting - convert Tree-sitter locations via `loc_to_pos` in [lib/parser.ml](../lib/parser.ml#L57)
- **Missing variables**: `extend_env` in [lib/runner.ml](../lib/runner.ml#L31) assigns `GTy.dyn` (any type) to free variables before inference
- **Error handling**: Type errors caught as `System.Checker.Untypeable` exceptions in [lib/runner.ml](../lib/runner.ml#L75)

## Anti-Patterns to Avoid

- Don't assume public opam packages - MLsem/SSTT require authenticated GitHub access
- Don't use newer cmdliner APIs - the project is locked to 1.0.4
- Don't skip environment setup - Tree-sitter paths are mandatory
- Don't directly edit `test/dune.inc` - regenerate via `add_test.sh`
- Don't forget to flush output after tests - see [test/typing_test.ml](../test/typing_test.ml#L25)

## Debugging Tips

- Use `--mlsem` flag to see the final IR sent to type inference
- Check `mlsen_recording.json` (generated after runs) for MLsem type system call traces
- Use `--debug` to see per-function inference status
- Tree-sitter parse errors appear in CST output - use `--cst` to diagnose parsing issues
