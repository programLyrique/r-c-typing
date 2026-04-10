# syntax=docker/dockerfile:1.4
# ─── Stage 1: Build ──────────────────────────────────────────────────────────
FROM ocaml/opam:ubuntu-22.04-ocaml-5.3 AS builder

USER root
RUN apt-get update && apt-get install -y \
    build-essential curl bc git \
    libgmp-dev pkg-config \
    libssl-dev \
    nodejs \
    && rm -rf /var/lib/apt/lists/*

USER opam
WORKDIR /home/opam

# Rust is required by tree-sitter-cli during the r-parser build.
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs \
      | sh -s -- -y --no-modify-path
ENV PATH="/home/opam/.cargo/bin:${PATH}"

RUN opam update --yes && eval $(opam env) && \
    opam install ocamlfind dune cmdliner.1.0.4 --yes

# sstt and MLsem are now public — no PAT needed.
RUN eval $(opam env) && \
    opam pin add sstt      "git+https://github.com/E-Sh4rk/sstt.git"   --yes && \
    opam pin add sstt-repl "git+https://github.com/E-Sh4rk/sstt.git"   --yes && \
    opam pin add sstt-bin  "git+https://github.com/E-Sh4rk/sstt.git"   --yes && \
    for pkg in mlsem-types mlsem-common mlsem-system mlsem-lang \
               mlsem mlsem-app mlsem-bin; do \
      opam pin add "$pkg" "git+https://github.com/E-Sh4rk/MLsem.git" --yes; \
    done && \
    opam pin add rstt      "git+https://github.com/E-Sh4rk/rstt.git" --yes && \
    opam pin add rstt-repl "git+https://github.com/E-Sh4rk/rstt.git" --yes && \
    opam pin add rstt-bin  "git+https://github.com/E-Sh4rk/rstt.git" --yes

# r-parser is private. The PAT is injected via a BuildKit secret:
#   • --mount=type=secret means it is never written to any image layer.
#   • We immediately overwrite the remote URL so the PAT is not stored in
#     .git/config inside the builder layer either.
RUN --mount=type=secret,id=pat,mode=0444 \
    PAT=$(cat /run/secrets/pat) && \
    git clone "https://${PAT}@github.com/E-Sh4rk/r-parser.git" r-parser && \
    git -C r-parser remote set-url origin https://github.com/E-Sh4rk/r-parser.git && \
    cd r-parser && \
    sed -i 's|git@github.com:|https://github.com/|' .gitmodules && \
    git submodule sync && \
    export CARGO_TARGET_DIR="$PWD/core/downloads/tree-sitter/target" && \
    opam exec -- bash -c "make update && make setup" && \
    opam exec -- bash -c "make" && \
    cd core && \
    opam exec -- bash -c "opam pin add tree-sitter . --kind=path --yes" && \
    opam exec -- dune install --prefix=$(opam var prefix)

ENV TREESITTER_INCDIR="/home/opam/r-parser/core/tree-sitter/include"
ENV TREESITTER_LIBDIR="/home/opam/r-parser/core/tree-sitter/lib"
ENV LD_LIBRARY_PATH="/home/opam/r-parser/core/tree-sitter/lib"

COPY --chown=opam:opam . /home/opam/r-c-typing/
WORKDIR /home/opam/r-c-typing
RUN eval $(opam env) && \
    opam install . --deps-only --yes && \
    dune build

# ─── Stage 2: Runtime ────────────────────────────────────────────────────────
# Only binary artifacts are copied here — no opam metadata, no source,
# no git history, no PAT can reach this stage.
FROM ubuntu:22.04

RUN apt-get update && apt-get install -y \
    libstdc++6 bc git \
    && rm -rf /var/lib/apt/lists/*

# Keep the _build/default/bin/main.exe path that run_one_package.sh expects.
RUN mkdir -p /checker/_build/default/bin
COPY --from=builder \
    /home/opam/r-c-typing/_build/default/bin/main.exe \
    /checker/_build/default/bin/main.exe

# Type definitions read at runtime relative to CHECKER_DIR (CWD of the checker).
COPY --from=builder /home/opam/r-c-typing/types /checker/types

# tree-sitter shared library.
COPY --from=builder \
    /home/opam/r-parser/core/tree-sitter/lib/libtree-sitter.so* \
    /usr/local/lib/
RUN ldconfig

# These variables are consumed by scripts/run_one_package.sh in r-typing.
ENV CHECKER_DIR=/checker
ENV CHECKER=/checker/_build/default/bin/main.exe
ENV TS_LIB_DIR=/usr/local/lib

WORKDIR /checker
