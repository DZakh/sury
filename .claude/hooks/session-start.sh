#!/bin/bash
# SessionStart hook for Claude Code on the web.
#
# Builds the `sury-ppx` ReScript PPX from its OCaml source so that the e2e
# package (which uses `ppx-flags: ["sury-ppx/bin"]`) can compile. The published
# npm binary lags behind the source on feature branches, so the only reliable
# way to run the current ppx is to build it from source. We use opam with a
# system OCaml switch (apt's 4.14.1, supported by the pinned ppxlib 0.34.0),
# which avoids compiling the compiler from scratch.
set -euo pipefail

# Only run in the remote (web) environment; local machines manage their own
# toolchain.
if [ "${CLAUDE_CODE_REMOTE:-}" != "true" ]; then
  exit 0
fi

ROOT="${CLAUDE_PROJECT_DIR:-$(pwd)}"
PPX_SRC="$ROOT/packages/sury-ppx/src"
SWITCH="sury-ppx"

SUDO=""
if [ "$(id -u)" -ne 0 ]; then SUDO="sudo"; fi

# 1. JavaScript workspace dependencies (rescript, ava, sury, sury-ppx, ...).
if command -v pnpm >/dev/null 2>&1; then
  (cd "$ROOT" && pnpm install)
fi

# 2. opam + a system OCaml compiler (needed to build the ppx).
if ! command -v opam >/dev/null 2>&1 || ! command -v ocaml >/dev/null 2>&1; then
  $SUDO apt-get update
  $SUDO DEBIAN_FRONTEND=noninteractive apt-get install -y opam ocaml
fi

# 3. opam init + a system switch (re-uses apt's OCaml, no compiler build).
export OPAMYES=1
export OPAMROOT="${OPAMROOT:-$HOME/.opam}"
if [ ! -d "$OPAMROOT" ]; then
  opam init --bare --disable-sandboxing -y
fi
if ! opam switch list --short 2>/dev/null | grep -qx "$SWITCH"; then
  opam switch create "$SWITCH" ocaml-system -y --no-install
fi
eval "$(opam env --switch="$SWITCH")"

# 4. Build dependencies for the ppx (ppxlib 0.34.0, dune). Idempotent.
opam install "$PPX_SRC" --deps-only -y

# 5. Build the dev ppx binary that `packages/sury-ppx/bin` prefers at runtime.
(cd "$PPX_SRC" && dune build)

# 6. Persist the opam environment so rebuilding the ppx (dune) works later in
#    the session without re-running this hook.
if [ -n "${CLAUDE_ENV_FILE:-}" ]; then
  opam env --switch="$SWITCH" >> "$CLAUDE_ENV_FILE"
fi
