#!/usr/bin/env bash
#
# The local mirror of .github/workflows/ci.yml, so a run that is green here is
# green there. Keep the steps in step with that file.
#
# Usage: pnpm verify [--fast]
#   --fast  the inner-loop tier: specs, typecheck and the quick fuzzers.
#
# Every step runs even after one fails, so a single run shows everything that
# is red. Exits non-zero if any step failed.

set -uo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
FAST=0
[ "${1:-}" = "--fast" ] && FAST=1

results=()
failed=0

step() {
  local name="$1" dir="$2"
  shift 2
  echo
  echo "=== $name"
  local start=$SECONDS
  if (cd "$ROOT/$dir" && "$@"); then
    results+=("PASS  $name ($((SECONDS - start))s)")
  else
    results+=("FAIL  $name ($((SECONDS - start))s)")
    failed=1
  fi
}

# Everything between `spawn` and `collect` runs concurrently: a step added
# there must share no files with the others, so nothing in it may write
# index.mjs, which the fuzzers import.
LOGS="$(mktemp -d)"
trap 'rm -rf "$LOGS"' EXIT
spawned=()

spawn() {
  local name="$1" dir="$2" log="$LOGS/${#spawned[@]}"
  shift 2
  spawned+=("$name")
  (
    start=$SECONDS
    (cd "$ROOT/$dir" && "$@") >"$log" 2>&1
    echo "$? $((SECONDS - start))" >"$log.rc"
  ) &
}

collect() {
  wait
  local i rc secs
  for i in "${!spawned[@]}"; do
    echo
    echo "=== ${spawned[$i]}"
    cat "$LOGS/$i"
    read -r rc secs <"$LOGS/$i.rc"
    if [ "${rc:-1}" -eq 0 ]; then
      results+=("PASS  ${spawned[$i]} (${secs}s)")
    else
      results+=("FAIL  ${spawned[$i]} (${secs}s)")
      failed=1
    fi
  done
  spawned=()
}

if [ $FAST -eq 0 ]; then
  step "dead code" . pnpm lint:deadcode
  step "build" packages/sury pnpm build
  step "compiled ReScript matches source" packages/sury ../../scripts/assert-no-drift.sh '*.res.mjs'
fi

step "build entry" packages/sury pnpm build:entry
spawn "spec check" packages/sury pnpm exec tsx ../spec/cli.ts check --perf=skip
spawn "typecheck" packages/sury pnpm typecheck
spawn "fuzz:union" packages/sury pnpm fuzz:union --seed=1
spawn "fuzz:schema" packages/sury pnpm fuzz:schema
spawn "fuzz:formdata" packages/sury pnpm fuzz:formdata
spawn "fuzz:content" packages/sury pnpm fuzz:content
collect

if [ $FAST -eq 0 ]; then
  step "coverage" packages/sury pnpm coverage
  step "fuzz:escfree" packages/sury pnpm fuzz:escfree
  step "jsr dry run" packages/sury/artifacts npx --yes jsr@0.14.3 publish --dry-run --allow-dirty
  step "benchmarks" . pnpm benchmarks
  step "JSON Schema compliance" . pnpm compliance
  step "protobuf compliance" . pnpm protobuf:compliance
  step "protobuf fuzz" . pnpm protobuf:fuzz
  step "protobuf conformance" . pnpm protobuf:conformance
  step "protobuf conformance (generated schema)" . pnpm protobuf:conformance:generated
  step "protoc-gen-sury" . pnpm protobuf:codegen
  # Needs dune, which the session hook installs.
  step "ppx build" packages/sury-ppx/src dune build
  # Clean first: sury's own build above leaves artifacts the e2e build, which
  # compiles sury as a dependency, rejects as inconsistent.
  step "e2e" packages/e2e sh -c "pnpm rescript clean && pnpm rescript && pnpm test"
  step "e2e compiled ReScript matches source" packages/e2e ../../scripts/assert-no-drift.sh '*.res.mjs'
  # e2e's build compiles sury as a dependency, which deletes sury's checked-in
  # dev-only output (tests/*.res.mjs). CI runs e2e in its own job and never
  # sees it; here it would land in the next commit.
  step "restore sury compiled ReScript" packages/sury pnpm rescript
fi

echo
echo "=== verify$([ $FAST -eq 1 ] && echo ' --fast') summary"
printf '%s\n' "${results[@]}"
exit $failed
