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

skip() {
  results+=("SKIP  $1 ($2)")
}

if [ $FAST -eq 0 ]; then
  step "dead code" . pnpm lint:deadcode
  step "build" packages/sury pnpm build
  step "compiled ReScript matches source" packages/sury ../../scripts/assert-no-drift.sh '*.res.mjs'
fi

step "spec check" packages/sury pnpm spec check --perf=skip
step "typecheck" packages/sury pnpm typecheck
step "fuzz:union" packages/sury pnpm fuzz:union --seed=1
step "fuzz:schema" packages/sury pnpm fuzz:schema
step "fuzz:formdata" packages/sury pnpm fuzz:formdata
step "fuzz:content" packages/sury pnpm fuzz:content

if [ $FAST -eq 0 ]; then
  step "coverage" packages/sury pnpm coverage
  step "fuzz:escfree" packages/sury pnpm fuzz:escfree
  step "jsr dry run" packages/sury/artifacts npx --yes jsr@0.14.3 publish --dry-run --allow-dirty
  step "benchmarks" . pnpm benchmarks
  step "JSON Schema compliance" . pnpm compliance
  step "protobuf compliance" . pnpm protobuf:compliance
  step "protobuf fuzz" . pnpm protobuf:fuzz
  step "protobuf conformance" . pnpm protobuf:conformance
  # CI builds the ppx in a separate job; here only the session hook's dune
  # build can supply it.
  if [ -d "$ROOT/packages/sury-ppx/src/_build/default" ]; then
    step "e2e" packages/e2e sh -c "pnpm rescript && pnpm test"
    step "e2e compiled ReScript matches source" packages/e2e ../../scripts/assert-no-drift.sh '*.res.mjs'
    # e2e's build compiles sury as a dependency, which deletes sury's checked-in
    # dev-only output (tests/*.res.mjs). CI runs e2e in its own job and never
    # sees it; here it would land in the next commit.
    (cd "$ROOT/packages/sury" && pnpm rescript > /dev/null)
  else
    skip "e2e" "no ppx build; run pnpm --filter=e2e ppx:build"
  fi
fi

echo
echo "=== verify$([ $FAST -eq 1 ] && echo ' --fast') summary"
printf '%s\n' "${results[@]}"
exit $failed
