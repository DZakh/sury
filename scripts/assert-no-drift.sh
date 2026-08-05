#!/usr/bin/env bash
#
# Fail if the matched files differ from the commit, after the caller has already
# rebuilt whatever generates them.
#
# Used to gate checked-in compiled ReScript (*.res.mjs). Those files are only
# ever read by someone installing the published package: every job here
# recompiles them before use, so nothing else in CI can notice that the commit
# and the source disagree.
#
# Usage: scripts/assert-no-drift.sh <pathspec>...   (relative to $PWD)
#
# Pass a pathspec matching the generated files, not the directory holding them:
# scoped to a directory this also reports hand-edited sources, which makes it
# useless to run locally against work in progress.

set -euo pipefail

# --porcelain rather than `git diff --exit-code`: a generator that starts
# emitting a *new* file leaves it untracked, which a diff of tracked paths
# reports as clean.
drift=$(git status --porcelain -- "$@")

if [ -z "$drift" ]; then
  echo "up to date: $*"
  exit 0
fi

echo "Generated files matching '$*' do not match their source."
echo "The build regenerated these; commit the result."
echo
echo "$drift"
echo
git --no-pager diff -- "$@"
exit 1
