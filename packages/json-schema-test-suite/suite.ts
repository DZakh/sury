// Vendors the upstream test suite at the revision pinned in suite-ref.json.
//
// A git submodule would tax every clone and CI checkout with `--recursive`,
// and the `@json-schema-org/tests` npm mirror is archived and lags upstream —
// so fetch the one pinned commit into a gitignored dir instead. GitHub serves
// `fetch --depth 1 <sha>`, which makes this exact-by-construction: no tag or
// branch can move under us.
import { execFileSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, rmSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

export const PKG_DIR = dirname(fileURLToPath(import.meta.url));
export const SUITE_DIR = join(PKG_DIR, ".suite");

const ref = JSON.parse(readFileSync(join(PKG_DIR, "suite-ref.json"), "utf8")) as {
  repository: string;
  commit: string;
};

export const SUITE_COMMIT = ref.commit;

const git = (args: string[], cwd: string): string =>
  execFileSync("git", args, { cwd, encoding: "utf8", stdio: ["ignore", "pipe", "pipe"] }).trim();

const checkedOutCommit = (): string | null => {
  if (!existsSync(join(SUITE_DIR, ".git"))) return null;
  try {
    return git(["rev-parse", "HEAD"], SUITE_DIR);
  } catch {
    return null;
  }
};

// Returns the suite dir, fetching it if the pinned commit isn't already there.
// `offlineOk` turns a failed fetch into null so callers can degrade to a
// skip-with-instructions instead of a stack trace on a plane.
export const ensureSuite = ({ offlineOk = false } = {}): string | null => {
  if (checkedOutCommit() === SUITE_COMMIT) return SUITE_DIR;

  rmSync(SUITE_DIR, { recursive: true, force: true });
  mkdirSync(SUITE_DIR, { recursive: true });
  try {
    git(["init", "-q"], SUITE_DIR);
    git(["remote", "add", "origin", ref.repository], SUITE_DIR);
    git(["fetch", "-q", "--depth", "1", "origin", SUITE_COMMIT], SUITE_DIR);
    git(["checkout", "-q", "FETCH_HEAD"], SUITE_DIR);
  } catch (error) {
    rmSync(SUITE_DIR, { recursive: true, force: true });
    if (offlineOk) return null;
    throw new Error(
      `Failed to fetch ${ref.repository} at ${SUITE_COMMIT}\n${(error as Error).message}`
    );
  }
  return SUITE_DIR;
};
