// Vendors bufbuild/protobuf-conformance at the revision pinned in
// conformance-ref.json, the same way json-schema-test-suite vendors its
// upstream: one pinned commit fetched into a gitignored dir, exact by
// construction because no tag or branch can move under it.
//
// What comes from where, because it is two sources and the split is not
// obvious: the `.proto` corpus is checked in upstream, so the pinned commit is
// what our schema is written against. The `conformance_test_runner` binary is
// not - it is Google's C++ runner, published to npm by timostamm/protobuf-npm,
// and upstream depends on that same package. The tests themselves live inside
// that binary rather than in any file, so the runner version is as load-bearing
// as the commit.
import { execFileSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, rmSync } from "node:fs";
import { createRequire } from "node:module";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

export const PKG_DIR = dirname(fileURLToPath(import.meta.url));
export const UPSTREAM_DIR = join(PKG_DIR, ".upstream");

const ref = JSON.parse(readFileSync(join(PKG_DIR, "conformance-ref.json"), "utf8")) as {
  repository: string;
  commit: string;
};

export const UPSTREAM_COMMIT = ref.commit;

const git = (args: string[], cwd: string): string =>
  execFileSync("git", args, { cwd, encoding: "utf8", stdio: ["ignore", "pipe", "pipe"] }).trim();

const checkedOutCommit = (): string | null => {
  if (!existsSync(join(UPSTREAM_DIR, ".git"))) return null;
  try {
    return git(["rev-parse", "HEAD"], UPSTREAM_DIR);
  } catch {
    return null;
  }
};

// Returns the upstream dir, fetching it if the pinned commit isn't already
// there. `offlineOk` turns a failed fetch into null so callers can degrade to a
// skip-with-instructions instead of a stack trace.
export const ensureUpstream = ({ offlineOk = false } = {}): string | null => {
  if (checkedOutCommit() === UPSTREAM_COMMIT) return UPSTREAM_DIR;

  rmSync(UPSTREAM_DIR, { recursive: true, force: true });
  mkdirSync(UPSTREAM_DIR, { recursive: true });
  try {
    git(["init", "-q"], UPSTREAM_DIR);
    git(["remote", "add", "origin", ref.repository], UPSTREAM_DIR);
    git(["fetch", "-q", "--depth", "1", "origin", UPSTREAM_COMMIT], UPSTREAM_DIR);
    git(["checkout", "-q", "FETCH_HEAD"], UPSTREAM_DIR);
  } catch (error) {
    rmSync(UPSTREAM_DIR, { recursive: true, force: true });
    if (offlineOk) return null;
    throw new Error(
      `Failed to fetch ${ref.repository} at ${UPSTREAM_COMMIT}\n${(error as Error).message}`
    );
  }
  return UPSTREAM_DIR;
};

const require_ = createRequire(import.meta.url);

export const runnerVersion = (): string =>
  (JSON.parse(readFileSync(require_.resolve("protobuf-conformance/package.json"), "utf8")) as {
    version: string;
  }).version;

// The runner ships one prebuilt binary per platform. Only linux-x64 and
// darwin-x64 exist, which is the same pair upstream supports.
export const runnerBinary = (): string => {
  const pkg = dirname(require_.resolve("protobuf-conformance/package.json"));
  const path = join(pkg, "bin", `conformance_test_runner-${process.platform}-x64`);
  if (!existsSync(path)) {
    throw new Error(
      `protobuf-conformance ships no runner for ${process.platform}; Linux and macOS only`
    );
  }
  return path;
};
