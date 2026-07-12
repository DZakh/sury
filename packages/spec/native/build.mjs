// Compiles callgrind.c into callgrind.node with a plain C compiler — no
// node-gyp, no python, no binding.gyp toolchain. The addon is a dev-only
// measurement aid, so this must NEVER fail `pnpm install`: any problem (no
// compiler, unusual Node layout) is logged and swallowed, and the harness
// treats a missing callgrind.node as "perf measurement unavailable here".
import { execFileSync } from "node:child_process";
import { existsSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));
const out = join(here, "callgrind.node");
const src = join(here, "callgrind.c");

// Node ships its own headers next to the executable (…/include/node), which is
// where node_api.h lives. node-gyp would download version-matched headers; for
// a same-machine dev tool the running Node's own headers are exactly right.
const nodeInclude = join(dirname(dirname(process.execPath)), "include", "node");

try {
  if (!existsSync(join(nodeInclude, "node_api.h")))
    throw new Error(`Node headers not found at ${nodeInclude}`);
  const cc = process.env.CC || "cc";
  const isMac = process.platform === "darwin";
  const args = [
    "-O2",
    "-fPIC",
    "-shared",
    `-I${nodeInclude}`,
    "-DNODE_GYP_MODULE_NAME=callgrind",
    // On macOS a bundle resolves N-API symbols from the host at load time.
    ...(isMac ? ["-undefined", "dynamic_lookup"] : []),
    "-o",
    out,
    src,
  ];
  execFileSync(cc, args, { stdio: "pipe" });
  console.log(`[spec] built ${out}`);
} catch (e) {
  console.warn(
    `[spec] callgrind addon not built (perf specs will be skipped): ${(e && e.message) || e}`,
  );
}
