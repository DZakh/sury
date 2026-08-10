// A consumer installs this package and ReScript execs the binary directly, so
// every slice has to run on a machine that has nothing on it but the OS. Only
// the linux slices get that for free (`--profile static`); on macOS OCaml 5.1+
// links libzstd into anything that uses compiler-libs — ppxlib does — and the
// path it records is the build runner's Homebrew prefix. 11.0.0-rc.1 shipped a
// macOS binary wanting /usr/local/opt/zstd/lib/libzstd.1.dylib, which no
// consumer has; nothing in the pipeline could see it, because a load command is
// invisible until dyld resolves it on someone else's machine.
//
// So the headers are parsed here rather than trusted: architecture, dynamic
// dependencies and the packaged tarball, all before publish.
import { execFileSync } from "node:child_process";
import { readFileSync, statSync } from "node:fs";
import { join } from "node:path";

// This list is spelled four times: here, `files` in package.json, `install.cjs`
// and the `bin` fallback script. Keeping the other three honest is this file's
// job — every name below must reach the tarball, executable, built for the
// architecture claimed.
const BINARIES = {
  "ppx-linux.exe": { format: "elf", arch: "x86-64" },
  "ppx-linux-arm.exe": { format: "elf", arch: "aarch64" },
  "ppx-osx.exe": { format: "macho", arch: "x86-64" },
  "ppx-osx-arm.exe": { format: "macho", arch: "arm64" },
  "ppx-windows.exe": { format: "pe", arch: "x86-64" },
} as const;

// `bin.cmd` is not here: install.cjs deletes it on Windows, and the tarball is
// checked before any install runs.
const EXECUTABLE_IN_TARBALL = ["bin", ...Object.keys(BINARIES)];
const ALSO_IN_TARBALL = ["bin.cmd", "install.cjs"];

// Everything else has to ship beside the binary, and nothing does.
const MACOS_SYSTEM_PREFIXES = ["/usr/lib/", "/System/Library/"];

// The mingw-w64 build imports only DLLs Windows itself provides. `api-ms-win-*`
// are the API sets; the rest are named individually so a runtime DLL that has
// to be shipped (libwinpthread-1, libzstd) can't hide among them.
const WINDOWS_SYSTEM_DLLS = new Set(
  [
    "kernel32.dll",
    "msvcrt.dll",
    "shell32.dll",
    "ole32.dll",
    "shlwapi.dll",
    "version.dll",
    "ws2_32.dll",
    "advapi32.dll",
    "user32.dll",
    "dbghelp.dll",
    "userenv.dll",
  ].map((n) => n.toLowerCase()),
);

type Inspection = { arch: string; deps: string[] };

const ELF_MACHINES: Record<number, string> = { 0x3e: "x86-64", 0xb7: "aarch64" };

// A statically linked ELF has no PT_INTERP: no loader runs, so there is nothing
// to resolve. That single check is the whole "no shared libraries" question -
// the DT_NEEDED list can't be non-empty without an interpreter to read it.
const inspectElf = (buf: Buffer): Inspection => {
  if (buf.readUInt32BE(0) !== 0x7f454c46) {
    throw new Error("missing ELF magic");
  }
  if (buf[4] !== 2 || buf[5] !== 1) {
    throw new Error("not a little-endian 64-bit ELF");
  }
  const phoff = Number(buf.readBigUInt64LE(32));
  const phentsize = buf.readUInt16LE(54);
  const deps: string[] = [];
  for (let i = 0; i < buf.readUInt16LE(56); i++) {
    const ph = phoff + i * phentsize;
    if (buf.readUInt32LE(ph) === 3 /* PT_INTERP */) {
      const offset = Number(buf.readBigUInt64LE(ph + 8));
      const size = Number(buf.readBigUInt64LE(ph + 32));
      deps.push(
        buf
          .subarray(offset, offset + size)
          .toString("utf8")
          .replace(/\0.*$/, ""),
      );
    }
  }
  return { arch: ELF_MACHINES[buf.readUInt16LE(18)] ?? "unknown", deps };
};

const MACHO_CPUS: Record<number, string> = {
  0x01000007: "x86-64",
  0x0100000c: "arm64",
};

// LC_REQ_DYLD (0x80000000) is set on the weak/reexport/upward variants, and the
// lazy one defers the load rather than dropping it; every one of the five names
// a path dyld has to find on the consumer's machine, so all five count.
const MACHO_DYLIB_COMMANDS = new Set([
  0x0c, 0x20, 0x80000018, 0x8000001f, 0x80000023,
]);

const inspectMacho = (buf: Buffer): Inspection => {
  if (buf.readUInt32LE(0) === 0xbebafeca) {
    throw new Error("universal binary: the slices are shipped separately");
  }
  if (buf.readUInt32LE(0) !== 0xfeedfacf) {
    throw new Error("missing 64-bit Mach-O magic");
  }
  const deps: string[] = [];
  let cursor = 32;
  for (let i = 0; i < buf.readUInt32LE(16); i++) {
    const cmd = buf.readUInt32LE(cursor);
    const cmdsize = buf.readUInt32LE(cursor + 4);
    if (MACHO_DYLIB_COMMANDS.has(cmd)) {
      const nameOffset = cursor + buf.readUInt32LE(cursor + 8);
      deps.push(
        buf
          .subarray(nameOffset, cursor + cmdsize)
          .toString("utf8")
          .replace(/\0.*$/, ""),
      );
    }
    cursor += cmdsize;
  }
  return { arch: MACHO_CPUS[buf.readUInt32LE(4)] ?? "unknown", deps };
};

const PE_MACHINES: Record<number, string> = { 0x8664: "x86-64" };

const inspectPe = (buf: Buffer): Inspection => {
  const pe = buf.readUInt32LE(0x3c);
  if (buf.toString("ascii", pe, pe + 4) !== "PE\0\0") {
    throw new Error("missing PE signature");
  }
  const opt = pe + 24;
  if (buf.readUInt16LE(opt) !== 0x20b) {
    throw new Error("not a PE32+ image");
  }
  const sections = pe + 24 + buf.readUInt16LE(pe + 20);
  const toOffset = (rva: number): number => {
    for (let i = 0; i < buf.readUInt16LE(pe + 6); i++) {
      const s = sections + i * 40;
      const start = buf.readUInt32LE(s + 12);
      if (rva >= start && rva < start + buf.readUInt32LE(s + 16)) {
        return rva - start + buf.readUInt32LE(s + 20);
      }
    }
    throw new Error(`RVA ${rva} is outside every section`);
  };
  const name = (rva: number): string => {
    const at = toOffset(rva);
    return buf.toString("ascii", at, buf.indexOf(0, at));
  };

  const deps: string[] = [];
  // Data directory 1 is the import table, 13 the delay-load imports; the entry
  // stride and the offset of the DLL name differ between the two.
  const dirs = opt + 112;
  for (const [index, stride, nameField] of [
    [1, 20, 12],
    [13, 32, 4],
  ] as const) {
    if (buf.readUInt32LE(opt + 108) <= index) continue;
    const rva = buf.readUInt32LE(dirs + index * 8);
    if (!rva) continue;
    for (let at = toOffset(rva); ; at += stride) {
      const nameRva = buf.readUInt32LE(at + nameField);
      if (!nameRva) break;
      deps.push(name(nameRva));
    }
  }
  return { arch: PE_MACHINES[buf.readUInt16LE(pe + 4)] ?? "unknown", deps };
};

const isSystemDep = (format: string, dep: string): boolean =>
  format === "macho"
    ? MACOS_SYSTEM_PREFIXES.some((prefix) => dep.startsWith(prefix))
    : format === "pe" &&
      (dep.toLowerCase().startsWith("api-ms-win-") ||
        WINDOWS_SYSTEM_DLLS.has(dep.toLowerCase()));

const errors: string[] = [];
const fail = (message: string) => {
  errors.push(message);
};

const dir = import.meta.dirname;

for (const [file, expected] of Object.entries(BINARIES)) {
  const path = join(dir, file);
  let buf: Buffer;
  try {
    buf = readFileSync(path);
  } catch {
    fail(`${file}: missing — the pack step moves one artifact per build slice`);
    continue;
  }

  // install.cjs chmods the binary it picks, but an installer that skips
  // postinstall (pnpm >=10 by default) falls through to the `bin` script, which
  // execs it as-is.
  if (!(statSync(path).mode & 0o111)) {
    fail(`${file}: not executable`);
  }

  let inspection: Inspection;
  try {
    inspection =
      expected.format === "elf"
        ? inspectElf(buf)
        : expected.format === "macho"
          ? inspectMacho(buf)
          : inspectPe(buf);
  } catch (error) {
    fail(`${file}: not a readable ${expected.format} image — ${error}`);
    continue;
  }

  if (inspection.arch !== expected.arch) {
    // A runner label silently changing architecture is how #367 shipped.
    fail(`${file}: built for ${inspection.arch}, expected ${expected.arch}`);
  }
  for (const dep of inspection.deps) {
    if (!isSystemDep(expected.format, dep)) {
      fail(`${file}: depends on ${dep}, which a consumer has no reason to have`);
    }
  }
}

const [tarball] = process.argv.slice(2);
if (!tarball) {
  fail("usage: checkBinaries.ts <sury-ppx-*.tgz>");
} else {
  const listing = execFileSync("tar", ["-tvzf", tarball], { encoding: "utf8" })
    .split("\n")
    .flatMap((line) => {
      const match = line.match(/^(\S+).* package\/(\S+)$/);
      return match ? [{ mode: match[1]!, file: match[2]! }] : [];
    });
  for (const file of [...EXECUTABLE_IN_TARBALL, ...ALSO_IN_TARBALL]) {
    const entry = listing.find((e) => e.file === file);
    if (!entry) {
      fail(`${file}: missing from the tarball — add it to package.json "files"`);
    } else if (
      EXECUTABLE_IN_TARBALL.includes(file) &&
      !entry.mode.startsWith("-rwx")
    ) {
      fail(`${file}: not executable in the tarball`);
    }
  }
}

if (errors.length) {
  for (const error of errors) {
    console.error(`${process.env.GITHUB_ACTIONS ? "::error::" : "error: "}${error}`);
  }
  process.exit(1);
}
console.log(`sury-ppx binaries verified: ${Object.keys(BINARIES).join(", ")}`);
