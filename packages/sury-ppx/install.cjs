const path = require("path");
const fs = require("fs");

// Rosetta translates x64 -> arm64, never the reverse, so an Apple Silicon build
// can't stand in for the Intel one - each slice ships separately. Windows on ARM
// emulates x64, so it shares the one Windows binary.
//
// This list is spelled four times: here, `files` in package.json, the `bin`
// fallback script, and checkBinaries.ts. That last one runs before publish and
// asserts every entry reached the tarball, executable, built for the right
// architecture and needing nothing but the OS, so drift fails CI rather than
// reaching a user.
const BINARIES = {
  "linux-x64": "ppx-linux.exe",
  "linux-arm64": "ppx-linux-arm.exe",
  "darwin-x64": "ppx-osx.exe",
  "darwin-arm64": "ppx-osx-arm.exe",
  "win32-x64": "ppx-windows.exe",
  "win32-arm64": "ppx-windows.exe",
};

const target = `${process.platform}-${process.arch}`;
const binary = BINARIES[target];

if (!binary) {
  // This won't break the installation because the `bin` shell script remains
  // but that script will throw an error in this case anyway
  console.warn(`No release available for "${target}"`);
  process.exit(1);
}

const source = path.join(__dirname, binary);

if (!fs.existsSync(source)) {
  // assume we're in dev mode - nothing will break if the script
  // isn't overwritten, it will just be slower
} else if (process.platform === "win32") {
  fs.renameSync(source, path.join(__dirname, "bin.exe"));

  // windows scripts use a different file extension to executables
  // so we delete the script to make sure windows uses the exe now.
  // force: a re-run after a partial extract finds it already gone.
  fs.rmSync(path.join(__dirname, "bin.cmd"), { force: true });
} else {
  // mac and linux support extension-less executables
  // so just overwrite the shell script
  const bin = path.join(__dirname, "bin");
  fs.renameSync(source, bin);

  // The ppx should be executable in the bundle, but just in case
  fs.chmodSync(bin, 0o777);
}
