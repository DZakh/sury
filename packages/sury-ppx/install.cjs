const path = require("path");
const fs = require("fs");

const installMacLinuxBinary = (binary) => {
  const source = path.join(__dirname, binary);
  if (fs.existsSync(source)) {
    // mac and linux support extension-less executables
    // so just overwrite the shell script
    const target = path.join(__dirname, "bin");
    fs.renameSync(source, target);

    // The ppx should be executable in the bundle, but just in case
    fs.chmodSync(target, 0o777);
  } else {
    // assume we're in dev mode - nothing will break if the script
    // isn't overwritten, it will just be slower
  }
};

const installWindowsBinary = () => {
  const source = path.join(__dirname, "ppx-windows.exe");
  if (fs.existsSync(source)) {
    const target = path.join(__dirname, "bin.exe");
    fs.renameSync(source, target);

    // windows scripts use a different file extension to executables
    // so we delete the script to make sure windows uses the exe now
    const windowsScript = path.join(__dirname, "bin.cmd");
    fs.unlinkSync(windowsScript);
  } else {
    // assume we're in dev mode - nothing will break if the script
    // isn't overwritten, it will just be slower
  }
};

switch (process.platform) {
  case "linux":
    installMacLinuxBinary("ppx-linux.exe");
    break;
  case "darwin":
    const binaryName =
      process.arch === "x64" ? "ppx-osx-x64.exe" : "ppx-osx.exe";
    installMacLinuxBinary(binaryName);
  case "win32":
    installWindowsBinary();
    break;
  default:
    // This won't break the installation because the `bin` shell script remains
    // but that script will throw an error in this case anyway
    console.warn(`No release available for "${process.platform}"`);
    process.exit(1);
}
