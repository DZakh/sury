// protoc-gen-sury as protoc and buf run it: a CodeGeneratorRequest on stdin, a
// CodeGeneratorResponse on stdout. Given `--descriptor_set_in` it reads a
// FileDescriptorSet (what `buf build -o` and `protoc -o` write) instead and
// writes the files itself.
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { createRequire } from "node:module";
import { dirname, join } from "node:path";
import { decodeFileDescriptorSet, decodeRequest, encodeResponse } from "./descriptor";
import { generate } from "./plugin";

const version = (createRequire(import.meta.url)("sury/package.json") as { version: string }).version;

const flag = (name: string): string | undefined => {
  const prefix = `--${name}=`;
  return process.argv.find((arg) => arg.startsWith(prefix))?.slice(prefix.length);
};

const setIn = flag("descriptor_set_in");
if (setIn !== undefined) {
  const out = flag("out") ?? ".";
  const set = decodeFileDescriptorSet(new Uint8Array(readFileSync(setIn)));
  const files = flag("files")?.split(",") ?? set.file.map((file) => file.name).filter((name) => !name.startsWith("google/protobuf/"));
  const response = generate({ fileToGenerate: files, parameter: flag("opt"), protoFile: set.file }, version);
  if (response.error !== undefined) {
    process.stderr.write(`protoc-gen-sury: ${response.error}\n`);
    process.exit(1);
  }
  for (const file of response.file) {
    const path = join(out, file.name);
    mkdirSync(dirname(path), { recursive: true });
    writeFileSync(path, file.content);
  }
} else {
  process.stdout.write(encodeResponse(generate(decodeRequest(new Uint8Array(readFileSync(0))), version)));
}
