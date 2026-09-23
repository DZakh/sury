// The generator's gates, over the corpus in test/proto:
//
//   goldens     the generated files, committed under test/generated, and
//               sury's own src/wkt
//   compile     tsc over what TypeScript generation wrote, which is where
//               S.schemaOf holds each schema equal to the type beside it
//   types       every message and enum type equal to protoc-gen-es's for the
//               same file, minus the differences README.md lists
//   values      values drawn from each descriptor, encoded by one side and
//               decoded by the other, both ways, and the bytes compared
//   reprint     S.toProtoOrThrow of each message, compiled again, has the
//               fields the source declared: number, label and type
//
//   pnpm test           check
//   pnpm test update    rewrite the goldens
import { execFileSync } from "node:child_process";
import { existsSync, mkdirSync, mkdtempSync, readdirSync, readFileSync, rmSync, statSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, relative } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import { decodeFileDescriptorSet, type FileDescriptorProto } from "../src/descriptor";
import { buildRegistry, type Message, nestedTypes } from "../src/model";
import { namesOf } from "../src/names";
import { fieldNames, fileModule, moduleOf } from "../src/res";
import { isStruct, wrapperScalar } from "../src/shared";
import { generate } from "../src/plugin";
import { generateWkt, generateWktRes } from "../scripts/wkt";
import { normalize, rngOf, sampleMessage } from "./sample";

const root = join(dirname(fileURLToPath(import.meta.url)), "..");
const bin = (name: string) => join(root, "node_modules/.bin", name);
const update = process.argv[2] === "update";
const failures: string[] = [];
const fail = (message: string) => failures.push(message);

const listFiles = (dir: string): string[] =>
  existsSync(dir)
    ? readdirSync(dir).flatMap((name) => (statSync(join(dir, name)).isDirectory() ? listFiles(join(dir, name)).map((f) => `${name}/${f}`) : [name]))
    : [];

const holdGoldens = (dir: string, files: { name: string; content: string }[], label: string) => {
  if (update) {
    rmSync(dir, { recursive: true, force: true });
    for (const file of files) {
      mkdirSync(dirname(join(dir, file.name)), { recursive: true });
      writeFileSync(join(dir, file.name), file.content);
    }
    return;
  }
  const expected = new Map(files.map((file) => [file.name, file.content]));
  for (const name of listFiles(dir)) if (!expected.has(name) && !name.endsWith(".res.mjs")) fail(`${label}: ${name} is no longer generated`);
  for (const [name, content] of expected) {
    const path = join(dir, name);
    if (!existsSync(path)) fail(`${label}: ${name} is missing`);
    else if (readFileSync(path, "utf8") !== content) fail(`${label}: ${name} differs from what the generator writes`);
  }
};

// One generated file among hand-written ones: rewritten or compared on its own.
const holdFile = (path: string, content: string, label: string) => {
  if (update) writeFileSync(path, content);
  else if (!existsSync(path) || readFileSync(path, "utf8") !== content) fail(`${label}: ${path} differs from what the generator writes`);
};

// A value in the shape the ReScript schemas hold, and back: an unset oneof is
// `undefined` there.
const mapValues = (value: unknown, fn: (v: unknown) => unknown): unknown =>
  Array.isArray(value) ? value.map(fn)
  : value !== null && typeof value === "object" && !(value instanceof Uint8Array)
    ? Object.fromEntries(Object.entries(value).map(([k, v]) => [k, fn(v)]))
    : value;
// ...and each field under its ReScript label.
const toRes = (message: Message, value: Record<string, unknown>): Record<string, unknown> => {
  const names = fieldNames(message);
  const out: Record<string, unknown> = {};
  const element = (field: Message["fields"][number], item: unknown): unknown =>
    field.element.kind === "message" && !isStruct(field, field.element) && item !== undefined
      ? toRes(field.element.message, item as Record<string, unknown>)
      : item;
  for (const member of message.members) {
    const item = value[member.localName];
    if (!(member.localName in value)) continue;
    if (member.kind === "oneof") {
      const chosen = member.fields.find((field) => field.localName === (item as { case?: string }).case);
      out[names.get(member)!] = chosen === undefined ? undefined : { case: chosen.localName, value: element(chosen, (item as { value: unknown }).value) };
    } else if (member.mapKey !== undefined) {
      out[names.get(member)!] = mapValues(item, (v) => element(member, v));
    } else if (member.list) {
      out[names.get(member)!] = (item as unknown[]).map((v) => element(member, v));
    } else if (wrapperScalar(member) !== undefined) {
      out[names.get(member)!] = item;
    } else out[names.get(member)!] = element(member, item);
  }
  return out;
};

const work = mkdtempSync(join(tmpdir(), "protoc-gen-sury-"));
try {
  // ── corpus ───────────────────────────────────────────────────────────────
  execFileSync(bin("buf"), ["build", join(root, "test"), "-o", join(work, "set.binpb")], { stdio: ["ignore", "ignore", "inherit"] });
  const set = decodeFileDescriptorSet(new Uint8Array(readFileSync(join(work, "set.binpb"))));
  const corpus = set.file.filter((file) => !file.name.startsWith("google/"));
  const run = (parameter: string, files: FileDescriptorProto[] = corpus) => {
    const response = generate({ fileToGenerate: files.map((f) => f.name), parameter, protoFile: set.file }, "");
    if (response.error !== undefined) throw new Error(response.error);
    return response.file;
  };

  // ── goldens ──────────────────────────────────────────────────────────────
  holdGoldens(join(root, "test/generated/ts"), run("target=ts"), "ts golden");
  holdGoldens(join(root, "test/generated/res"), run("target=res"), "res golden");
  holdGoldens(join(root, "../sury/src/wkt"), generateWkt(), "sury/wkt");
  holdFile(join(root, "../sury/src/SuryProtobuf.res"), generateWktRes(), "SuryProtobuf");

  // ── protoc-gen-es, the reference ─────────────────────────────────────────
  const es = join(root, "test/.es");
  rmSync(es, { recursive: true, force: true });
  writeFileSync(
    join(work, "buf.gen.yaml"),
    `version: v2\nplugins:\n  - local: ${bin("protoc-gen-es")}\n    out: ${es}\n    opt: [target=ts, erasable_syntax=true]\n`,
  );
  execFileSync(bin("buf"), ["generate", join(root, "test"), "--template", join(work, "buf.gen.yaml")], { stdio: "inherit" });

  // ── types ────────────────────────────────────────────────────────────────
  const registry = buildRegistry(set.file);
  const messages: { file: string; message: Message; name: string }[] = [];
  let typeTest = `// Generated by test/cli.ts: protoc-gen-es's type for every message and enum,
// with what the README lists as differences normalized, equal to ours.
import type { JsonObject, UnknownEnum } from "@bufbuild/protobuf";
type Own<T> = { [K in keyof T as K extends "$typeName" | "$unknown" ? never : K]: T[K] };
type IsJsonObject<T> = [T] extends [JsonObject] ? ([JsonObject] extends [T] ? true : false) : false;
type Normalize<T> = T extends UnknownEnum ? never
  // protobuf-es's own well-known types keep TS enums, whose members are the
  // numbers ours are.
  : T extends number ? (\`\${T}\` extends \`\${infer N extends number}\` ? N : T)
  : T extends Uint8Array | bigint | string | boolean | undefined | null ? T
  : T extends readonly (infer U)[] ? Normalize<U>[]
  : IsJsonObject<Own<T>> extends true ? "JsonObject"
  : T extends { case: undefined; value?: undefined } ? { case?: undefined; value?: undefined }
  : { [K in keyof Own<T> as K extends number ? string : K]: Normalize<Own<T>[K]> };
type Equal<A, B> = (<V>() => V extends A ? 1 : 2) extends <V>() => V extends B ? 1 : 2 ? true : false;
`;
  for (const proto of corpus) {
    const file = registry.files.get(proto.name)!;
    const path = `./${file.name}_pb`;
    const alias = file.name.replace(/[^A-Za-z0-9]/g, "_");
    typeTest += `import type * as es_${alias} from "./.es/${file.name}_pb";\nimport type * as sury_${alias} from "./generated/ts/${file.name}_pb";\n`;
    for (const desc of nestedTypes(file)) {
      const name = namesOf(file).shape.get(desc)!;
      if (desc.kind === "message") messages.push({ file: path, message: desc, name: namesOf(file).schema.get(desc)! });
      typeTest += `export const ${alias}_${name}: Equal<Normalize<es_${alias}.${name}>, Normalize<sury_${alias}.${name}>> = true;\n`;
    }
  }
  writeFileSync(join(root, "test/types.generated.ts"), typeTest);
  try {
    execFileSync(bin("tsc"), ["-p", join(root, "test/tsconfig.json")], { stdio: "pipe" });
  } catch (error) {
    fail(`compile and types:\n${(error as { stdout: Buffer }).stdout.toString()}`);
  }
  try {
    execFileSync(bin("rescript"), [], { cwd: join(root, "test"), stdio: "pipe" });
  } catch (error) {
    const { stdout, stderr } = error as { stdout: Buffer; stderr: Buffer };
    fail(`rescript:\n${stdout.toString()}${stderr.toString()}`);
  }

  // ── values ───────────────────────────────────────────────────────────────
  const S = await import("sury");
  const { create, fromBinary, toBinary } = await import("@bufbuild/protobuf");
  const rng = rngOf(Number(process.env.SEED ?? 1));
  let cases = 0;
  let byteDiffs = 0;
  for (const { file, message, name } of messages) {
    const esModule = await import(pathToFileURL(join(es, `${file}.ts`)).href);
    const suryModule = await import(pathToFileURL(join(root, "test/generated/ts", `${file}.ts`)).href);
    const esSchema = esModule[name];
    const surySchema = suryModule[name];
    // The ReScript file compiles to the same runtime values, a oneof with no
    // member set aside: `None` there, `{ case: undefined }` here.
    const dir = message.file.name.includes("/") ? message.file.name.slice(0, message.file.name.lastIndexOf("/") + 1) : "";
    const resModule = await import(pathToFileURL(join(root, "test/generated/res", `${dir}${fileModule(message.file)}.res.mjs`)).href);
    const resSchema = resModule[moduleOf(message)].schema;
    const resEncode = S.decodeOrThrow(resSchema, S.protobuf) as (value: unknown) => Uint8Array;
    const resDecode = S.decodeOrThrow(S.protobuf, resSchema) as (bytes: Uint8Array) => unknown;
    const encode = S.decodeOrThrow(surySchema, S.protobuf) as (value: unknown) => Uint8Array;
    const decode = S.decodeOrThrow(S.protobuf, surySchema) as (bytes: Uint8Array) => unknown;
    for (let idx = 0; idx < 60; idx++) {
      const value = sampleMessage(rng, message);
      const label = `values: ${message.typeName} #${idx} ${JSON.stringify(normalize(value))}`;
      cases++;
      try {
        const esBytes = toBinary(esSchema, create(esSchema, value as never));
        const suryBytes = encode(value);
        const fromEs = JSON.stringify(normalize(decode(esBytes)));
        const fromSury = JSON.stringify(normalize(fromBinary(esSchema, suryBytes)));
        const expected = JSON.stringify(normalize(value));
        if (fromEs !== expected) fail(`${label}\n  Sury read protobuf-es's bytes as ${fromEs}`);
        if (fromSury !== expected) fail(`${label}\n  protobuf-es read Sury's bytes as ${fromSury}`);
        const resValue = toRes(message, value);
        const resBytes = resEncode(resValue);
        const fromEsRes = JSON.stringify(normalize(resDecode(esBytes)));
        if (fromEsRes !== JSON.stringify(normalize(resValue))) fail(`${label}\n  ReScript read protobuf-es's bytes as ${fromEsRes}`);
        if (Buffer.compare(Buffer.from(resBytes), Buffer.from(esBytes)) !== 0) fail(`${label}\n  ReScript wrote [${[...resBytes]}], protobuf-es [${[...esBytes]}]`);
        if (Buffer.compare(Buffer.from(esBytes), Buffer.from(suryBytes)) !== 0) {
          byteDiffs++;
          fail(`${label}\n  bytes differ: protobuf-es [${[...esBytes]}] Sury [${[...suryBytes]}]`);
        }
      } catch (error) {
        fail(`${label}\n  threw ${(error as Error).stack}`);
      }
    }
  }

  // ── reprint ──────────────────────────────────────────────────────────────
  const reprint = join(work, "reprint");
  mkdirSync(reprint);
  writeFileSync(join(reprint, "buf.yaml"), "version: v2\n");
  const printed: { message: Message; path: string; root: string }[] = [];
  for (const [idx, { file, message, name }] of messages.entries()) {
    const suryModule = await import(pathToFileURL(join(root, "test/generated/ts", `${file}.ts`)).href);
    const pkg = `reprint${idx}`;
    const rootName = `Root${idx}`;
    writeFileSync(join(reprint, `m${idx}.proto`), S.toProtoOrThrow(suryModule[name], { name: rootName, package: pkg }));
    printed.push({ message, path: `m${idx}.proto`, root: `${pkg}.${rootName}` });
  }
  execFileSync(bin("buf"), ["build", reprint, "-o", join(work, "reprint.binpb")], { stdio: ["ignore", "ignore", "inherit"] });
  const reprinted = buildRegistry(decodeFileDescriptorSet(new Uint8Array(readFileSync(join(work, "reprint.binpb")))).file);
  const shape = (field: Message["fields"][number]) =>
    `${field.list ? "repeated " : ""}${field.mapKey ? `map<${field.mapKey}> ` : ""}${field.oneof ? "oneof " : ""}${
      field.element.kind === "scalar" ? field.element.scalar : field.element.kind
    }`;
  for (const { message, root: rootName } of printed) {
    const again = reprinted.types.get(rootName);
    if (again === undefined || again.kind !== "message") {
      fail(`reprint: ${message.typeName} printed no message ${rootName}`);
      continue;
    }
    const want = message.fields.map((f) => `${f.number} ${shape(f)}`).sort().join("; ");
    const got = again.fields.map((f) => `${f.number} ${shape(f)}`).sort().join("; ");
    if (want !== got) fail(`reprint: ${message.typeName}\n  declared ${want}\n  printed  ${got}`);
  }

  // ── tree-shaking ─────────────────────────────────────────────────────────
  // Rollup 4, which honors `@__NO_SIDE_EFFECTS__` across the package boundary
  // (esbuild does only within a file): importing one message keeps no other.
  const shake = join(root, "test/.shake");
  rmSync(shake, { recursive: true, force: true });
  const { transform } = await import("esbuild");
  const { rollup } = await import("rollup");
  const { nodeResolve } = await import("@rollup/plugin-node-resolve");
  for (const proto of corpus) {
    const name = `${registry.files.get(proto.name)!.name}_pb`;
    const { code } = await transform(readFileSync(join(root, "test/generated/ts", `${name}.ts`), "utf8"), { loader: "ts", format: "esm" });
    mkdirSync(dirname(join(shake, `${name}.js`)), { recursive: true });
    writeFileSync(join(shake, `${name}.js`), code.replace(/from "(\.\.?\/[^"]+)"/g, 'from "$1.js"'));
  }
  const bundled = async (entry: string): Promise<string> => {
    writeFileSync(join(shake, "entry.js"), entry);
    const bundle = await rollup({
      input: join(shake, "entry.js"),
      plugins: [nodeResolve({ rootDir: root })],
      onwarn: (warning) => {
        if (warning.code === "UNRESOLVED_IMPORT") throw new Error(warning.message);
      },
    });
    const { output } = await bundle.generate({ format: "es" });
    await bundle.close();
    return output[0].code;
  };
  // Markers are text only the message they name emits: a string it passes, or
  // one of its field labels.
  const shakes = (label: string, code: string, kept: string[], dropped: string[]) => {
    for (const marker of kept) if (!code.includes(marker)) fail(`tree-shaking: ${label} lost ${marker}`);
    for (const marker of dropped) if (code.includes(marker)) fail(`tree-shaking: ${label} kept ${marker}`);
  };
  shakes(
    "one generated message",
    await bundled('import { EmptySchema } from "./example/v1/kitchen_sink_pb.js";\nconsole.log(EmptySchema);\n'),
    ['"Empty"'],
    ['"Scalars"', '"User"', '"WellKnown"', '"Node"', '"Branch"', '"Address"', '"sfixed64"', "boxedInOneof", "nanos"],
  );
  shakes(
    "one well-known type",
    await bundled('import { TimestampSchema } from "sury/wkt";\nconsole.log(TimestampSchema);\n'),
    ['"Timestamp"', "seconds"],
    ['"Duration"', '"Struct"', '"Value"', "typeUrl", "fileName", "responseStreaming", '"Int32Value"'],
  );
  shakes(
    "one ReScript module",
    await bundled(`import { Empty } from ${JSON.stringify(join(root, "test/generated/res/example/v1/Example_v1_kitchen_sink_pb.res.mjs"))};\nconsole.log(Empty.schema);\n`),
    [],
    ["firstName", "boxedInOneof", '"sfixed64"', '"Node"', '"Branch"', "nanos"],
  );
  shakes(
    "one SuryProtobuf module",
    await bundled('import { Timestamp } from "sury/src/SuryProtobuf.res.mjs";\nconsole.log(Timestamp.schema);\n'),
    ["seconds", "nanos"],
    ['"Struct"', '"Value"', "typeUrl", "fileName", "responseStreaming", '"Int32Value"'],
  );

  console.log(`protoc-gen-sury: ${corpus.length} files, ${messages.length} messages, ${cases} value cases, ${printed.length} reprints`);
  if (byteDiffs) console.log(`  ${byteDiffs} cases wrote different bytes`);
} finally {
  rmSync(work, { recursive: true, force: true });
}

if (failures.length) {
  console.error(failures.slice(0, 30).join("\n\n"));
  console.error(`\n${failures.length} failure(s)${update ? "" : " - `pnpm test update` rewrites the goldens"}`);
  process.exit(1);
}
console.log(update ? "goldens written" : "all gates pass");
