// The generator's gates, over the corpus in test/proto, one function each.
//
//   pnpm protobuf:codegen           check
//   pnpm protobuf:codegen update    rewrite the goldens
import { execFileSync, spawnSync } from "node:child_process";
import { existsSync, mkdirSync, mkdtempSync, readdirSync, readFileSync, rmSync, statSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import type { DescMessage, MessageShape } from "@bufbuild/protobuf";
import { type CodeGeneratorResponse, type DescriptorProto, decodeFileDescriptorSet, type FieldDescriptorProto, type FileDescriptorProto } from "../src/descriptor";
import { buildRegistry, type Message, nestedTypes, type Registry } from "../src/model";
import { namesOf } from "../src/names";
import { fieldNames, moduleOf, resPath } from "../src/res";
import { isStruct, wktFiles, wrapperScalar } from "../src/shared";
import { generate } from "../src/plugin";
import { generateWkt, generateWktRes, sRes, withWktRes, wktSet } from "../scripts/wkt";
import { normalize, rngOf, sampleMessage } from "./sample";

const root = join(dirname(fileURLToPath(import.meta.url)), "..");
const bin = (name: string) => join(root, "node_modules/.bin", name);
const update = process.argv[2] === "update";
const failures: string[] = [];
const fail = (message: string) => failures.push(message);
const S = await import("sury");
const { version } = JSON.parse(readFileSync(join(root, "../sury/package.json"), "utf8")) as { version: string };
const { create, fromBinary, toBinary } = await import("@bufbuild/protobuf");

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

const holdFile = (path: string, content: string, label: string) => {
  if (update) writeFileSync(path, content);
  else if (!existsSync(path) || readFileSync(path, "utf8") !== content) fail(`${label}: ${path} differs from what the generator writes`);
};

const mapValues = (value: unknown, fn: (v: unknown) => unknown): unknown =>
  Array.isArray(value) ? value.map(fn)
  : value !== null && typeof value === "object" && !(value instanceof Uint8Array)
    ? Object.fromEntries(Object.entries(value).map(([k, v]) => [k, fn(v)]))
    : value;

// A value in the shape the ReScript schemas hold: each field under its
// ReScript label, and an unset oneof of a generated message `undefined`, while
// a well-known type, bound to `sury/protobuf/wkt`, keeps protobuf-es's shape.
const toRes = (message: Message, value: Record<string, unknown>): Record<string, unknown> => {
  if (wktFiles.has(message.file.proto.name)) return value;
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

type Corpus = {
  bytes: Uint8Array;
  set: { file: FileDescriptorProto[] };
  files: FileDescriptorProto[];
  registry: Registry;
  messages: { file: string; message: Message; name: string }[];
  run: (parameter: string, version?: string) => { name: string; content: string }[];
};

const corpusOf = (work: string): Corpus => {
  execFileSync(bin("buf"), ["build", join(root, "test"), "-o", join(work, "set.binpb")], { stdio: ["ignore", "ignore", "inherit"] });
  const bytes = new Uint8Array(readFileSync(join(work, "set.binpb")));
  const set = decodeFileDescriptorSet(bytes);
  const files = set.file.filter((file) => !file.name.startsWith("google/"));
  const registry = buildRegistry(set.file);
  const messages: Corpus["messages"] = [];
  for (const proto of files) {
    const file = registry.files.get(proto.name)!;
    for (const desc of nestedTypes(file)) {
      if (desc.kind === "message") messages.push({ file: `./${file.name}_pb`, message: desc, name: namesOf(file).schema.get(desc)! });
    }
  }
  const run = (parameter: string, version = "") => {
    const response = generate({ fileToGenerate: files.map((f) => f.name), parameter, protoFile: set.file }, version);
    if (response.error !== undefined) throw new Error(response.error);
    return response.file;
  };
  return { bytes, set, files, registry, messages, run };
};

const goldens = (corpus: Corpus) => {
  holdGoldens(join(root, "test/generated/ts"), corpus.run("target=ts"), "ts golden");
  holdGoldens(join(root, "test/generated/res"), corpus.run("target=res"), "res golden");
  const wkt = wktSet();
  holdGoldens(join(root, "../sury/src/protobuf/wkt"), generateWkt(wkt), "sury/protobuf/wkt");
  holdFile(sRes, withWktRes(readFileSync(sRes, "utf8"), generateWktRes(wkt)), "S.Protobuf");
};

const es = join(root, "test/.es");

const reference = (work: string) => {
  rmSync(es, { recursive: true, force: true });
  writeFileSync(
    join(work, "buf.gen.yaml"),
    `version: v2\nplugins:\n  - local: ${bin("protoc-gen-es")}\n    out: ${es}\n    opt: [target=ts, erasable_syntax=true]\n`,
  );
  execFileSync(bin("buf"), ["generate", join(root, "test"), "--template", join(work, "buf.gen.yaml")], { stdio: "inherit" });
};

// tsc over the generated TypeScript, where S.schemaOf holds each schema equal
// to the type beside it, and over every type set equal to protoc-gen-es's;
// rescript over the generated ReScript, warnings as errors.
const compile = (corpus: Corpus) => {
  let typeTest = `// Generated by test/cli.ts: protoc-gen-es's type for every message and enum,
// with what docs/js-protobuf.md lists as differences normalized, equal to ours.
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
  : { [K in keyof Own<T> as K extends number ? string : K]: Normalize<Own<T>[K]> };
type Equal<A, B> = (<V>() => V extends A ? 1 : 2) extends <V>() => V extends B ? 1 : 2 ? true : false;
`;
  for (const proto of corpus.files) {
    const file = corpus.registry.files.get(proto.name)!;
    const alias = file.name.replace(/[^A-Za-z0-9]/g, "_");
    typeTest += `import type * as es_${alias} from "./.es/${file.name}_pb";\nimport type * as sury_${alias} from "./generated/ts/${file.name}_pb";\n`;
    for (const desc of nestedTypes(file)) {
      const name = namesOf(file).shape.get(desc)!;
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
    // Building sury as a dependency cleans the output of its dev sources, the
    // committed tests/*.res.mjs; its own build puts them back.
    execFileSync(bin("rescript"), [], { cwd: join(root, "../sury"), stdio: "pipe" });
  } catch (error) {
    const { stdout, stderr } = error as { stdout: Buffer; stderr: Buffer };
    fail(`rescript:\n${stdout.toString()}${stderr.toString()}`);
  }
};

const generatedTs = (file: string) => import(pathToFileURL(join(root, "test/generated/ts", `${file}.ts`)).href);

// Values drawn from each descriptor, encoded by one side and decoded by the
// other, both ways, through the TypeScript and the ReScript, and the bytes
// compared.
const values = async (corpus: Corpus, rng: () => number): Promise<number> => {
  let cases = 0;
  for (const { file, message, name } of corpus.messages) {
    const esSchema = (await import(pathToFileURL(join(es, `${file}.ts`)).href))[name];
    const surySchema = (await generatedTs(file))[name];
    const resModule = await import(pathToFileURL(join(root, "test/generated/res", `${resPath(message.file)}.res.mjs`)).href);
    // ReScript prefixes `$$` to a module named as a JS global, `String` among them.
    const resSchema = (resModule[moduleOf(message)] ?? resModule[`$$${moduleOf(message)}`]).schema;
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
        if (Buffer.compare(Buffer.from(esBytes), Buffer.from(suryBytes)) !== 0) fail(`${label}\n  bytes differ: protobuf-es [${[...esBytes]}] Sury [${[...suryBytes]}]`);
      } catch (error) {
        fail(`${label}\n  threw ${(error as Error).stack}`);
      }
    }
  }
  return cases;
};

// An Any field read as a tagged union of the generated messages it holds,
// against protobuf-es's anyPack of the same values.
const typedAny = async (corpus: Corpus, rng: () => number): Promise<number> => {
  const file = "example/v1/kitchen_sink_pb";
  const esModule = await import(pathToFileURL(join(es, `${file}.ts`)).href);
  const suryModule = await generatedTs(file);
  const { anyPack } = await import("@bufbuild/protobuf/wkt");
  const arms = ["User", "Node"].map((name) => ({
    name,
    model: corpus.messages.find((entry) => entry.message.typeName === `example.v1.${name}`)!.message,
  }));
  const member = S.union(arms.map(({ name }) => S.schema({ type: name, value: suryModule[`${name}Schema`] })));
  const Typed = S.schema({
    payload: S.protobufField(S.optional(member), 1),
    details: S.protobufField(S.array(member), 2),
  });
  const encodeTyped = S.decodeOrThrow(Typed, S.protobuf) as (value: unknown) => Uint8Array;
  const decodeTyped = S.decodeOrThrow(S.protobuf, Typed) as (bytes: Uint8Array) => unknown;
  const pack = (arm: (typeof arms)[number], value: unknown) =>
    anyPack(esModule[`${arm.name}Schema`], create(esModule[`${arm.name}Schema`], value as never));
  let cases = 0;
  for (let idx = 0; idx < 60; idx++) {
    const picked = Array.from({ length: Math.floor(rng() * 3) + 1 }, () => {
      const arm = arms[Math.floor(rng() * arms.length)]!;
      return { arm, value: sampleMessage(rng, arm.model) };
    });
    const [first, ...rest] = picked;
    const typed = { payload: { type: first!.arm.name, value: first!.value }, details: rest.map(({ arm, value }) => ({ type: arm.name, value })) };
    const label = `typed Any #${idx} ${JSON.stringify(normalize(typed))}`;
    cases++;
    try {
      const esBytes = toBinary(
        esModule.EnvelopeSchema,
        create(esModule.EnvelopeSchema, { payload: pack(first!.arm, first!.value), details: rest.map(({ arm, value }) => pack(arm, value)) }),
      );
      const suryBytes = encodeTyped(typed);
      if (Buffer.compare(Buffer.from(esBytes), Buffer.from(suryBytes)) !== 0) {
        fail(`${label}\n  bytes differ: protobuf-es [${[...esBytes]}] Sury [${[...suryBytes]}]`);
      }
      const fromEs = JSON.stringify(normalize(decodeTyped(esBytes)));
      if (fromEs !== JSON.stringify(normalize(typed))) fail(`${label}\n  Sury read protobuf-es's bytes as ${fromEs}`);
    } catch (error) {
      fail(`${label}\n  threw ${(error as Error).stack}`);
    }
  }
  return cases;
};

// S.toProtoOrThrow of each message, compiled again by buf, has the fields the
// source declared: number, label and type.
const reprint = async (corpus: Corpus, work: string) => {
  const dir = join(work, "reprint");
  mkdirSync(dir);
  writeFileSync(join(dir, "buf.yaml"), "version: v2\n");
  const printed: { message: Message; root: string }[] = [];
  for (const [idx, { file, message, name }] of corpus.messages.entries()) {
    const pkg = `reprint${idx}`;
    const rootName = `Root${idx}`;
    writeFileSync(join(dir, `m${idx}.proto`), S.toProtoOrThrow((await generatedTs(file))[name], { name: rootName, package: pkg }));
    printed.push({ message, root: `${pkg}.${rootName}` });
  }
  execFileSync(bin("buf"), ["build", dir, "-o", join(work, "reprint.binpb")], { stdio: ["ignore", "ignore", "inherit"] });
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
  return printed.length;
};

// descriptor.ts's decode of the corpus's descriptor set against protobuf-es's
// own, over every field the generator reads.
const descriptors = async (corpus: Corpus) => {
  const { fromBinary, isFieldSet } = await import("@bufbuild/protobuf");
  const wkt = await import("@bufbuild/protobuf/wkt");
  type Es<T extends DescMessage> = MessageShape<T>;
  const set = (message: object, schema: DescMessage, name: string) => isFieldSet(message as never, schema.field[name]!);
  const only = <T>(present: boolean, value: T): T | undefined => (present ? value : undefined);
  const options = (o: Es<typeof wkt.FieldOptionsSchema> | undefined) =>
    o && { packed: only(set(o, wkt.FieldOptionsSchema, "packed"), o.packed), deprecated: o.deprecated, jstype: o.jstype };
  const field = (f: Es<typeof wkt.FieldDescriptorProtoSchema>) => ({
    name: f.name,
    extendee: f.extendee,
    number: f.number,
    label: f.label,
    type: f.type,
    typeName: f.typeName,
    defaultValue: only(set(f, wkt.FieldDescriptorProtoSchema, "defaultValue"), f.defaultValue),
    options: options(f.options),
    oneofIndex: only(set(f, wkt.FieldDescriptorProtoSchema, "oneofIndex"), f.oneofIndex),
    jsonName: only(set(f, wkt.FieldDescriptorProtoSchema, "jsonName"), f.jsonName),
    proto3Optional: f.proto3Optional,
  });
  const enumType = (e: Es<typeof wkt.EnumDescriptorProtoSchema>) => ({
    name: e.name,
    value: e.value.map((v) => ({ name: v.name, number: v.number, options: v.options && { deprecated: v.options.deprecated } })),
    options: e.options && { deprecated: e.options.deprecated },
  });
  const message = (m: Es<typeof wkt.DescriptorProtoSchema>): unknown => ({
    name: m.name,
    field: m.field.map(field),
    nestedType: m.nestedType.map(message),
    enumType: m.enumType.map(enumType),
    extension: m.extension.map(field),
    options: m.options && { deprecated: m.options.deprecated, mapEntry: m.options.mapEntry },
    oneofDecl: m.oneofDecl.map((o) => ({ name: o.name })),
  });
  const file = (f: Es<typeof wkt.FileDescriptorProtoSchema>) => ({
    name: f.name,
    package: f.package,
    dependency: f.dependency,
    messageType: f.messageType.map(message),
    enumType: f.enumType.map(enumType),
    service: f.service.map((s) => ({ name: s.name })),
    extension: f.extension.map(field),
    sourceCodeInfo: f.sourceCodeInfo && {
      location: f.sourceCodeInfo.location.map((l) => ({
        path: l.path,
        leadingComments: only(set(l, wkt.SourceCodeInfo_LocationSchema, "leadingComments"), l.leadingComments),
        trailingComments: only(set(l, wkt.SourceCodeInfo_LocationSchema, "trailingComments"), l.trailingComments),
      })),
    },
    syntax: f.syntax,
  });
  const es = fromBinary(wkt.FileDescriptorSetSchema, corpus.bytes).file;
  for (const [idx, ours] of corpus.set.file.entries()) {
    const want = JSON.stringify(normalize(file(es[idx]!)));
    if (JSON.stringify(normalize(ours)) !== want) fail(`descriptors: ${ours.name} decodes otherwise than protobuf-es reads it\n  protobuf-es ${want}\n  ours        ${JSON.stringify(normalize(ours))}`);
  }
};

const protocGenSury = join(root, "bin/protoc-gen-sury.mjs");

// The bin as buf runs it, a CodeGeneratorRequest on stdin, and as it runs on a
// descriptor set, each writing what generate() returns for the same files.
const plugin = (corpus: Corpus, work: string) => {
  const expected = corpus.run("target=ts+res", version);
  const holds = (label: string, dir: string) => {
    const written = listFiles(dir);
    if (written.length !== expected.length) fail(`${label}: wrote ${written.join(", ")}`);
    for (const { name, content } of expected) {
      if (!existsSync(join(dir, name))) fail(`${label}: didn't write ${name}`);
      else if (readFileSync(join(dir, name), "utf8") !== content) fail(`${label}: ${name} differs from what generate() returns`);
    }
  };
  writeFileSync(
    join(work, "plugin.gen.yaml"),
    `version: v2\nplugins:\n  - local: ${protocGenSury}\n    out: ${join(work, "plugin")}\n    opt: target=ts+res\n    strategy: all\n`,
  );
  execFileSync(bin("buf"), ["generate", join(root, "test"), "--template", join(work, "plugin.gen.yaml")], { cwd: root, stdio: "inherit" });
  holds("plugin over stdin", join(work, "plugin"));
  const run = (out: string, opt: string) =>
    spawnSync(protocGenSury, [`--descriptor_set_in=${join(work, "set.binpb")}`, `--out=${out}`, `--opt=${opt}`], { cwd: root, encoding: "utf8" });
  const done = run(join(work, "set"), "target=ts+res");
  if (done.status !== 0) fail(`plugin over a descriptor set: exited ${done.status}\n${done.stderr}`);
  holds("plugin over a descriptor set", join(work, "set"));
  const refused = run(join(work, "refused"), "target=go");
  if (refused.status !== 1 || !refused.stderr.includes('protoc-gen-sury: invalid option "target=go"') || existsSync(join(work, "refused"))) {
    fail(`plugin over a descriptor set: an invalid option exited ${refused.status} and wrote ${refused.stderr}`);
  }
};

const proto = (name: string, fields: Partial<FileDescriptorProto> = {}): FileDescriptorProto => ({
  name,
  package: "",
  dependency: [],
  messageType: [],
  enumType: [],
  service: [],
  extension: [],
  syntax: "proto3",
  ...fields,
});
const messageProto = (name: string, field: FieldDescriptorProto[] = [], fields: Partial<DescriptorProto> = {}): DescriptorProto => ({
  name,
  field,
  nestedType: [],
  enumType: [],
  extension: [],
  oneofDecl: [],
  ...fields,
});
const fieldProto = (name: string, type: number, typeName = "", fields: Partial<FieldDescriptorProto> = {}): FieldDescriptorProto => ({
  name,
  number: 1,
  label: 1,
  type,
  typeName,
  extendee: "",
  proto3Optional: false,
  ...fields,
});

// Every option, and every refusal, by what generate() answers.
const options = (corpus: Corpus) => {
  const sink = "example/v1/kitchen_sink.proto";
  const one = (parameter: string, files = [sink], protoFile = corpus.set.file) =>
    generate({ fileToGenerate: files, parameter, protoFile }, "");
  const names = (response: CodeGeneratorResponse) => response.error ?? response.file.map((file) => file.name).join(", ");
  const content = (response: CodeGeneratorResponse) => response.error ?? response.file.map((file) => file.content).join("\n");
  const imports = (response: CodeGeneratorResponse) => content(response).match(/from "\.[^"]*"/g)?.join(", ") ?? "";
  const extensionField = fieldProto("x", 5, "", { extendee: ".M", number: 100 });
  const cases: [string, () => string, (got: string) => boolean][] = [
    ["target=ts+res", () => names(one("target=ts+res")), (got) => got === "example/v1/kitchen_sink_pb.ts, example/v1/Example_v1_kitchen_sink_pb.res"],
    ["target=res", () => names(one("target=res")), (got) => got === "example/v1/Example_v1_kitchen_sink_pb.res"],
    ["no target", () => names(one("")), (got) => got === "example/v1/kitchen_sink_pb.ts"],
    ["ts_nocheck", () => content(one("ts_nocheck")), (got) => got.includes("/* eslint-disable */\n// @ts-nocheck\n")],
    ["ts_nocheck=false", () => content(one("ts_nocheck=false")), (got) => !got.includes("@ts-nocheck")],
    ["import_extension=ts", () => imports(one("import_extension=ts")), (got) => got === 'from "../common/address_pb.ts"'],
    ["import_extension=js", () => imports(one("import_extension=js")), (got) => got === 'from "../common/address_pb.js"'],
    ["import_extension=none", () => imports(one("import_extension=none")), (got) => got === 'from "../common/address_pb"'],
    ["a file with no types", () => names(one("", ["e.proto"], [proto("e.proto")])), (got) => got === ""],
    ["keep_empty_files", () => names(one("keep_empty_files", ["e.proto"], [proto("e.proto")])), (got) => got === "e_pb.ts"],
    ["keep_empty_files=false", () => names(one("keep_empty_files=false", ["e.proto"], [proto("e.proto")])), (got) => got === ""],
    ["target=go", () => names(one("target=go")), (got) => got === 'invalid option "target=go": the targets are ts, res and ts+res'],
    ["import_extension=mjs", () => names(one("import_extension=mjs")), (got) => got === 'invalid option "import_extension=mjs": use none, js or ts'],
    ["ts_nocheck=yes", () => names(one("ts_nocheck=yes")), (got) => got === 'invalid option "ts_nocheck=yes": use true or false'],
    ["keep_empty_files=1", () => names(one("keep_empty_files=1")), (got) => got === 'invalid option "keep_empty_files=1": use true or false'],
    ["an unknown option", () => names(one("target=ts,json_types=true")), (got) => got === 'unknown option "json_types": protoc-gen-sury takes target, import_extension, keep_empty_files and ts_nocheck'],
    ["a file not in the request", () => names(one("", ["missing.proto"])), (got) => got === "missing.proto is not among the files in the request"],
    ["proto2", () => names(one("", ["p.proto"], [proto("p.proto", { syntax: "" })])), (got) => got === "p.proto is proto2; protoc-gen-sury generates proto3 files"],
    ["editions", () => names(one("", ["p.proto"], [proto("p.proto", { syntax: "editions" })])), (got) => got === "p.proto is editions; protoc-gen-sury generates proto3 files"],
    [
      "an extension",
      () => names(one("", ["p.proto"], [proto("p.proto", { messageType: [messageProto("M")], extension: [extensionField] })])),
      (got) => got === "p.proto declares an extension, which protoc-gen-sury does not generate",
    ],
    [
      "a nested extension",
      () => names(one("", ["p.proto"], [proto("p.proto", { messageType: [messageProto("M", [], { nestedType: [messageProto("N", [], { extension: [extensionField] })] })] })])),
      (got) => got === "p.proto declares an extension, which protoc-gen-sury does not generate",
    ],
    [
      "a group",
      () => names(one("", ["p.proto"], [proto("p.proto", { messageType: [messageProto("M", [fieldProto("g", 10, ".M.G")], { nestedType: [messageProto("G")] })] })])),
      (got) => got === "field M.g is a group, which protoc-gen-sury does not generate",
    ],
    [
      "a proto2 dependency",
      () =>
        names(one("", ["p.proto"], [
          proto("old.proto", { syntax: "proto2", messageType: [messageProto("Old")] }),
          proto("p.proto", { dependency: ["old.proto"], messageType: [messageProto("M", [fieldProto("old", 11, ".Old")])] }),
        ])),
      (got) => got === "field M.old refers to Old, from old.proto, which is proto2",
    ],
    [
      "a type no file declares",
      () => names(one("", ["p.proto"], [proto("p.proto", { messageType: [messageProto("M", [fieldProto("gone", 11, ".Gone")])] })])),
      (got) => got === "field M.gone refers to .Gone, which no file in the request declares",
    ],
  ];
  for (const [label, run, holds] of cases) {
    const got = run();
    if (!holds(got)) fail(`options: ${label} answered ${JSON.stringify(got)}`);
  }
};

// Rollup 4, which honors `@__NO_SIDE_EFFECTS__` across the package boundary,
// and esbuild, which reads only `/* @__PURE__ */` there: importing one message
// keeps no other. ReScript writes no annotation, so its modules are held to
// Rollup alone.
const treeShaking = async (corpus: Corpus) => {
  const shake = join(root, "test/.shake");
  rmSync(shake, { recursive: true, force: true });
  const { build, transform } = await import("esbuild");
  const { rollup } = await import("rollup");
  const { nodeResolve } = await import("@rollup/plugin-node-resolve");
  for (const proto of corpus.files) {
    const name = `${corpus.registry.files.get(proto.name)!.name}_pb`;
    const { code } = await transform(readFileSync(join(root, "test/generated/ts", `${name}.ts`), "utf8"), { loader: "ts", format: "esm" });
    mkdirSync(dirname(join(shake, `${name}.js`)), { recursive: true });
    writeFileSync(join(shake, `${name}.js`), code.replace(/from "(\.\.?\/[^"]+)"/g, 'from "$1.js"'));
  }
  type Bundler = "Rollup" | "esbuild";
  const bundled = async (entry: string, bundler: Bundler): Promise<string> => {
    const input = join(shake, "entry.js");
    writeFileSync(input, entry);
    if (bundler === "esbuild") {
      const { outputFiles } = await build({ entryPoints: [input], bundle: true, write: false, format: "esm", logLevel: "silent" });
      return outputFiles[0]!.text;
    }
    const bundle = await rollup({
      input,
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
  const shakes = async (label: string, bundlers: Bundler[], entry: string | ((bundler: Bundler) => Promise<string>), kept: string[], dropped: string[]) => {
    for (const bundler of bundlers) {
      const code = typeof entry === "string" ? await bundled(entry, bundler) : await entry(bundler);
      for (const marker of kept) if (!code.includes(marker)) fail(`tree-shaking: ${bundler}'s ${label} lost ${marker}`);
      for (const marker of dropped) if (code.includes(marker)) fail(`tree-shaking: ${bundler}'s ${label} kept ${marker}`);
    }
  };
  const both: Bundler[] = ["Rollup", "esbuild"];
  await shakes(
    "one generated message",
    both,
    'import { EmptySchema } from "./example/v1/kitchen_sink_pb.js";\nconsole.log(EmptySchema);\n',
    ['"Empty"'],
    ['"Scalars"', '"User"', '"WellKnown"', '"Node"', '"Branch"', '"GraphA"', '"Address"', '"sfixed64"', "boxedInOneof", "nanos"],
  );
  await shakes(
    "one well-known type",
    both,
    'import { TimestampSchema } from "sury/protobuf/wkt";\nconsole.log(TimestampSchema);\n',
    ['name: "Timestamp"', "seconds"],
    ['name: "Duration"', '"numberValue"', "typeUrl", "fileName", "responseStreaming", 'name: "Int32Value"'],
  );
  const resModule = JSON.stringify(join(root, "test/generated/res/example/v1/Example_v1_kitchen_sink_pb.res.mjs"));
  await shakes(
    "one ReScript module",
    ["Rollup"],
    `import { Empty } from ${resModule};\nconsole.log(Empty.schema);\n`,
    [],
    ["firstName", "boxedInOneof", '"sfixed64"', '"Node"', '"Branch"', '"GraphA"', "nanos"],
  );
  // A group member other than the first, bundled alone, still builds: what it
  // reads can't be a variable only a dropped export fills.
  const graph = "{ a: [{ b: [{ pick: { case: \"c\", value: { a: [], b: {} } } }], c: {} }], b: { 1: { pick: { case: \"none\", value: 1 } } } }";
  const runs = (label: string, from: string, schema: string) => async (bundler: Bundler) => {
    const code = await bundled(
      `import * as S from "sury";\nimport ${from};\nconst value = ${graph};\n` +
        `const back = S.decodeOrThrow(S.protobuf, ${schema})(S.decodeOrThrow(${schema}, S.protobuf)(value));\n` +
        `if (back.a[0].b[0].pick.case !== "c") throw new Error(JSON.stringify(back));\n`,
      bundler,
    );
    const path = join(shake, `${bundler}_${label.replace(/\W+/g, "_")}.mjs`);
    writeFileSync(path, code);
    try {
      await import(pathToFileURL(path).href);
    } catch (error) {
      fail(`tree-shaking: ${bundler}'s ${label} bundled alone doesn't run: ${(error as Error).message}`);
    }
    return code;
  };
  await shakes(
    "one member of a group",
    both,
    runs("one member of a group", '{ GraphCSchema } from "./example/v1/kitchen_sink_pb.js"', "GraphCSchema"),
    ['"example.v1.GraphA"', '"example.v1.GraphB"'],
    ['"Node"', '"Branch"', '"Scalars"'],
  );
  await shakes(
    "one ReScript module of a group",
    ["Rollup"],
    runs("one ReScript module of a group", `{ GraphC } from ${resModule}`, "GraphC.schema"),
    ['"example.v1.GraphA"', '"example.v1.GraphB"'],
    ['"Node"', '"Branch"', "firstName"],
  );
  const timestampOnly = join(root, "test/res/TimestampOnly.res.mjs");
  if (!readFileSync(timestampOnly, "utf8").includes('from "sury/protobuf/wkt"')) fail(`tree-shaking: ${timestampOnly} doesn't import sury/protobuf/wkt`);
  await shakes(
    "one S.Protobuf type",
    both,
    `import { schema } from ${JSON.stringify(timestampOnly)};\nconsole.log(schema);\n`,
    ['name: "Timestamp"', "seconds"],
    ['name: "Duration"', '"numberValue"', "typeUrl", "fileName", "responseStreaming", 'name: "Int32Value"', '"google.protobuf.Mixin"'],
  );
};

function report(): void {
  if (failures.length) {
    console.error(failures.slice(0, 30).join("\n\n"));
    console.error(`\n${failures.length} failure(s)${update ? "" : " - `pnpm protobuf:codegen update` rewrites the goldens"}`);
    process.exit(1);
  }
  console.log(update ? "goldens written" : "all gates pass");
}

const work = mkdtempSync(join(tmpdir(), "protoc-gen-sury-"));
try {
  const corpus = corpusOf(work);
  await descriptors(corpus);
  goldens(corpus);
  plugin(corpus, work);
  options(corpus);
  reference(work);
  compile(corpus);
  // What follows imports the compiled output.
  if (failures.length) report();
  const rng = rngOf(Number(process.env.SEED ?? 1));
  const cases = (await values(corpus, rng)) + (await typedAny(corpus, rng));
  const reprints = await reprint(corpus, work);
  await treeShaking(corpus);
  console.log(`protoc-gen-sury: ${corpus.files.length} files, ${corpus.messages.length} messages, ${cases} value cases, ${reprints} reprints`);
} finally {
  rmSync(work, { recursive: true, force: true });
}

report();
