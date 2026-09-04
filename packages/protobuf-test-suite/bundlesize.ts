// One message's codec per library, bundled with esbuild and gzipped: the
// bytes a consumer ships, and how much a decode-only client gets back from
// tree-shaking.
import { mkdirSync, writeFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { gzipSync } from "node:zlib";
import { build } from "esbuild";
import { compileRaw } from "pbf/compile";
import schemaParse from "protocol-buffers-schema";
import protobuf from "protobufjs";
import descriptor from "protobufjs/ext/descriptor/index.js";
import { main as pbjsMain } from "protobufjs-cli/pbjs";
import { WORKLOADS } from "./bench";
import { protoSource } from "./reference";

export type BundleRow = { entry: string; minified: number; gzip: number };

const suryFields =
  '{ id: S.integer.with(S.protobufField, { number: 1, type: "uint32" }), name: S.string.with(S.protobufField, 2), active: S.boolean.with(S.protobufField, 3), tags: S.array(S.string).with(S.protobufField, 4), score: S.optional(S.number).with(S.protobufField, 5), payload: S.optional(S.uint8Array).with(S.protobufField, 6) }';

export const runBundleSize = async (): Promise<BundleRow[]> => {
  const root = fileURLToPath(new URL(".", import.meta.url));
  const dir = `${root}.generated/`;
  mkdirSync(dir, { recursive: true });
  const src = protoSource(WORKLOADS.find((w) => w.id === "typical")!.fields);
  writeFileSync(`${dir}b.proto`, src);
  await new Promise<void>((resolve, reject) =>
    pbjsMain(
      ["-t", "static-module", "-w", "es6", "--no-comments", "--no-verify", "--no-convert", "--no-delimited", "--no-service", "-o", `${dir}b-static.mjs`, `${dir}b.proto`],
      (err) => (err ? reject(err) : resolve()),
    ),
  );
  writeFileSync(`${dir}b-pbf.mjs`, compileRaw(schemaParse(src), { legacy: false }));
  const file = (protobuf.parse(src).root as unknown as { toDescriptor: (s: string) => { file: unknown[] } }).toDescriptor("proto3").file[0];
  const fileBytes = (descriptor as unknown as { FileDescriptorProto: { encode: (v: unknown) => { finish: () => Uint8Array } } }).FileDescriptorProto.encode(file).finish();
  const b64 = Buffer.from(fileBytes).toString("base64");
  const entries: Record<string, string> = {
    "sury (encode+decode)": `import * as S from "sury"; const M = S.schema(${suryFields}); export const d = S.decoder(S.protobuf, M); export const e = S.decoder(M, S.protobuf);`,
    "sury (decode only)": `import * as S from "sury"; const M = S.schema(${suryFields}); export const d = S.decoder(S.protobuf, M);`,
    "protobufjs reflect": `import protobuf from "protobufjs"; const T = protobuf.parse(${JSON.stringify(src)}).root.lookupType("M"); export const d = (b) => T.decode(b); export const e = (v) => T.encode(v).finish();`,
    "protobufjs static (encode+decode)": `import { M } from "./b-static.mjs"; export const d = (b) => M.decode(b); export const e = (v) => M.encode(v).finish();`,
    "protobufjs static (decode only)": `import { M } from "./b-static.mjs"; export const d = (b) => M.decode(b);`,
    "protobuf-es (encode+decode)": `import { fromBinary, toBinary } from "@bufbuild/protobuf"; import { fileDesc, messageDesc } from "@bufbuild/protobuf/codegenv2"; const M = messageDesc(fileDesc(${JSON.stringify(b64)}), 0); export const d = (b) => fromBinary(M, b); export const e = (v) => toBinary(M, v);`,
    "protobuf-es (decode only)": `import { fromBinary } from "@bufbuild/protobuf"; import { fileDesc, messageDesc } from "@bufbuild/protobuf/codegenv2"; const M = messageDesc(fileDesc(${JSON.stringify(b64)}), 0); export const d = (b) => fromBinary(M, b);`,
    "pbf (encode+decode)": `import { PbfReader, PbfWriter } from "pbf"; import { readM, writeM } from "./b-pbf.mjs"; export const d = (b) => readM(new PbfReader(b)); export const e = (v) => { const p = new PbfWriter(); writeM(v, p); return p.finish(); };`,
    "pbf (decode only)": `import { PbfReader } from "pbf"; import { readM } from "./b-pbf.mjs"; export const d = (b) => readM(new PbfReader(b));`,
    "google-protobuf (runtime only)": `import * as jspb from "google-protobuf"; export const d = jspb.BinaryReader; export const e = jspb.BinaryWriter;`,
    "@protobuf-ts/runtime (runtime only)": `import { BinaryReader, BinaryWriter, MessageType } from "@protobuf-ts/runtime"; export const d = BinaryReader; export const e = BinaryWriter; export const m = MessageType;`,
  };
  const rows: BundleRow[] = [];
  for (const [entry, code] of Object.entries(entries)) {
    writeFileSync(`${dir}entry.mjs`, code);
    const result = await build({
      entryPoints: [`${dir}entry.mjs`],
      bundle: true,
      minify: true,
      format: "esm",
      target: "es2020",
      write: false,
      logLevel: "silent",
      absWorkingDir: root,
    });
    const out = result.outputFiles[0]!.contents;
    rows.push({ entry, minified: out.length, gzip: gzipSync(out).length });
  }
  return rows;
};

export const formatBundleSize = (rows: BundleRow[]): string =>
  rows.map((r) => `${r.entry.padEnd(38)} min ${String(r.minified).padStart(7)}  gzip ${String(r.gzip).padStart(6)}`).join("\n");
