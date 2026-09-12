// The Protobuf page. Every number here already had a home in
// packages/protobuf-test-suite or packages/protobuf-conformance; this module
// is the pivot from "what that suite prints" to "what the page shows", and
// adds nothing of its own.
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { type BenchRow, benchVersions, runBench } from "protobuf-test-suite/bench";
import { type BundleRow, runBundleSize } from "protobuf-test-suite/bundlesize";
import { COLUMNS, FEATURES } from "protobuf-test-suite/features";
import { buildFeatures } from "../features";
import type { Cell, Table } from "../table";
import type { Source } from "../registry";

// Bench and bundle name the same libraries differently, so the page picks one
// spelling and both are read through it.
const LIBRARIES = [
  { column: "Sury", bench: "sury", bundle: "Sury" },
  { column: "protobufjs (reflect)", bench: "protobufjs reflect", bundle: "protobufjs (reflect)" },
  { column: "protobufjs (static)", bench: "protobufjs static", bundle: "protobufjs (static)" },
  { column: "protobuf-es", bench: "protobuf-es", bundle: "protobuf-es" },
  { column: "pbf", bench: "pbf", bundle: "pbf" },
];

const golden = (relative: string): Record<string, unknown> =>
  JSON.parse(readFileSync(fileURLToPath(new URL(relative, import.meta.url)), "utf8")) as Record<string, unknown>;

const bundleSize = async (): Promise<Table> => {
  const rows = await runBundleSize();
  const gzip = (library: string, variant: string): Cell =>
    rows.find((r: BundleRow) => r.library === library && r.variant === variant)?.gzip ?? null;
  // Two libraries ship a runtime that a message is loaded into rather than a
  // codec per message, so they have no cell in the table above. Their figures
  // belong on the page anyway, and stating them here keeps every number on it
  // measured.
  const runtimes = rows
    .filter((r: BundleRow) => r.variant === "runtime only")
    .map((r: BundleRow) => `${r.library} ${(r.gzip / 1000).toFixed(1)} kB`)
    .join(", ");
  return {
    columns: LIBRARIES.map((l) => l.column),
    rows: [
      {
        label: "Encode and decode",
        cells: LIBRARIES.map((l) => gzip(l.bundle, "encode+decode")),
        format: "bytes",
        best: "low",
      },
      {
        label: "Decode only",
        note: "what a client that never sends the message gets back from tree-shaking",
        cells: LIBRARIES.map((l) => gzip(l.bundle, "decode only")),
        format: "bytes",
        best: "low",
      },
    ],
    note: `<sub>One six-field message's codec, bundled with esbuild, minified and gzipped. protobufjs's reflection path parses the \`.proto\` at runtime, so it has no decode-only build. Runtimes that carry no message of their own: ${runtimes}.</sub>`,
  };
};

const performance = async (): Promise<Table> => {
  const rows = await runBench();
  const ns = (row: BenchRow, bench: string, direction: "encodeNs" | "decodeNs"): Cell =>
    row.cells.find((c) => c.library === bench)?.[direction] ?? null;
  return {
    columns: LIBRARIES.map((l) => l.column),
    rows: rows.flatMap((row: BenchRow) =>
      (["encodeNs", "decodeNs"] as const).map((direction) => ({
        label: `${row.id} · ${direction === "encodeNs" ? "encode" : "decode"}`,
        note: direction === "encodeNs" ? `${row.bytes} bytes on the wire` : undefined,
        cells: LIBRARIES.map((l) => ns(row, l.bench, direction)),
        format: "ns" as const,
        best: "low" as const,
      })),
    ),
    note: "<sub>Best of seven samples per cell, since a sample landing on a GC pause reads double and which library pays it is luck of the draw. `tiny` and `typical` are this suite's own shapes, `common` is protobuf.js's own benchmark message, and `tile` is a Mapbox vector tile: almost entirely packed varints, which is what pbf is built for.</sub>",
  };
};

const conformance = async (): Promise<Table> => {
  const google = golden("../../protobuf-conformance/goldens/conformance.json") as unknown as {
    attempted: number;
    passed: number;
    expectedFailures: number;
    skipped: number;
    upstream: string;
    runner: string;
  };
  const ours = golden("../../protobuf-test-suite/goldens/coverage.json") as unknown as {
    summary: { cases: number; passed: number };
  };
  return {
    columns: ["Cases", "Passed", "Rate"],
    rows: [
      {
        label: "Google's `conformance_test_runner`, binary proto3",
        note: `${google.expectedFailures} known failures, each named with its reason in \`failing_tests.txt\``,
        cells: [google.attempted, google.passed, `${((google.passed / google.attempted) * 100).toFixed(1)}%`],
        format: "count",
      },
      {
        label: "This repo's corpus, against protobufjs and protobuf-es",
        note: "every round trip checked against two independent implementations",
        cells: [ours.summary.cases, ours.summary.passed, `${((ours.summary.passed / ours.summary.cases) * 100).toFixed(1)}%`],
        format: "count",
      },
    ],
    note: `<sub>Google's runner generates its cases inside the binary rather than reading them from a file, so nothing here can drift from upstream; the corpus is pinned at \`${google.upstream.slice(0, 12)}\` and the runner at ${google.runner}. Its other ${google.skipped} cases are ProtoJSON, text format and proto2 messages, none of which \`S.protobuf\` claims.</sub>`,
  };
};

export const protobuf: Source = {
  id: "protobuf",
  title: "Protobuf benchmarks",
  label: "Protobuf",
  blurb:
    "`S.protobuf` encodes and decodes the Protocol Buffers binary format from an ordinary Sury schema: no `.proto` file, no code generation step, and the same schema still parses, infers types and converts to JSON Schema.",
  versions: benchVersions,
  bundleSize,
  features: () => Promise.resolve(buildFeatures(COLUMNS, FEATURES)),
  conformance,
  performance,
};
