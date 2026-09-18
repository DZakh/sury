import { mkdirSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import {
  COMPILER_API_NAMES,
  Coverage,
  NO_WITNESS,
  compileSchema,
  renderSchema,
  schemaCategory,
  schemaWitness,
  type SuryApi,
} from "./schema";
import {
  CASE_VERSION,
  type CaseResult,
  type CompilerCase,
  type Failure,
  type FailureArtifact,
  type OperationKind,
  type SchemaAst,
} from "./types";

const ASYNC_OPERATIONS = new Set<OperationKind>([
  "asyncParser",
  "asyncDecoder",
  "asyncEncoder",
]);

const isSuryError = (error: unknown, S: SuryApi): boolean =>
  !!error &&
  ((typeof S.Error === "function" && error instanceof S.Error) ||
    (error as { name?: unknown }).name === "SuryError");

const isControlledError = (error: unknown, S: SuryApi): boolean =>
  isSuryError(error, S) ||
  (error instanceof Error && error.message.startsWith("[Sury]"));

const errorInfo = (error: unknown): { name: string; message: string } => {
  if (error && typeof error === "object") {
    const value = error as { name?: unknown; message?: unknown; constructor?: { name?: unknown } };
    return {
      name:
        typeof value.name === "string"
          ? value.name
          : typeof value.constructor?.name === "string"
            ? value.constructor.name
            : "Error",
      message: typeof value.message === "string" ? value.message : String(error),
    };
  }
  return { name: typeof error, message: String(error) };
};

const failure = (
  phase: Failure["phase"],
  error: unknown,
  overrideName?: string,
): Failure => {
  const info = errorInfo(error);
  const name = overrideName ?? info.name;
  return {
    phase,
    name,
    message: info.message,
    signature: `${phase}:${name}`,
  };
};

const operationFactory = (
  kind: OperationKind,
  schemas: unknown[],
  S: SuryApi,
): unknown => {
  const operation = S[kind];
  if (typeof operation !== "function") throw new Error(`Missing public Sury API S.${kind}`);
  return operation(...schemas);
};

const withTimeout = async <T>(promise: Promise<T>, timeoutMs: number): Promise<T> => {
  let timer: ReturnType<typeof setTimeout> | undefined;
  const timeout = new Promise<never>((_, reject) => {
    timer = setTimeout(() => reject(new Error(`Timed out after ${timeoutMs}ms`)), timeoutMs);
  });
  try {
    return await Promise.race([promise, timeout]);
  } finally {
    if (timer !== undefined) clearTimeout(timer);
  }
};

const witnessFor = (testCase: CompilerCase): unknown => {
  const first = testCase.schemas[0]!;
  return schemaWitness(
    first,
    testCase.operation === "encoder" || testCase.operation === "asyncEncoder"
      ? "output"
      : "input",
  );
};

const recordCombination = (testCase: CompilerCase, coverage: Coverage): void => {
  const first = testCase.schemas[0]!;
  const last = testCase.schemas[testCase.schemas.length - 1]!;
  const encode = testCase.operation === "encoder" || testCase.operation === "asyncEncoder";
  const from = schemaCategory(first, encode ? "output" : "input");
  const to = schemaCategory(last, encode ? "input" : "output");
  coverage.hit(
    coverage.combinations,
    `${testCase.operation}:${testCase.schemas.length}:${from}->${to}`,
  );
};

export const runCase = async (
  testCase: CompilerCase,
  S: SuryApi,
  coverage: Coverage,
  timeoutMs: number,
): Promise<CaseResult> => {
  coverage.hit(coverage.operations, `${testCase.operation}:${testCase.schemas.length}`);
  recordCombination(testCase, coverage);

  let schemas: unknown[];
  try {
    schemas = testCase.schemas.map((schema) => compileSchema(schema, S, coverage));
  } catch (error) {
    if (isControlledError(error, S)) {
      const info = errorInfo(error);
      coverage.hit(coverage.outcomes, "expected-error:schema");
      return { status: "expected-error", ...info };
    }
    coverage.hit(coverage.outcomes, "bug:schema");
    return { status: "bug", failure: failure("schema", error) };
  }

  let operation: unknown;
  let cacheHit = false;
  try {
    operation = operationFactory(testCase.operation, schemas, S);
    const second = operationFactory(testCase.operation, schemas, S);
    cacheHit = operation === second;
  } catch (error) {
    if (isControlledError(error, S)) {
      const info = errorInfo(error);
      coverage.hit(coverage.outcomes, "expected-error:compile");
      return { status: "expected-error", ...info };
    }
    coverage.hit(coverage.outcomes, "bug:compile");
    return { status: "bug", failure: failure("compile", error) };
  }

  if (typeof operation !== "function") {
    const result = failure("compile", `S.${testCase.operation} returned ${typeof operation}`, "NonFunction");
    coverage.hit(coverage.outcomes, "bug:compile");
    return { status: "bug", failure: result };
  }

  const callable = operation as (input: unknown) => unknown;

  try {
    const source = Function.prototype.toString.call(callable);
    new Function(`return (${source})`);
  } catch (error) {
    coverage.hit(coverage.outcomes, "bug:source");
    return { status: "bug", failure: failure("source", error) };
  }

  if (!testCase.runWitness) {
    coverage.hit(coverage.outcomes, "compiled:skipped");
    return { status: "compiled", cacheHit, witness: "skipped" };
  }

  const witness = witnessFor(testCase);
  if (witness === NO_WITNESS) {
    coverage.hit(coverage.outcomes, "compiled:skipped");
    return { status: "compiled", cacheHit, witness: "skipped" };
  }

  try {
    const output = callable(witness);
    if (ASYNC_OPERATIONS.has(testCase.operation)) {
      if (!output || typeof (output as { then?: unknown }).then !== "function") {
        const result = failure(
          "runtime",
          `S.${testCase.operation} returned a non-Promise`,
          "NonPromise",
        );
        coverage.hit(coverage.outcomes, "bug:runtime");
        return { status: "bug", failure: result };
      }
      await withTimeout(Promise.resolve(output), timeoutMs);
    } else if (output && typeof (output as { then?: unknown }).then === "function") {
      const result = failure(
        "runtime",
        `S.${testCase.operation} unexpectedly returned a Promise`,
        "UnexpectedPromise",
      );
      coverage.hit(coverage.outcomes, "bug:runtime");
      return { status: "bug", failure: result };
    }
  } catch (error) {
    if (isControlledError(error, S)) {
      coverage.hit(coverage.outcomes, "compiled:sury-error");
      return { status: "compiled", cacheHit, witness: "sury-error" };
    }
    const info = errorInfo(error);
    const timedOut = info.message.startsWith("Timed out after ");
    coverage.hit(coverage.outcomes, timedOut ? "bug:timeout" : "bug:runtime");
    return {
      status: "bug",
      failure: failure(timedOut ? "timeout" : "runtime", error),
    };
  }

  coverage.hit(coverage.outcomes, "compiled:passed");
  return { status: "compiled", cacheHit, witness: "passed" };
};

const schemaShrinks = (schema: SchemaAst): SchemaAst[] => {
  const simplest: SchemaAst[] = [
    { kind: "primitive", name: "string" },
    { kind: "primitive", name: "unknown" },
  ];
  switch (schema.kind) {
    case "primitive":
      return schema.name === "string" ? [] : simplest;
    case "literal":
    case "enum":
    case "instance":
      return simplest;
    case "array":
      return [
        schema.item,
        ...schemaShrinks(schema.item).map((item): SchemaAst => ({ ...schema, item })),
      ];
    case "record":
      return [
        schema.value,
        ...schemaShrinks(schema.value).map((value): SchemaAst => ({ ...schema, value })),
      ];
    case "tuple":
      return [
        ...simplest,
        ...(schema.items.length > 1 ? [{ ...schema, items: schema.items.slice(0, 1) } as SchemaAst] : []),
        ...schema.items.flatMap((item, index) =>
          schemaShrinks(item).map((next) => ({
            ...schema,
            items: schema.items.map((current, itemIndex) => (itemIndex === index ? next : current)),
          })),
        ),
      ];
    case "object":
      return [
        ...simplest,
        ...(schema.fields.length > 1 ? [{ ...schema, fields: schema.fields.slice(0, 1) } as SchemaAst] : []),
        ...schema.fields.flatMap(([key, field], index) =>
          schemaShrinks(field).map((next) => ({
            ...schema,
            fields: schema.fields.map((current, fieldIndex) =>
              fieldIndex === index ? ([key, next] as [string, SchemaAst]) : current,
            ),
          })),
        ),
      ];
    case "union":
      return [
        ...schema.members,
        ...(schema.members.length > 2
          ? [{ ...schema, members: schema.members.slice(0, 2) } as SchemaAst]
          : []),
      ];
    case "optional":
    case "nullable":
    case "nullish":
    case "refine":
    case "modifier":
      return [schema.inner, ...schemaShrinks(schema.inner).map((inner) => ({ ...schema, inner }))];
    case "unary":
      return [
        schema.inner,
        ...schemaShrinks(schema.inner).map((inner): SchemaAst => ({ ...schema, inner })),
      ];
    case "merge":
      return [
        schema.left,
        schema.right,
        ...schemaShrinks(schema.left).map((left): SchemaAst => ({ ...schema, left })),
        ...schemaShrinks(schema.right).map((right): SchemaAst => ({ ...schema, right })),
      ];
    case "to":
      return [
        schema.source,
        schema.target,
        ...schemaShrinks(schema.source).map((source) => ({ ...schema, source })),
        ...schemaShrinks(schema.target).map((target) => ({ ...schema, target })),
        ...(schema.codec.kind === "custom-bidirectional"
          ? [
              {
                ...schema,
                codec: { kind: "custom-decoder" as const, decoder: schema.codec.decoder },
              },
            ]
          : []),
      ];
    case "recursive":
      return [schema.leaf, ...schemaShrinks(schema.leaf).map((leaf) => ({ ...schema, leaf }))];
  }
};

const caseShrinks = (testCase: CompilerCase): CompilerCase[] => {
  const cases: CompilerCase[] = [];
  if (testCase.schemas.length > 1) {
    cases.push({ ...testCase, schemas: [testCase.schemas[0]!] });
    cases.push({ ...testCase, schemas: testCase.schemas.slice(0, -1) });
  }
  for (let index = 0; index < testCase.schemas.length; index++) {
    for (const schema of schemaShrinks(testCase.schemas[index]!)) {
      cases.push({
        ...testCase,
        schemas: testCase.schemas.map((current, schemaIndex) =>
          schemaIndex === index ? schema : current,
        ),
      });
    }
  }
  return cases;
};

export const shrinkFailure = async (
  original: CompilerCase,
  targetSignature: string,
  S: SuryApi,
  timeoutMs: number,
  maxAttempts = 250,
): Promise<CompilerCase> => {
  let current = original;
  let attempts = 0;
  let changed = true;
  while (changed && attempts < maxAttempts) {
    changed = false;
    for (const candidate of caseShrinks(current)) {
      if (attempts++ >= maxAttempts) break;
      const result = await runCase(candidate, S, new Coverage(), timeoutMs);
      if (result.status === "bug" && result.failure.signature === targetSignature) {
        current = candidate;
        changed = true;
        break;
      }
    }
  }
  return current;
};

export const writeFailureArtifact = (
  directory: string,
  artifact: FailureArtifact,
): string => {
  mkdirSync(directory, { recursive: true });
  const safeId = artifact.minimized.id.replace(/[^a-zA-Z0-9_.-]+/g, "-");
  const path = join(directory, `${safeId}-${artifact.failure.signature.replace(":", "-")}.json`);
  writeFileSync(path, `${JSON.stringify(artifact, null, 2)}\n`);
  return path;
};

const printMap = (title: string, map: Map<string, number>): string[] => {
  const lines = [`${title}:`];
  for (const [key, count] of [...map].sort(([left], [right]) => left.localeCompare(right))) {
    lines.push(`  ${key.padEnd(48)} ${count}`);
  }
  return lines;
};

export const renderCoverage = (coverage: Coverage): string =>
  [
    `missing compiler APIs: ${COMPILER_API_NAMES.filter((name) => !coverage.api.has(name)).join(", ") || "none"}`,
    ...printMap("operations", coverage.operations),
    ...printMap("outcomes", coverage.outcomes),
    ...printMap("api", coverage.api),
    ...printMap("combinations", coverage.combinations),
  ].join("\n");

export const renderCase = (testCase: CompilerCase): string =>
  `S.${testCase.operation}(${testCase.schemas.map(renderSchema).join(", ")})`;

export const artifactFor = (
  seed: number | undefined,
  original: CompilerCase,
  minimized: CompilerCase,
  failureValue: Failure,
): FailureArtifact => ({
  artifactVersion: 1,
  createdAt: new Date().toISOString(),
  seed,
  original,
  minimized: { ...minimized, version: CASE_VERSION },
  failure: failureValue,
});
