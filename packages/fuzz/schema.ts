import { codecFunction, renderValue, valueFromAst } from "./catalog";
import type { PrimitiveName, SchemaAst, ValueAst } from "./types";

export type SuryApi = Record<string, any>;

export const COMPILER_API_NAMES = [
  "string", "any", "boolean", "int32", "number", "nan", "bigint", "symbol", "void",
  "never", "unknown", "json", "jsonString", "jsonStringWithSpace", "uint8Array",
  "isoDateTime", "port", "email", "uuid", "cuid", "url", "date", "literal", "enum",
  "instance", "array", "tuple", "record", "schema", "object", "union", "optional",
  "nullable", "nullish", "recursive", "strict", "deepStrict", "strip", "deepStrip",
  "noValidation", "meta", "brand", "refine", "gt", "gte", "lt", "lte", "minLength",
  "maxLength", "length", "empty", "nonEmpty", "pattern", "trim", "compactColumns",
  "list", "shape", "asyncDecoderAssert", "reverse", "merge", "to", "to:builtin",
  "to:custom-decoder", "to:custom-bidirectional",
] as const;

export class Coverage {
  readonly api = new Map<string, number>();
  readonly operations = new Map<string, number>();
  readonly combinations = new Map<string, number>();
  readonly outcomes = new Map<string, number>();

  hit(map: Map<string, number>, key: string): void {
    map.set(key, (map.get(key) ?? 0) + 1);
  }

  hitApi(key: string): void {
    this.hit(this.api, key);
  }
}

const primitive = (S: SuryApi, name: PrimitiveName): unknown => {
  if (name === "void") return S.void;
  if (name === "jsonStringWithSpace") return S.jsonStringWithSpace(2);
  return S[name];
};

class FuzzInstance {
  readonly value = "fuzz";
}

const refinementPredicate = (
  name: Extract<SchemaAst, { kind: "refine" }>["refinement"],
): ((value: unknown) => boolean) => {
  switch (name) {
    case "always":
      return () => true;
    case "never":
      return () => false;
    case "non-empty":
      return (value) =>
        (typeof value === "string" || Array.isArray(value)) && value.length > 0;
    case "non-negative":
      return (value) =>
        (typeof value === "number" || typeof value === "bigint") && value >= 0;
    case "min-length":
    case "max-length":
    case "gte":
    case "lte":
    case "gt":
    case "lt":
    case "length":
    case "empty":
    case "nonEmpty":
    case "pattern":
      return () => true;
  }
};

export const compileSchema = (ast: SchemaAst, S: SuryApi, coverage: Coverage): any => {
  switch (ast.kind) {
    case "primitive": {
      coverage.hitApi(ast.name);
      const schema = primitive(S, ast.name);
      if (schema === undefined) throw new Error(`Missing public Sury API S.${ast.name}`);
      return schema;
    }
    case "literal":
      coverage.hitApi("literal");
      return S.literal(valueFromAst(ast.value));
    case "enum":
      coverage.hitApi("enum");
      return S.enum(ast.values.map(valueFromAst));
    case "instance":
      coverage.hitApi("instance");
      return S.instance(FuzzInstance);
    case "array":
      coverage.hitApi("array");
      return S.array(compileSchema(ast.item, S, coverage));
    case "tuple":
      coverage.hitApi("tuple");
      return S.tuple(ast.items.map((item) => compileSchema(item, S, coverage)));
    case "record":
      coverage.hitApi("record");
      return S.record(compileSchema(ast.value, S, coverage));
    case "object": {
      coverage.hitApi(ast.factory);
      const definition = Object.fromEntries(
        ast.fields.map(([key, field]) => [key, compileSchema(field, S, coverage)]),
      );
      return S[ast.factory](definition);
    }
    case "union":
      coverage.hitApi("union");
      return S.union(ast.members.map((member) => compileSchema(member, S, coverage)));
    case "optional":
    case "nullable":
    case "nullish":
      coverage.hitApi(ast.kind);
      return S[ast.kind](compileSchema(ast.inner, S, coverage));
    case "refine": {
      const schema = compileSchema(ast.inner, S, coverage);
      switch (ast.refinement) {
        case "min-length":
          coverage.hitApi("minLength");
          return S.minLength(schema, ast.argument ?? 1);
        case "max-length":
          coverage.hitApi("maxLength");
          return S.maxLength(schema, ast.argument ?? 1);
        case "gte":
        case "lte":
        case "gt":
        case "lt":
        case "length":
          coverage.hitApi(ast.refinement);
          return S[ast.refinement](schema, ast.argument ?? 0);
        case "empty":
        case "nonEmpty":
          coverage.hitApi(ast.refinement);
          return S[ast.refinement](schema);
        case "pattern":
          coverage.hitApi("pattern");
          return S.pattern(schema, /^fuzz/);
        default:
          coverage.hitApi("refine");
          return S.refine(schema, refinementPredicate(ast.refinement), {
            error: `fuzz:${ast.refinement}`,
          });
      }
    }
    case "modifier": {
      coverage.hitApi(ast.modifier);
      const schema = compileSchema(ast.inner, S, coverage);
      if (ast.modifier === "noValidation") return S.noValidation(schema, true);
      if (ast.modifier === "meta")
        return S.meta(schema, { description: "compiler fuzz metadata" });
      return S[ast.modifier](schema);
    }
    case "unary": {
      coverage.hitApi(ast.api);
      const schema = compileSchema(ast.inner, S, coverage);
      switch (ast.api) {
        case "brand":
          return S.brand(schema, "FuzzBrand");
        case "shape":
          return S.shape(schema, codecFunction(ast.codec ?? "identity"));
        case "asyncDecoderAssert":
          return S.asyncDecoderAssert(schema, async () => undefined);
        default:
          return S[ast.api](schema);
      }
    }
    case "merge":
      coverage.hitApi("merge");
      return S.merge(
        compileSchema(ast.left, S, coverage),
        compileSchema(ast.right, S, coverage),
      );
    case "to": {
      coverage.hitApi("to");
      const source = compileSchema(ast.source, S, coverage);
      const target = compileSchema(ast.target, S, coverage);
      switch (ast.codec.kind) {
        case "builtin":
          coverage.hitApi("to:builtin");
          return S.to(source, target);
        case "custom-decoder":
          coverage.hitApi("to:custom-decoder");
          return S.to(source, target, codecFunction(ast.codec.decoder));
        case "custom-bidirectional":
          coverage.hitApi("to:custom-bidirectional");
          return S.to(
            source,
            target,
            codecFunction(ast.codec.decoder),
            codecFunction(ast.codec.encoder),
          );
      }
    }
    case "recursive":
      coverage.hitApi("recursive");
      return S.recursive(ast.name, (self: unknown) =>
        S.union([compileSchema(ast.leaf, S, coverage), S.array(self)]),
      );
  }
};

export type SchemaCategory =
  | "string"
  | "number"
  | "bigint"
  | "boolean"
  | "symbol"
  | "undefined"
  | "object"
  | "array"
  | "date"
  | "unknown"
  | "never"
  | "union";

const primitiveCategory = (name: PrimitiveName): SchemaCategory => {
  switch (name) {
    case "string":
    case "jsonString":
    case "jsonStringWithSpace":
    case "isoDateTime":
    case "email":
    case "uuid":
    case "cuid":
    case "url":
      return "string";
    case "number":
    case "int32":
    case "port":
    case "nan":
      return "number";
    case "bigint":
      return "bigint";
    case "boolean":
      return "boolean";
    case "symbol":
      return "symbol";
    case "void":
      return "undefined";
    case "date":
      return "date";
    case "uint8Array":
      return "object";
    case "json":
    case "unknown":
    case "any":
      return "unknown";
    case "never":
      return "never";
  }
};

export const schemaCategory = (
  ast: SchemaAst,
  side: "input" | "output",
): SchemaCategory => {
  switch (ast.kind) {
    case "primitive":
      return primitiveCategory(ast.name);
    case "literal":
      switch (ast.value.kind) {
        case "string":
          return "string";
        case "number":
          return "number";
        case "bigint":
          return "bigint";
        case "boolean":
          return "boolean";
        case "undefined":
          return "undefined";
        case "array":
          return "array";
        case "object":
        case "null":
          return "object";
      }
    case "enum":
      return "union";
    case "instance":
      return "object";
    case "array":
    case "tuple":
      return "array";
    case "record":
    case "object":
      return "object";
    case "union":
    case "optional":
    case "nullable":
    case "nullish":
    case "recursive":
      return "union";
    case "refine":
    case "modifier":
      return schemaCategory(ast.inner, side);
    case "unary":
      if (ast.api === "reverse") {
        return schemaCategory(ast.inner, side === "input" ? "output" : "input");
      }
      if (ast.api === "compactColumns") return "array";
      if (ast.api === "list") return side === "input" ? "array" : "object";
      if (ast.api === "shape" && side === "output") return "unknown";
      return schemaCategory(ast.inner, side);
    case "merge":
      return "object";
    case "to":
      return schemaCategory(side === "input" ? ast.source : ast.target, side);
  }
};

const literalWitness = (value: ValueAst): unknown => valueFromAst(value);

const primitiveWitness = (name: PrimitiveName): unknown => {
  switch (name) {
    case "string":
      return "fuzz";
    case "any":
      return null;
    case "boolean":
      return true;
    case "number":
    case "int32":
      return 1;
    case "port":
      return 80;
    case "nan":
      return NaN;
    case "bigint":
      return 1n;
    case "symbol":
      return Symbol.for("sury-fuzz");
    case "void":
      return undefined;
    case "never":
      return NO_WITNESS;
    case "unknown":
    case "json":
      return null;
    case "jsonString":
    case "jsonStringWithSpace":
      return "null";
    case "uint8Array":
      return new Uint8Array();
    case "isoDateTime":
      return "2024-01-01T00:00:00.000Z";
    case "email":
      return "fuzz@example.com";
    case "uuid":
      return "00000000-0000-4000-8000-000000000000";
    case "cuid":
      return "clh7q8x9y0000qzrmn831i7rn";
    case "url":
      return "https://example.com";
    case "date":
      return new Date(0);
  }
};

export const NO_WITNESS = Symbol("no-witness");

export const schemaWitness = (ast: SchemaAst, side: "input" | "output"): unknown => {
  switch (ast.kind) {
    case "primitive":
      return primitiveWitness(ast.name);
    case "literal":
      return literalWitness(ast.value);
    case "enum":
      return ast.values.length ? literalWitness(ast.values[0]!) : NO_WITNESS;
    case "instance":
      return new FuzzInstance();
    case "array":
    case "record":
      return ast.kind === "array" ? [] : {};
    case "tuple": {
      const values = ast.items.map((item) => schemaWitness(item, side));
      return values.some((value) => value === NO_WITNESS) ? NO_WITNESS : values;
    }
    case "object": {
      const values = ast.fields.map(([key, field]) => [key, schemaWitness(field, side)] as const);
      if (values.some(([, value]) => value === NO_WITNESS)) return NO_WITNESS;
      return Object.fromEntries(values);
    }
    case "union":
      for (const member of ast.members) {
        const value = schemaWitness(member, side);
        if (value !== NO_WITNESS) return value;
      }
      return NO_WITNESS;
    case "optional":
    case "nullable":
    case "nullish":
    case "refine":
    case "modifier":
      return schemaWitness(ast.inner, side);
    case "unary":
      if (ast.api === "reverse") {
        return schemaWitness(ast.inner, side === "input" ? "output" : "input");
      }
      if (ast.api === "compactColumns") return [];
      if (ast.api === "list") return side === "input" ? [] : 0;
      if (ast.api === "shape" && side === "output") return NO_WITNESS;
      return schemaWitness(ast.inner, side);
    case "merge": {
      const left = schemaWitness(ast.left, side);
      const right = schemaWitness(ast.right, side);
      return left && right && typeof left === "object" && typeof right === "object"
        ? { ...left, ...right }
        : NO_WITNESS;
    }
    case "to":
      return schemaWitness(side === "input" ? ast.source : ast.target, side);
    case "recursive":
      return schemaWitness(ast.leaf, side);
  }
};

export const renderSchema = (ast: SchemaAst): string => {
  switch (ast.kind) {
    case "primitive":
      return `S.${ast.name}`;
    case "literal":
      return `S.literal(${renderValue(ast.value)})`;
    case "enum":
      return `S.enum([${ast.values.map(renderValue).join(", ")}])`;
    case "instance":
      return "S.instance(FuzzInstance)";
    case "array":
      return `S.array(${renderSchema(ast.item)})`;
    case "tuple":
      return `S.tuple([${ast.items.map(renderSchema).join(", ")}])`;
    case "record":
      return `S.record(${renderSchema(ast.value)})`;
    case "object":
      return `S.${ast.factory}({${ast.fields
        .map(([key, field]) => `${JSON.stringify(key)}: ${renderSchema(field)}`)
        .join(", ")}})`;
    case "union":
      return `S.union([${ast.members.map(renderSchema).join(", ")}])`;
    case "optional":
    case "nullable":
    case "nullish":
      return `S.${ast.kind}(${renderSchema(ast.inner)})`;
    case "refine":
      return ast.refinement === "min-length"
        ? `S.minLength(${renderSchema(ast.inner)}, ${ast.argument})`
        : ast.refinement === "max-length"
          ? `S.maxLength(${renderSchema(ast.inner)}, ${ast.argument})`
          : ["gte", "lte", "gt", "lt", "length"].includes(ast.refinement)
            ? `S.${ast.refinement}(${renderSchema(ast.inner)}, ${ast.argument})`
            : ast.refinement === "empty" || ast.refinement === "nonEmpty"
              ? `S.${ast.refinement}(${renderSchema(ast.inner)})`
              : ast.refinement === "pattern"
                ? `S.pattern(${renderSchema(ast.inner)}, /^fuzz/)`
                : `S.refine(${renderSchema(ast.inner)}, <${ast.refinement}>)`;
    case "modifier":
      return ast.modifier === "noValidation"
        ? `S.noValidation(${renderSchema(ast.inner)}, true)`
        : ast.modifier === "meta"
          ? `S.meta(${renderSchema(ast.inner)}, {description: "compiler fuzz metadata"})`
          : `S.${ast.modifier}(${renderSchema(ast.inner)})`;
    case "unary":
      return ast.api === "brand"
        ? `S.brand(${renderSchema(ast.inner)}, "FuzzBrand")`
        : ast.api === "shape"
          ? `S.shape(${renderSchema(ast.inner)}, <${ast.codec ?? "identity"}>)`
          : ast.api === "asyncDecoderAssert"
            ? `S.asyncDecoderAssert(${renderSchema(ast.inner)}, <async-assert>)`
            : `S.${ast.api}(${renderSchema(ast.inner)})`;
    case "merge":
      return `S.merge(${renderSchema(ast.left)}, ${renderSchema(ast.right)})`;
    case "to":
      return ast.codec.kind === "builtin"
        ? `S.to(${renderSchema(ast.source)}, ${renderSchema(ast.target)})`
        : ast.codec.kind === "custom-decoder"
          ? `S.to(${renderSchema(ast.source)}, ${renderSchema(ast.target)}, <${ast.codec.decoder}>)`
          : `S.to(${renderSchema(ast.source)}, ${renderSchema(ast.target)}, <${ast.codec.decoder}>, <${ast.codec.encoder}>)`;
    case "recursive":
      return `S.recursive(${JSON.stringify(ast.name)}, self => S.union([${renderSchema(ast.leaf)}, S.array(self)]))`;
  }
};
