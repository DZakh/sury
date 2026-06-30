// Cross-library comparison benchmarks (Sury vs Zod / Valibot / ArkType /
// TypeBox). Run with `pnpm --filter=e2e benchmark:comparison`; instrumented by
// CodSpeed in CI. Each `describe` is a head-to-head group, so Vitest prints the
// relative "x faster" summary per operation.

import { bench, describe } from "vitest";
import { z } from "zod";
import * as v from "valibot";
import * as S from "sury/src/S.js";
import { type } from "arktype";
import { Type } from "@sinclair/typebox";
import { Value } from "@sinclair/typebox/value";
import { TypeCompiler } from "@sinclair/typebox/compiler";
import { TypeSystemPolicy } from "@sinclair/typebox/system";

// Align NaN handling across libraries so the comparison is apples-to-apples.
S.global({
  disableNanNumberValidation: true,
});
TypeSystemPolicy.AllowNaN = true;

const data = Object.freeze({
  number: 1,
  negNumber: -1,
  maxNumber: Number.MAX_VALUE,
  string: "string",
  longString:
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum. Vivendum intellegat et qui, ei denique consequuntur vix. Semper aeterno percipit ut his, sea ex utinam referrentur repudiandae. No epicuri hendrerit consetetur sit, sit dicta adipiscing ex, in facete detracto deterruisset duo. Quot populo ad qui. Sit fugit nostrum et. Ad per diam dicant interesset, lorem iusto sensibus ut sed. No dicam aperiam vis. Pri posse graeco definitiones cu, id eam populo quaestio adipiscing, usu quod malorum te. Ex nam agam veri, dicunt efficiantur ad qui, ad legere adversarium sit. Commune platonem mel id, brute adipiscing duo an. Vivendum intellegat et qui, ei denique consequuntur vix. Offendit eleifend moderatius ex vix, quem odio mazim et qui, purto expetendis cotidieque quo cu, veri persius vituperata ei nec. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.",
  boolean: true,
  deeplyNested: {
    foo: "bar",
    num: 1,
    bool: false,
  },
});

const surySchema = S.schema({
  number: S.number,
  negNumber: S.number,
  maxNumber: S.number,
  string: S.string,
  longString: S.string,
  boolean: S.boolean,
  deeplyNested: {
    foo: S.string,
    num: S.number,
    bool: S.boolean,
  },
});
// S.parser returns the compiled parse fn — the hot path users pay per call,
// equivalent to a pre-built `zodSchema.parse` / compiled TypeBox check.
const suryParse = S.parser(surySchema);

const zodSchema = z.object({
  number: z.number(),
  negNumber: z.number(),
  maxNumber: z.number(),
  string: z.string(),
  longString: z.string(),
  boolean: z.boolean(),
  deeplyNested: z.object({
    foo: z.string(),
    num: z.number(),
    bool: z.boolean(),
  }),
});

const valibotSchema = v.object({
  number: v.number(),
  negNumber: v.number(),
  maxNumber: v.number(),
  string: v.string(),
  longString: v.string(),
  boolean: v.boolean(),
  deeplyNested: v.object({
    foo: v.string(),
    num: v.number(),
    bool: v.boolean(),
  }),
});

const arkTypeSchema = type({
  number: "number",
  negNumber: "number",
  maxNumber: "number",
  string: "string",
  longString: "string",
  boolean: "boolean",
  deeplyNested: {
    foo: "string",
    num: "number",
    bool: "boolean",
  },
});

const typeBoxSchema = TypeCompiler.Compile(
  Type.Object({
    number: Type.Number(),
    negNumber: Type.Number(),
    maxNumber: Type.Number(),
    string: Type.String(),
    longString: Type.String(),
    boolean: Type.Boolean(),
    deeplyNested: Type.Object({
      foo: Type.String(),
      num: Type.Number(),
      bool: Type.Boolean(),
    }),
  })
);

// Historical Benchmark.js ops/sec for the union parse, kept for reference
// across library versions (newer CodSpeed runs track this going forward):
const SuryUnion = S.union([{ box: S.string }, S.string.with(S.to, S.number)]);
// S.parseOrThrow("123", SuryUnion)
// Sury@10.0.0 x 81,797,072 ops/sec ±2.19% (89 runs sampled)

const ValibotUnion = v.union([
  v.object({
    box: v.string(),
  }),
  v.pipe(v.string(), v.decimal(), v.transform(Number)),
]);
// v.parse(ValibotUnion, "123")
// Valibot@1.0.0 x 5,055,079 ops/sec ±1.52% (98 runs sampled)

const ArkTypeUnion = type({ box: "string" }).or("string.numeric.parse");
// ArkTypeUnion("123")
// ArkType@2.0.4 x 4,300,118 ops/sec ±0.33% (97 runs sampled)
// ArkType@2.1.20 x 28,353,756 ops/sec ±0.75% (98 runs sampled)

const ZodUnion = z.union([
  z.object({ box: z.string() }),
  z.string().pipe(z.coerce.number()),
]);
// ZodUnion.parse("123")
// Zod@3.24.2 x 3,278,494 ops/sec ±0.55% (94 runs sampled)
// Zod@4.0.0-beta x 13,112,988 ops/sec ±0.45% (95 runs sampled)

const TypeBoxUnion = Type.Union([
  Type.Object({ box: Type.String() }),
  Type.Transform(Type.String())
    .Decode(Number)
    .Encode((value) => value.toString()),
]);
// Value.Decode(TypeBoxUnion, "123")
// TypeBox@0.34.33 x 2,205,614 ops/sec ±26.92% (68 runs sampled)

const suryUnionParse = S.parser(SuryUnion);

describe("create", () => {
  bench("create: Sury", () => {
    S.schema({
      number: S.number,
      negNumber: S.number,
      maxNumber: S.number,
      string: S.string,
      longString: S.string,
      boolean: S.boolean,
      deeplyNested: {
        foo: S.string,
        num: S.number,
        bool: S.boolean,
      },
    });
  });
  bench("create: Zod", () => {
    z.object({
      number: z.number(),
      negNumber: z.number(),
      maxNumber: z.number(),
      string: z.string(),
      longString: z.string(),
      boolean: z.boolean(),
      deeplyNested: z.object({
        foo: z.string(),
        num: z.number(),
        bool: z.boolean(),
      }),
    });
  });
  bench("create: Valibot", () => {
    v.object({
      number: v.number(),
      negNumber: v.number(),
      maxNumber: v.number(),
      string: v.string(),
      longString: v.string(),
      boolean: v.boolean(),
      deeplyNested: v.object({
        foo: v.string(),
        num: v.number(),
        bool: v.boolean(),
      }),
    });
  });
  bench("create: ArkType", () => {
    type({
      number: "number",
      negNumber: "number",
      maxNumber: "number",
      string: "string",
      longString: "string",
      boolean: "boolean",
      deeplyNested: {
        foo: "string",
        num: "number",
        bool: "boolean",
      },
    });
  });
  bench("create: TypeBox", () => {
    TypeCompiler.Compile(
      Type.Object({
        number: Type.Number(),
        negNumber: Type.Number(),
        maxNumber: Type.Number(),
        string: Type.String(),
        longString: Type.String(),
        boolean: Type.Boolean(),
        deeplyNested: Type.Object({
          foo: Type.String(),
          num: Type.Number(),
          bool: Type.Boolean(),
        }),
      })
    );
  });
});

describe("parse", () => {
  bench("parse: Sury", () => {
    suryParse(data);
  });
  bench("parse: Zod", () => {
    zodSchema.parse(data);
  });
  bench("parse: Valibot", () => {
    v.parse(valibotSchema, data);
  });
  bench("parse: ArkType", () => {
    arkTypeSchema(data);
  });
  bench("parse: TypeBox", () => {
    if (!typeBoxSchema.Check(data)) {
      throw new Error(typeBoxSchema.Errors(data).First()?.message);
    }
  });
});

describe("create + parse", () => {
  bench("create + parse: Sury", () => {
    const schema = S.schema({
      number: S.number,
      negNumber: S.number,
      maxNumber: S.number,
      string: S.string,
      longString: S.string,
      boolean: S.boolean,
      deeplyNested: {
        foo: S.string,
        num: S.number,
        bool: S.boolean,
      },
    });
    S.parser(schema)(data);
  });
  bench("create + parse: Zod", () => {
    const schema = z.object({
      number: z.number(),
      negNumber: z.number(),
      maxNumber: z.number(),
      string: z.string(),
      longString: z.string(),
      boolean: z.boolean(),
      deeplyNested: z.object({
        foo: z.string(),
        num: z.number(),
        bool: z.boolean(),
      }),
    });
    schema.parse(data);
  });
  bench("create + parse: Valibot", () => {
    const schema = v.object({
      number: v.number(),
      negNumber: v.number(),
      maxNumber: v.number(),
      string: v.string(),
      longString: v.string(),
      boolean: v.boolean(),
      deeplyNested: v.object({
        foo: v.string(),
        num: v.number(),
        bool: v.boolean(),
      }),
    });
    v.parse(schema, data);
  });
  bench("create + parse: ArkType", () => {
    const schema = type({
      number: "number",
      negNumber: "number",
      maxNumber: "number",
      string: "string",
      longString: "string",
      boolean: "boolean",
      deeplyNested: {
        foo: "string",
        num: "number",
        bool: "boolean",
      },
    });
    schema(data);
  });
  bench("create + parse: TypeBox", () => {
    const schema = TypeCompiler.Compile(
      Type.Object({
        number: Type.Number(),
        negNumber: Type.Number(),
        maxNumber: Type.Number(),
        string: Type.String(),
        longString: Type.String(),
        boolean: Type.Boolean(),
        deeplyNested: Type.Object({
          foo: Type.String(),
          num: Type.Number(),
          bool: Type.Boolean(),
        }),
      })
    );
    if (!schema.Check(data)) {
      throw new Error(schema.Errors(data).First()?.message);
    }
  });
});

describe("union", () => {
  bench("union: Sury", () => {
    suryUnionParse("123");
  });
  bench("union: Zod", () => {
    ZodUnion.parse("123");
  });
  bench("union: Valibot", () => {
    v.parse(ValibotUnion, "123");
  });
  bench("union: ArkType", () => {
    ArkTypeUnion("123");
  });
  bench("union: TypeBox", () => {
    Value.Decode(TypeBoxUnion, "123");
  });
});
