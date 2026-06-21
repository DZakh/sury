// @ts-check
//
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

const SuryUnion = S.union([{ box: S.string }, S.string.with(S.to, S.number)]);
const ValibotUnion = v.union([
  v.object({
    box: v.string(),
  }),
  v.pipe(v.string(), v.decimal(), v.transform(Number)),
]);
const ArkTypeUnion = type({ box: "string" }).or("string.numeric.parse");
const ZodUnion = z.union([
  z.object({ box: z.string() }),
  z.string().pipe(z.coerce.number()),
]);
const TypeBoxUnion = Type.Union([
  Type.Object({ box: Type.String() }),
  Type.Transform(Type.String())
    .Decode(Number)
    .Encode((v) => v.toString()),
]);
const suryUnionParse = S.parser(SuryUnion);

describe("create", () => {
  bench("Sury", () => {
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
  bench("Zod", () => {
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
  bench("Valibot", () => {
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
  bench("ArkType", () => {
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
  bench("TypeBox", () => {
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
  bench("Sury", () => {
    suryParse(data);
  });
  bench("Zod", () => {
    zodSchema.parse(data);
  });
  bench("Valibot", () => {
    v.parse(valibotSchema, data);
  });
  bench("ArkType", () => {
    arkTypeSchema(data);
  });
  bench("TypeBox", () => {
    if (!typeBoxSchema.Check(data)) {
      throw new Error(typeBoxSchema.Errors(data).First()?.message);
    }
  });
});

describe("create + parse", () => {
  bench("Sury", () => {
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
  bench("Zod", () => {
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
  bench("Valibot", () => {
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
  bench("ArkType", () => {
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
  bench("TypeBox", () => {
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
  bench("Sury", () => {
    suryUnionParse("123");
  });
  bench("Zod", () => {
    ZodUnion.parse("123");
  });
  bench("Valibot", () => {
    v.parse(ValibotUnion, "123");
  });
  bench("ArkType", () => {
    ArkTypeUnion("123");
  });
  bench("TypeBox", () => {
    Value.Decode(TypeBoxUnion, "123");
  });
});
