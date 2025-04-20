import B from "benchmark";
import { z } from "zod";
import * as v from "valibot";
import * as S from "sury/src/S.js";
import { type } from "arktype";

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

const arkType = type({
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

const RescriptSchemaUnion = S.union([
  { box: S.string },
  S.to(S.string, S.number),
]);
// S.parseOrThrow("123", RescriptSchemaUnion)
// rescript-schema@9.3.0 x 82,715,204 ops/sec ±2.11% (86 runs sampled)

const ValibotUnion = v.union([
  v.object({
    box: v.string(),
  }),
  v.pipe(v.string(), v.decimal(), v.transform(Number)),
]);
// v.parse(ValibotUnion, "123")
// Valibot@1.0.0-rc.1 x 5,507,472 ops/sec ±0.38% (98 runs sampled)

const ArkTypeUnion = type({ box: "string" }).or("string.numeric.parse");
// ArkTypeUnion("123")
// ArkType@2.0.4 x 4,300,118 ops/sec ±0.33% (97 runs sampled)
// ArkType@2.1.0 x 28,938,669 ops/sec ±0.84% (98 runs sampled)

const ZodUnion = z.union([
  z.object({ box: z.string() }),
  z.string().pipe(z.coerce.number()),
]);
// ZodUnion.parse("123")
// Zod@3.24.2 x 3,278,494 ops/sec ±0.55% (94 runs sampled)

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

S.setGlobalConfig({
  disableNanNumberValidation: true,
});
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
const parseOrThrow = S.compile(schema, "Input", "Output", "Sync", true);

new B.Suite()
  // 1,820,280 ops/sec
  .add("rescript-schema (create)", () => {
    return S.schema({
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
  })
  .add("rescript-schema (parse)", () => {
    return S.parseOrThrow(data, schema);
  })
  .add("rescript-schema (precompiled parse)", () => {
    return parseOrThrow(data, schema);
  })
  .add("rescript-schema (create + parse)", () => {
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
    return S.parseOrThrow(data, schema);
  })
  .add("rescript-schema (union)", () => {
    return S.parseOrThrow("123", RescriptSchemaUnion);
  })
  .add("Zod (create)", () => {
    return z.object({
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
  })
  .add("Zod (parse)", () => {
    return zodSchema.parse(data);
  })
  .add("Zod (create + parse)", () => {
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
    return zodSchema.parse(data);
  })
  .add("Zod (union)", () => {
    return ZodUnion.parse("123");
  })
  .add("Valibot (create)", () => {
    return v.object({
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
  })
  .add("Valibot (parse)", () => {
    return v.parse(valibotSchema, data);
  })
  .add("Valibot (create + parse)", () => {
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
    return v.parse(valibotSchema, data);
  })
  .add("Valibot (union)", () => {
    return v.parse(ValibotUnion, "123");
  })
  .add("ArkType (create)", () => {
    return type({
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
  })
  .add("ArkType (parse)", () => {
    return arkType(data);
  })
  .add("ArkType (create + parse)", () => {
    const arkType = type({
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
    return arkType(data);
  })
  .add("ArkType (union)", () => {
    return ArkTypeUnion("123");
  })
  .on("cycle", (event) => {
    console.log(String(event.target));
  })
  .run();
