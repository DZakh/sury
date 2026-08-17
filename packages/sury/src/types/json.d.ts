// The JSON data type, and the type a JSON Schema literal describes.
//
// `FromJSONSchema` is what gives `S.fromJSONSchema` its inferred result. It
// resolves a schema written inline; anything it cannot read statically — a
// value typed `unknown`, `JSON`, or one of the dialect interfaces in
// ./jsonschema.d.ts — resolves to `JSON`, so a schema loaded at runtime keeps
// working without a cast.

/**
 * Any value `JSON.parse` can return.
 */
export type JSON =
  | string
  | boolean
  | number
  | null
  | { [key: string]: JSON }
  | JSON[];

// A private copy of index.d.ts's `Flatten` — a non-exported type can't cross a
// file, and exporting one would put `S.Flatten` in the public API.
type Flatten<T> = T extends object ? { [K in keyof T]: T[K] } : T;

type JSONSchemaDefs<S> = S extends { $defs: infer D }
  ? D
  : S extends { definitions: infer D }
  ? D
  : {};

type JSONSchemaRefName<R> = R extends `#/$defs/${infer N}`
  ? N
  : R extends `#/definitions/${infer N}`
  ? N
  : never;

// `[…] extends [never]` first: a non-local pointer must fall back to `JSON`,
// and letting a bare `never` reach the `keyof D` check would resolve it.
type JSONSchemaRef<R, D, M extends boolean> = [JSONSchemaRefName<R>] extends [never]
  ? JSON
  : JSONSchemaRefName<R> extends keyof D
  ? JSONSchemaResolve<D[JSONSchemaRefName<R>], D, M>
  : JSON;

// The `string extends K` guard catches a `required` widened to `string[]` —
// e.g. by a `satisfies S.JSONSchema` annotation on the argument — where
// treating every key as required would type the result narrower than the
// runtime. Widened means unknowable, so no key is marked required.
type JSONSchemaRequiredKeys<S> = S extends { required: ReadonlyArray<infer K extends string> }
  ? string extends K
    ? never
    : K
  : never;

type JSONSchemaAdditionalProperty<S, D, M extends boolean> = S extends {
  additionalProperties: infer A;
}
  ? A extends true
    ? JSON
    : A extends false
      ? never
      : JSONSchemaResolve<A, D, M>
  : JSON;

type JSONSchemaRecord<S, V> = [JSONSchemaRequiredKeys<S>] extends [never]
  ? { [key: string]: V }
  : { [key: string]: V } & { [K in JSONSchemaRequiredKeys<S>]: V };

// Required/optional split by key remapping, not index.d.ts's `ResolveObject`:
// its `undefined extends TFields[keyof TFields]` probe forces every field type
// eagerly, which turns a recursive `$ref` through a property into a
// circular-reference error. Same required-first shape as `ResolveObject`.
type JSONSchemaObject<S, D, M extends boolean> = S extends { properties: infer P }
  ? Flatten<
      {
        -readonly [K in keyof P as K extends JSONSchemaRequiredKeys<S>
          ? K
          : never]: JSONSchemaResolve<P[K], D, M>;
      } & {
        -readonly [K in keyof P as K extends JSONSchemaRequiredKeys<S> ? never : K]?:
          | JSONSchemaResolve<P[K], D, M>
          | undefined;
      } & {
        [K in Exclude<JSONSchemaRequiredKeys<S>, keyof P>]: JSONSchemaAdditionalProperty<S, D, M>;
      }
    >
  : S extends { additionalProperties: infer A }
  ? A extends true
    ? JSONSchemaRecord<S, JSON>
    : A extends false
    ? JSONSchemaRecord<S, never>
    : JSONSchemaRecord<S, JSONSchemaResolve<A, D, M>>
  : JSONSchemaRecord<S, JSON>;

type JSONSchemaOptionalTuple<
  P extends readonly unknown[],
  D,
  M extends boolean,
> = {
  -readonly [K in keyof P]?: JSONSchemaResolve<P[K], D, M>;
};

type JSONSchemaTupleWithRequired<
  P extends readonly unknown[],
  D,
  M extends boolean,
  Min extends number,
  Acc extends unknown[] = [],
> = number extends Min
  ? JSONSchemaOptionalTuple<P, D, M>
  : Acc["length"] extends Min
    ? JSONSchemaOptionalTuple<P, D, M>
    : P extends readonly [infer H, ...infer T]
      ? [
          JSONSchemaResolve<H, D, M>,
          ...JSONSchemaTupleWithRequired<T, D, M, Min, [...Acc, unknown]>,
        ]
      : [];

type JSONSchemaRest<I, D, M extends boolean> = I extends false
  ? never
  : I extends true
  ? JSON
  : I extends readonly unknown[]
  ? JSON
  : JSONSchemaResolve<I, D, M>;

type JSONSchemaNaturalGreater<
  A extends number,
  B extends number,
> = `${A}` extends `${bigint}`
  ? `${B}` extends `${bigint}`
    ? JSONSchemaIntegerStringGreater<`${A}`, `${B}`>
    : false
  : false;

type JSONSchemaStringLength<S extends string, A extends unknown[] = []> =
  S extends `${infer _}${infer R}`
    ? JSONSchemaStringLength<R, [...A, unknown]>
    : A;

type JSONSchemaTupleGreater<A extends unknown[], B extends unknown[]> =
  A extends [unknown, ...infer AR]
    ? B extends [unknown, ...infer BR]
      ? JSONSchemaTupleGreater<AR, BR>
      : true
    : false;

type JSONSchemaDigitGreater<A extends string, B extends string> = A extends "9"
  ? B extends "9" ? false : true
  : A extends "8"
    ? B extends "8" | "9" ? false : true
    : A extends "7"
      ? B extends "7" | "8" | "9" ? false : true
      : A extends "6"
        ? B extends "6" | "7" | "8" | "9" ? false : true
        : A extends "5"
          ? B extends "5" | "6" | "7" | "8" | "9" ? false : true
          : A extends "4"
            ? B extends "4" | "5" | "6" | "7" | "8" | "9" ? false : true
            : A extends "3"
              ? B extends "3" | "4" | "5" | "6" | "7" | "8" | "9" ? false : true
              : A extends "2"
                ? B extends "0" | "1" ? true : false
                : A extends "1"
                  ? B extends "0" ? true : false
                  : false;

type JSONSchemaEqualLengthStringGreater<A extends string, B extends string> =
  A extends `${infer AH}${infer AR}`
    ? B extends `${infer BH}${infer BR}`
      ? AH extends BH
        ? JSONSchemaEqualLengthStringGreater<AR, BR>
        : JSONSchemaDigitGreater<AH, BH>
      : false
    : false;

type JSONSchemaIntegerStringGreater<A extends string, B extends string> =
  JSONSchemaStringLength<A> extends infer AL extends unknown[]
    ? JSONSchemaStringLength<B> extends infer BL extends unknown[]
      ? AL["length"] extends BL["length"]
        ? JSONSchemaEqualLengthStringGreater<A, B>
        : JSONSchemaTupleGreater<AL, BL>
      : false
    : false;

type JSONSchemaGreaterOne<A extends number, B extends number> = number extends A | B
  ? false
  : `${A}` extends `-${infer AbsA extends number}`
    ? `${B}` extends `-${infer AbsB extends number}`
      ? JSONSchemaNaturalGreater<AbsB, AbsA>
      : false
    : `${B}` extends `-${number}`
      ? true
      : JSONSchemaNaturalGreater<A, B>;

// Distributing both sides makes a widened union return `boolean`; callers only
// narrow when every possible pair proves the same relation.
type JSONSchemaGreater<A extends number, B extends number> = A extends A
  ? B extends B
    ? JSONSchemaGreaterOne<A, B>
    : never
  : never;

type JSONSchemaEqual<A, B> = A extends A
  ? B extends B
    ? [A] extends [B]
      ? [B] extends [A]
        ? true
        : false
      : false
    : never
  : never;

type JSONSchemaRepeat<
  E,
  N extends number,
  Acc extends unknown[] = [],
> = Acc["length"] extends N
  ? Acc
  : Acc["length"] extends 64
    ? E[]
    : JSONSchemaRepeat<E, N, [...Acc, E]>;

type JSONSchemaTupleRest<S, P extends readonly unknown[], I, D, M extends boolean> =
  I extends false
    ? []
    : S extends { maxItems: infer Max extends number }
    ? JSONSchemaGreater<P["length"], Max> extends true
      ? []
      : JSONSchemaEqual<P["length"], Max> extends true
        ? []
        : JSONSchemaRest<I, D, M>[]
    : JSONSchemaRest<I, D, M>[];

type JSONSchemaTuple<S, P extends readonly unknown[], I, D, M extends boolean> =
  S extends { minItems: infer Min extends number }
    ? // `items: false` caps the length at the prefix, so a `minItems` past it
      // leaves no length an array can have. The `maxItems` spelling of the same
      // emptiness is `JSONSchemaArrayBounds`.
      (I extends false ? JSONSchemaGreater<Min, P["length"]> : false) extends true
      ? never
      : [
          ...JSONSchemaTupleWithRequired<P, D, M, Min>,
          ...JSONSchemaTupleRest<S, P, I, D, M>,
        ]
    : S extends { maxItems: number }
      ? [
          ...JSONSchemaOptionalTuple<P, D, M>,
          ...JSONSchemaTupleRest<S, P, I, D, M>,
        ]
      : [...JSONSchemaOptionalTuple<P, D, M>, ...JSONSchemaRest<I, D, M>[]];

type JSONSchemaArrayBase<S, D, M extends boolean> = S extends {
  prefixItems: infer P extends readonly unknown[];
}
  ? JSONSchemaTuple<S, P, S extends { items: infer I } ? I : true, D, M>
  : S extends { items: infer I }
  ? I extends readonly unknown[]
    ? JSONSchemaTuple<
        S,
        I,
        S extends { additionalItems: infer A } ? A : true,
        D,
        M
      >
    : JSONSchemaResolve<I, D, M>[]
  : JSON[];

type JSONSchemaHasPositionalItems<S> = S extends {
  prefixItems: readonly unknown[];
}
  ? true
  : S extends { items: readonly unknown[] }
    ? true
    : false;

type JSONSchemaApplyArrayLength<
  S,
  T,
  N extends number,
> = number extends N
  ? T
  : N extends 0
    ? []
    : JSONSchemaHasPositionalItems<S> extends true
      ? T
      : T extends (infer E)[]
        ? JSONSchemaRepeat<E, N>
        : T;

type JSONSchemaArrayBounds<
  S,
  T,
  Max extends number,
> = S extends { minItems: infer Min extends number }
  ? JSONSchemaGreater<Min, Max> extends true
    ? never
    : JSONSchemaEqual<Min, Max> extends true
      ? JSONSchemaApplyArrayLength<S, T, Min>
      : T
  : [Max] extends [0]
    ? []
    : T;

type JSONSchemaArray<S, D, M extends boolean> = S extends {
  maxItems: infer Max extends number;
}
  ? JSONSchemaArrayBounds<S, JSONSchemaArrayBase<S, D, M>, Max>
  : JSONSchemaArrayBase<S, D, M>;

// Undoes the `readonly` a `const T` call site stamps onto `enum`/`const`
// values, the same way index.d.ts's `UnknownToOutput` does for `S.schema`.
// One branch covers arrays too: the homomorphic mapped type keeps a tuple a
// tuple.
type JSONSchemaLiteral<C> = C extends object
  ? { -readonly [K in keyof C]: JSONSchemaLiteral<C[K]> }
  : C;

type JSONSchemaUnion<A extends readonly unknown[], D, M extends boolean> = {
  [K in keyof A]: JSONSchemaResolve<A[K], D, M>;
}[number];

type JSONSchemaString<S> = S extends { maxLength: infer Max extends number }
  ? S extends { minLength: infer Min extends number }
    ? JSONSchemaGreater<Min, Max> extends true
      ? never
      : [Max] extends [0]
        ? ""
        : string
    : [Max] extends [0]
      ? ""
      : string
  : string;

type JSONSchemaBoundsImpossiblePair<
  Lower extends number,
  LowerExclusive extends boolean,
  Upper extends number,
  UpperExclusive extends boolean,
> = JSONSchemaGreater<Lower, Upper> extends true
  ? true
  : JSONSchemaEqual<Lower, Upper> extends true
    ? true extends LowerExclusive | UpperExclusive
      ? true
      : false
    : false;

// draft-04 uses boolean exclusivity flags beside minimum/maximum, while
// draft-06+ use numeric exclusive bounds; both spellings remain accepted.
type JSONSchemaNumber<S> = S extends {
  exclusiveMinimum: infer Lower extends number;
  exclusiveMaximum: infer Upper extends number;
}
  ? JSONSchemaBoundsImpossiblePair<Lower, true, Upper, true> extends true
    ? never
    : number
  : S extends {
        exclusiveMinimum: infer Lower extends number;
        maximum: infer Upper extends number;
      }
    ? JSONSchemaBoundsImpossiblePair<
        Lower,
        true,
        Upper,
        S extends { exclusiveMaximum: true } ? true : false
      > extends true
      ? never
      : number
    : S extends {
          minimum: infer Lower extends number;
          exclusiveMaximum: infer Upper extends number;
        }
      ? JSONSchemaBoundsImpossiblePair<
          Lower,
          S extends { exclusiveMinimum: true } ? true : false,
          Upper,
          true
        > extends true
        ? never
        : number
      : S extends {
            minimum: infer Lower extends number;
            maximum: infer Upper extends number;
          }
        ? JSONSchemaBoundsImpossiblePair<
            Lower,
            S extends { exclusiveMinimum: true } ? true : false,
            Upper,
            S extends { exclusiveMaximum: true } ? true : false
          > extends true
          ? never
          : number
        : number;

type JSONSchemaTypeNameToType<N, S, D, M extends boolean> = N extends "object"
  ? JSONSchemaObject<S, D, M>
  : N extends "array"
  ? JSONSchemaArray<S, D, M>
  : N extends "string"
  ? JSONSchemaString<S>
  : N extends "number" | "integer"
  ? JSONSchemaNumber<S>
  : N extends "boolean"
  ? boolean
  : N extends "null"
  ? null
  : JSON;

// Member-by-member fold: intersecting `UnionToIntersection` of the flattened
// member union would collapse any union-producing member (`enum`, `anyOf`,
// `nullable`) to `never` instead of intersecting it with its siblings.
type JSONSchemaIntersection<A, D, M extends boolean> = A extends readonly [infer H, ...infer T]
  ? JSONSchemaResolve<H, D, M> & JSONSchemaIntersection<T, D, M>
  : unknown;

type JSONSchemaConstrain<T, U> = JSON extends T ? U : JSON extends U ? T : T & U;

type JSONSchemaApplyCompositions<S, D, M extends boolean, T> = S extends {
  allOf: infer A extends readonly unknown[];
}
  ? A extends readonly []
    ? JSONSchemaApplyAnyOf<S, D, M, T>
    : JSONSchemaApplyAnyOf<
        S,
        D,
        M,
        JSONSchemaConstrain<T, JSONSchemaIntersection<A, D, M>>
      >
  : JSONSchemaApplyAnyOf<S, D, M, T>;

type JSONSchemaApplyAnyOf<S, D, M extends boolean, T> = S extends {
  anyOf: infer A extends readonly unknown[];
}
  ? A extends readonly []
    ? JSONSchemaApplyOneOf<S, D, M, never>
    : JSONSchemaApplyOneOf<S, D, M, JSONSchemaConstrain<T, JSONSchemaUnion<A, D, M>>>
  : JSONSchemaApplyOneOf<S, D, M, T>;

type JSONSchemaApplyOneOf<S, D, M extends boolean, T> = S extends {
  oneOf: infer A extends readonly unknown[];
}
  ? A extends readonly []
    ? never
    : JSONSchemaConstrain<T, JSONSchemaUnion<A, D, M>>
  : T;

type JSONSchemaApplyValues<S, D, M extends boolean, T> = S extends {
  enum: infer E extends readonly unknown[];
}
  ? JSONSchemaApplyConst<S, D, M, JSONSchemaConstrain<T, JSONSchemaLiteral<E[number]>>>
  : JSONSchemaApplyConst<S, D, M, T>;

type JSONSchemaApplyConst<S, D, M extends boolean, T> = S extends { const: infer C }
  ? JSONSchemaApplyCompositions<S, D, M, JSONSchemaConstrain<T, JSONSchemaLiteral<C>>>
  : JSONSchemaApplyCompositions<S, D, M, T>;

type JSONSchemaResolve<S, D, M extends boolean> = S extends true
  ? JSON
  : S extends false
  ? never
  : string extends keyof S
  ? JSON
  : S extends { nullable: true }
  ? null | JSONSchemaResolveNonNullable<S, D, M>
  : JSONSchemaResolveNonNullable<S, D, M>;

// The continuation after the `nullable` branch avoids `Omit<S, "nullable">`,
// which would re-map every key and burn recursion-depth budget per level.
type JSONSchemaResolveBase<S, D, M extends boolean> = S extends { type: "object" }
  ? JSONSchemaObject<S, D, M>
  : S extends { type: "array" }
  ? JSONSchemaArray<S, D, M>
  : S extends { type: infer N }
  ? N extends readonly unknown[]
    ? JSONSchemaTypeNameToType<N[number], S, D, M>
    : JSONSchemaTypeNameToType<N, S, D, M>
  : JSON;

type JSONSchemaResolveNonNullable<S, D, M extends boolean> = S extends { $ref: infer R }
  ? M extends true
    ? JSONSchemaApplyValues<
        S,
        D,
        M,
        JSONSchemaConstrain<JSONSchemaRef<R, D, M>, JSONSchemaResolveBase<S, D, M>>
      >
    : JSONSchemaRef<R, D, M>
  : S extends { not: infer N }
    ? N extends object
      ? keyof N extends never
        ? never
        : JSONSchemaApplyValues<S, D, M, JSONSchemaResolveBase<S, D, M>>
      : N extends true
        ? never
        : JSONSchemaApplyValues<S, D, M, JSONSchemaResolveBase<S, D, M>>
    : JSONSchemaApplyValues<S, D, M, JSONSchemaResolveBase<S, D, M>>;

type JSONSchemaModernDialect =
  | `${"http" | "https"}://json-schema.org/draft/2019-09/schema${"" | "#"}`
  | `${"http" | "https"}://json-schema.org/draft/2020-12/schema${"" | "#"}`;

type JSONSchemaRefSiblings<S> = S extends { $schema: infer U extends string }
  ? U extends JSONSchemaModernDialect
    ? true
    : false
  : false;

type JSONSchemaOutputRef<R, D, M extends boolean> = [JSONSchemaRefName<R>] extends [never]
  ? JSON
  : JSONSchemaRefName<R> extends keyof D
    ? JSONSchemaResolveOutput<D[JSONSchemaRefName<R>], D, M>
    : JSON;

type JSONSchemaOutputAdditionalProperty<S, D, M extends boolean> = S extends {
  additionalProperties: infer A;
}
  ? A extends true
    ? JSON
    : A extends false
      ? never
      : JSONSchemaResolveOutput<A, D, M>
  : JSON;

// A `default` the property's own schema rejects is an annotation the runtime
// can't fill in with (see `withDefault` in src/jsonschema.ts), so it leaves the
// key optional. Resolving the property is only paid for on a key that has one.
type JSONSchemaOutputRequiredKeys<S, P, D, M extends boolean> = Extract<
  keyof P,
  | JSONSchemaRequiredKeys<S>
  | {
      [K in keyof P]: P[K] extends { default: infer V }
        ? V extends JSONSchemaResolve<P[K], D, M>
          ? K
          : never
        : never;
    }[keyof P]
>;

type JSONSchemaOutputNativeObject<S, P, D, M extends boolean> = Flatten<
  {
    -readonly [K in keyof P as K extends JSONSchemaOutputRequiredKeys<S, P, D, M>
      ? K
      : never]: JSONSchemaResolveOutput<P[K], D, M>;
  } & {
    -readonly [K in keyof P as K extends JSONSchemaOutputRequiredKeys<S, P, D, M> ? never : K]?:
      | JSONSchemaResolveOutput<P[K], D, M>
      | undefined;
  } & {
    [K in Exclude<JSONSchemaRequiredKeys<S>, keyof P>]: JSONSchemaOutputAdditionalProperty<
      S,
      D,
      M
    >;
  }
>;

type JSONSchemaOutputObject<S, D, M extends boolean> = S extends { properties: infer P }
  ? S extends { additionalProperties: infer A }
    ? A extends false
      ? JSONSchemaOutputNativeObject<S, P, D, M>
      : JSONSchemaObject<S, D, M>
    : JSONSchemaOutputNativeObject<S, P, D, M>
  : S extends { additionalProperties: infer A }
    ? A extends true
      ? JSONSchemaRecord<S, JSON>
      : A extends false
        ? JSONSchemaRecord<S, never>
        : JSONSchemaRecord<S, JSONSchemaResolveOutput<A, D, M>>
    : JSONSchemaRecord<S, JSON>;

type JSONSchemaOutputOptionalTuple<
  P extends readonly unknown[],
  D,
  M extends boolean,
> = {
  -readonly [K in keyof P]?: JSONSchemaResolveOutput<P[K], D, M>;
};

type JSONSchemaOutputTupleWithRequired<
  P extends readonly unknown[],
  D,
  M extends boolean,
  Min extends number,
  Acc extends unknown[] = [],
> = number extends Min
  ? JSONSchemaOutputOptionalTuple<P, D, M>
  : Acc["length"] extends Min
    ? JSONSchemaOutputOptionalTuple<P, D, M>
    : P extends readonly [infer H, ...infer T]
      ? [
          JSONSchemaResolveOutput<H, D, M>,
          ...JSONSchemaOutputTupleWithRequired<T, D, M, Min, [...Acc, unknown]>,
        ]
      : [];

type JSONSchemaOutputRest<I, D, M extends boolean> = I extends false
  ? never
  : I extends true
    ? JSON
    : I extends readonly unknown[]
      ? JSON
      : JSONSchemaResolveOutput<I, D, M>;

type JSONSchemaOutputTupleRest<
  S,
  P extends readonly unknown[],
  I,
  D,
  M extends boolean,
> = I extends false
  ? []
  : S extends { maxItems: infer Max extends number }
    ? JSONSchemaGreater<P["length"], Max> extends true
      ? []
      : JSONSchemaEqual<P["length"], Max> extends true
        ? []
        : JSONSchemaOutputRest<I, D, M>[]
    : JSONSchemaOutputRest<I, D, M>[];

type JSONSchemaOutputTuple<
  S,
  P extends readonly unknown[],
  I,
  D,
  M extends boolean,
> = S extends { minItems: infer Min extends number }
  ? [
      ...JSONSchemaOutputTupleWithRequired<P, D, M, Min>,
      ...JSONSchemaOutputTupleRest<S, P, I, D, M>,
    ]
  : S extends { maxItems: number }
    ? [
        ...JSONSchemaOutputOptionalTuple<P, D, M>,
        ...JSONSchemaOutputTupleRest<S, P, I, D, M>,
      ]
    : [...JSONSchemaOutputOptionalTuple<P, D, M>, ...JSONSchemaOutputRest<I, D, M>[]];

type JSONSchemaOutputPositionalArray<
  S,
  P extends readonly unknown[],
  I,
  D,
  M extends boolean,
> = S extends { minItems: infer Min extends number }
  ? number extends Min
    ? JSONSchemaTuple<S, P, I, D, M>
    : JSONSchemaGreater<P["length"], Min> extends true
      ? JSONSchemaTuple<S, P, I, D, M>
      : I extends false
        ? // Min >= the prefix and nothing may follow it, so the tuple compiles
          // natively at exactly the prefix length — or not at all.
          JSONSchemaEqual<Min, P["length"]> extends true
          ? JSONSchemaOutputTuple<S, P, I, D, M>
          : never
        : S extends { maxItems: infer Max extends number }
          ? number extends Max
            ? JSONSchemaTuple<S, P, I, D, M>
            : JSONSchemaGreater<Max, P["length"]> extends true
              ? JSONSchemaTuple<S, P, I, D, M>
              : JSONSchemaOutputTuple<S, P, I, D, M>
          : JSONSchemaTuple<S, P, I, D, M>
  : JSONSchemaTuple<S, P, I, D, M>;

type JSONSchemaOutputArrayBase<S, D, M extends boolean> = S extends {
  prefixItems: infer P extends readonly unknown[];
}
  ? JSONSchemaOutputPositionalArray<
      S,
      P,
      S extends { items: infer I } ? I : true,
      D,
      M
    >
  : S extends { items: infer I }
    ? I extends readonly unknown[]
      ? JSONSchemaOutputPositionalArray<
          S,
          I,
          S extends { additionalItems: infer A } ? A : true,
          D,
          M
        >
      : JSONSchemaResolveOutput<I, D, M>[]
    : JSON[];

type JSONSchemaOutputArray<S, D, M extends boolean> = S extends {
  maxItems: infer Max extends number;
}
  ? JSONSchemaArrayBounds<S, JSONSchemaOutputArrayBase<S, D, M>, Max>
  : JSONSchemaOutputArrayBase<S, D, M>;

type JSONSchemaOutputTypeName<N, S, D, M extends boolean> = N extends "object"
  ? JSONSchemaOutputObject<S, D, M>
  : N extends "array"
    ? JSONSchemaOutputArray<S, D, M>
    : JSONSchemaTypeNameToType<N, S, D, M>;

type JSONSchemaOutputUnion<A extends readonly unknown[], D, M extends boolean> = {
  [K in keyof A]: JSONSchemaResolveOutput<A[K], D, M>;
}[number];

type JSONSchemaOutputIntersection<A, D, M extends boolean> = A extends readonly [
  infer H,
  ...infer T,
]
  ? JSONSchemaResolveOutput<H, D, M> & JSONSchemaOutputIntersection<T, D, M>
  : unknown;

type JSONSchemaOutputCompositions<S, D, M extends boolean, T> = S extends {
  allOf: infer A extends readonly unknown[];
}
  ? A extends readonly []
    ? JSONSchemaOutputAnyOf<S, D, M, T>
    : JSONSchemaOutputAnyOf<
        S,
        D,
        M,
        JSONSchemaConstrain<T, JSONSchemaOutputIntersection<A, D, M>>
      >
  : JSONSchemaOutputAnyOf<S, D, M, T>;

type JSONSchemaOutputAnyOf<S, D, M extends boolean, T> = S extends {
  anyOf: infer A extends readonly unknown[];
}
  ? A extends readonly []
    ? JSONSchemaOutputOneOf<S, D, M, never>
    : JSONSchemaOutputOneOf<
        S,
        D,
        M,
        JSONSchemaConstrain<T, JSONSchemaOutputUnion<A, D, M>>
      >
  : JSONSchemaOutputOneOf<S, D, M, T>;

type JSONSchemaOutputOneOf<S, D, M extends boolean, T> = S extends {
  oneOf: infer A extends readonly unknown[];
}
  ? A extends readonly []
    ? never
    : JSONSchemaConstrain<T, JSONSchemaOutputUnion<A, D, M>>
  : T;

type JSONSchemaOutputValues<S, D, M extends boolean, T> = S extends {
  enum: infer E extends readonly unknown[];
}
  ? JSONSchemaOutputConst<S, D, M, JSONSchemaConstrain<T, JSONSchemaLiteral<E[number]>>>
  : JSONSchemaOutputConst<S, D, M, T>;

type JSONSchemaOutputConst<S, D, M extends boolean, T> = S extends { const: infer C }
  ? JSONSchemaOutputCompositions<S, D, M, JSONSchemaConstrain<T, JSONSchemaLiteral<C>>>
  : JSONSchemaOutputCompositions<S, D, M, T>;

type JSONSchemaResolveOutputBase<S, D, M extends boolean> = S extends { type: "object" }
  ? JSONSchemaOutputObject<S, D, M>
  : S extends { type: "array" }
    ? JSONSchemaOutputArray<S, D, M>
    : S extends { type: infer N }
      ? N extends readonly unknown[]
        ? JSONSchemaOutputTypeName<N[number], S, D, M>
        : JSONSchemaOutputTypeName<N, S, D, M>
      : JSON;

type JSONSchemaResolveOutputNonNullable<S, D, M extends boolean> = S extends { $ref: infer R }
  ? M extends true
    ? JSONSchemaOutputValues<
        S,
        D,
        M,
        JSONSchemaConstrain<JSONSchemaOutputRef<R, D, M>, JSONSchemaResolveOutputBase<S, D, M>>
      >
    : JSONSchemaOutputRef<R, D, M>
  : S extends { not: infer N }
    ? N extends object
      ? keyof N extends never
        ? never
        : JSONSchemaOutputValues<S, D, M, JSONSchemaResolveOutputBase<S, D, M>>
      : N extends true
        ? never
        : JSONSchemaOutputValues<S, D, M, JSONSchemaResolveOutputBase<S, D, M>>
    : JSONSchemaOutputValues<S, D, M, JSONSchemaResolveOutputBase<S, D, M>>;

type JSONSchemaResolveOutput<S, D, M extends boolean> = S extends true
  ? JSON
  : S extends false
    ? never
    : string extends keyof S
      ? JSON
      : S extends { nullable: true }
        ? null | JSONSchemaResolveOutputNonNullable<S, D, M>
        : JSONSchemaResolveOutputNonNullable<S, D, M>;

// Only schemas reached by a native decoder can change the output. Composition
// members and mixed properties/additionalProperties are validation-only, so
// treating defaults inside them as transformations would make this type claim
// required values that runtime never inserts.
type JSONSchemaHasDefault<S, D, A extends unknown[] = []> = A["length"] extends 16
  ? false
  : S extends { default: unknown }
    ? true
    : S extends { $ref: infer R }
      ? JSONSchemaRefName<R> extends keyof D
        ? JSONSchemaHasDefault<D[JSONSchemaRefName<R>], D, [...A, unknown]>
        : false
      : S extends { properties: infer P }
        ? true extends {
            [K in keyof P]: JSONSchemaHasDefault<P[K], D, [...A, unknown]>;
          }[keyof P]
          ? true
          : false
        : S extends { prefixItems: infer P extends readonly unknown[] }
          ? true extends JSONSchemaHasDefault<P[number], D, [...A, unknown]>
            ? true
            : S extends { items: infer I }
              ? JSONSchemaHasDefault<I, D, [...A, unknown]>
              : false
          : S extends { items: infer I }
            ? JSONSchemaHasDefault<
                I extends readonly unknown[] ? I[number] : I,
                D,
                [...A, unknown]
              >
            : S extends { additionalProperties: infer I }
              ? JSONSchemaHasDefault<I, D, [...A, unknown]>
              : false;

/**
 * The type a JSON Schema literal describes, as inferred by
 * `S.fromJSONSchema`. Resolves local `$ref` pointers (`#/$defs/…`,
 * `#/definitions/…`) against the root schema, including recursive and
 * mutually recursive ones. A `$ref` on any other path (`#/components/schemas/…`)
 * is validated the same, but resolves to `S.JSON` here — as does a non-literal
 * schema (`unknown`, `S.JSON`, a dialect interface).
 */
export type FromJSONSchema<T> = unknown extends T
  ? JSON
  : JSONSchemaResolve<T, JSONSchemaDefs<T>, JSONSchemaRefSiblings<T>>;

export type FromJSONSchemaOutput<T> = unknown extends T
  ? JSON
  : JSONSchemaHasDefault<T, JSONSchemaDefs<T>> extends true
    ? JSONSchemaResolveOutput<T, JSONSchemaDefs<T>, JSONSchemaRefSiblings<T>>
    : FromJSONSchema<T>;
