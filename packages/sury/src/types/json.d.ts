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
        [K in Exclude<JSONSchemaRequiredKeys<S>, keyof P>]: JSONSchemaAdditionalProperty<
          S,
          D,
          M
        >;
      }
    >
  : S extends { additionalProperties: infer A }
  ? A extends true
    ? { [key: string]: JSON } & { [K in JSONSchemaRequiredKeys<S>]: JSON }
    : A extends false
    ? Record<string, never> & { [K in JSONSchemaRequiredKeys<S>]: never }
    : { [key: string]: JSONSchemaResolve<A, D, M> } & {
        [K in JSONSchemaRequiredKeys<S>]: JSONSchemaResolve<A, D, M>;
      }
  : { [key: string]: JSON } & { [K in JSONSchemaRequiredKeys<S>]: JSON };

type JSONSchemaOptionalTuple<P extends readonly unknown[], D, M extends boolean> = {
  -readonly [K in keyof P]?: JSONSchemaResolve<P[K], D, M>;
};

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
  Acc extends unknown[] = [],
> = `${A}` extends `${bigint}`
  ? `${B}` extends `${bigint}`
    ? Acc["length"] extends A
      ? false
      : Acc["length"] extends B
        ? true
        : Acc["length"] extends 64
          ? false
          : JSONSchemaNaturalGreater<A, B, [...Acc, unknown]>
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

type JSONSchemaTuple<P extends readonly unknown[], I, D, M extends boolean> = [
  ...JSONSchemaOptionalTuple<P, D, M>,
  ...JSONSchemaRest<I, D, M>[],
];

type JSONSchemaArrayBase<S, D, M extends boolean> = S extends {
  prefixItems: infer P extends readonly unknown[];
}
  ? JSONSchemaTuple<P, S extends { items: infer I } ? I : true, D, M>
  : S extends { items: infer I }
  ? I extends readonly unknown[]
    ? JSONSchemaTuple<
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
  ? JSONSchemaApplyOneOf<S, D, M, JSONSchemaConstrain<T, JSONSchemaUnion<A, D, M>>>
  : JSONSchemaApplyOneOf<S, D, M, T>;

type JSONSchemaApplyOneOf<S, D, M extends boolean, T> = S extends {
  oneOf: infer A extends readonly unknown[];
}
  ? JSONSchemaConstrain<T, JSONSchemaUnion<A, D, M>>
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

type JSONSchemaRefSiblings<S> = S extends { $schema: infer U extends string }
  ? U extends `${string}/draft/2020-12/${string}` | `${string}/draft/2019-09/${string}`
    ? true
    : false
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
