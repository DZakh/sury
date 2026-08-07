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

type UnionToIntersection<U> = (U extends unknown ? (k: U) => void : never) extends (
  k: infer I
) => void
  ? I
  : never;

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
type JSONSchemaRef<R, D> = [JSONSchemaRefName<R>] extends [never]
  ? JSON
  : JSONSchemaRefName<R> extends keyof D
  ? JSONSchemaResolve<D[JSONSchemaRefName<R>], D>
  : JSON;

type JSONSchemaRequiredKeys<S> = S extends { required: ReadonlyArray<infer K extends string> }
  ? K
  : never;

// Required/optional split by key remapping, not index.d.ts's `ResolveObject`:
// its `undefined extends TFields[keyof TFields]` probe forces every field type
// eagerly, which turns a recursive `$ref` through a property into a
// circular-reference error. Same required-first shape as `ResolveObject`.
type JSONSchemaObject<S, D> = S extends { properties: infer P }
  ? Flatten<
      {
        -readonly [K in keyof P as K extends JSONSchemaRequiredKeys<S>
          ? K
          : never]: JSONSchemaResolve<P[K], D>;
      } & {
        -readonly [K in keyof P as K extends JSONSchemaRequiredKeys<S> ? never : K]?:
          | JSONSchemaResolve<P[K], D>
          | undefined;
      }
    >
  : S extends { additionalProperties: infer A }
  ? A extends true
    ? { [key: string]: JSON }
    : A extends false
    ? {}
    : { [key: string]: JSONSchemaResolve<A, D> }
  : {};

type JSONSchemaArray<S, D> = S extends { prefixItems: infer P extends readonly unknown[] }
  ? { -readonly [K in keyof P]: JSONSchemaResolve<P[K], D> }
  : S extends { items: infer I }
  ? I extends readonly unknown[]
    ? { -readonly [K in keyof I]: JSONSchemaResolve<I[K], D> }
    : JSONSchemaResolve<I, D>[]
  : JSON[];

// Undoes the `readonly` a `const T` call site stamps onto `enum`/`const`
// values, the same way index.d.ts's `UnknownToOutput` does for `S.schema`.
type JSONSchemaLiteral<C> = C extends readonly unknown[]
  ? { -readonly [K in keyof C]: JSONSchemaLiteral<C[K]> }
  : C extends object
  ? { -readonly [K in keyof C]: JSONSchemaLiteral<C[K]> }
  : C;

type JSONSchemaUnion<A extends readonly unknown[], D> = A extends readonly []
  ? JSON
  : { [K in keyof A]: JSONSchemaResolve<A[K], D> }[number];

type JSONSchemaTypeNameToType<N, S, D> = N extends "object"
  ? JSONSchemaObject<S, D>
  : N extends "array"
  ? JSONSchemaArray<S, D>
  : N extends "string"
  ? string
  : N extends "number" | "integer"
  ? number
  : N extends "boolean"
  ? boolean
  : N extends "null"
  ? null
  : JSON;

// First-match dispatch in the same order as the runtime chain in
// src/jsonschema.ts (nullable → type:"object" → type:"array" → anyOf → enum →
// const → type[] → scalar type → JSON fallback), so a keyword the runtime
// ignores in a given position is ignored here too. `$ref` resolves ahead of it
// — the one place the static type leads the runtime, which still parses a
// `$ref` as plain JSON. The `string extends keyof S` guard sends
// index-signature values (e.g. the object arm of `JSON` itself) to the
// fallback before any keyword can match structurally.
type JSONSchemaResolve<S, D> = S extends boolean
  ? S extends true
    ? JSON
    : never
  : string extends keyof S
  ? JSON
  : S extends { $ref: infer R }
  ? JSONSchemaRef<R, D>
  : S extends { nullable: true }
  ? null | JSONSchemaResolve<Omit<S, "nullable">, D>
  : S extends { type: "object" }
  ? JSONSchemaObject<S, D>
  : S extends { type: "array" }
  ? JSONSchemaArray<S, D>
  : S extends { anyOf: infer A extends readonly unknown[] }
  ? JSONSchemaUnion<A, D>
  : S extends { enum: infer E extends readonly unknown[] }
  ? JSONSchemaLiteral<E[number]>
  : S extends { const: infer C }
  ? JSONSchemaLiteral<C>
  : S extends { type: infer N }
  ? N extends readonly unknown[]
    ? JSONSchemaTypeNameToType<N[number], S, D>
    : JSONSchemaTypeNameToType<N, S, D>
  : S extends { oneOf: infer A extends readonly unknown[] }
  ? JSONSchemaUnion<A, D>
  : S extends { allOf: infer A extends readonly unknown[] }
  ? Flatten<UnionToIntersection<JSONSchemaUnion<A, D>>>
  : JSON;

/**
 * The type a JSON Schema literal describes, as inferred by
 * `S.fromJSONSchema`. Resolves local `$ref` pointers (`#/$defs/…`,
 * `#/definitions/…`) against the root schema, including recursive and
 * mutually recursive ones. A non-literal schema — `unknown`, `S.JSON`, a
 * dialect interface — resolves to `S.JSON`.
 */
export type FromJSONSchema<T> = unknown extends T
  ? JSON
  : JSONSchemaResolve<T, JSONSchemaDefs<T>>;
