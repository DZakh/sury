# Content codec spec

**Proposal.** `CODEC_SPEC.md` governs built-in conversions, `CUSTOM_CODEC_SPEC.md`
custom coders. This file governs the pairs where **two** built-in readings exist:
a carrier (a value that stores other data) meeting a format that could either
rewrite it or open it.

## The problem

```ts
S.uint8Array.with(S.to, S.jsonString);
// pack:   base64            → '"ZGF0YQ=="'     the bytes are a value IN the document
// unpack: UTF-8 + JSON.parse                    the bytes ARE the document
```

Both are useful. Today the pair silently does a third, broken thing — UTF-8
escape, which corrupts any byte outside ASCII (`[137, 80, 78, 71]` → `"�PNG"` →
decodes back as different bytes, no error at either end).

## The API

Two values join `CUSTOM_CODEC_SPEC.md`'s `Conversion` union:

```ts
type Conversion<A, B> = Coder<A, B> | "auto" | "never" | "pack" | "unpack" | { async: Coder<A, Promise<B>> };
```

Each names what its direction does to its own source:

- `"unpack"` — open the source, hand its payload to the target.
- `"pack"` — store the source's value inside the target.

```ts
// the file holds a JSON config
S.file.with(S.to, configSchema, { decode: "unpack", encode: "pack" });
// Schema<File, Config> — asyncParser reads + parses, encoder builds the File back

// base64 inside a JSON string, into a File
S.jsonString.with(S.to, S.file, { decode: "unpack", encode: "pack" });
```

Mixing with `"auto"`, `"never"`, coders and `{async}` follows
`CUSTOM_CODEC_SPEC.md`. One extra creation check: `"unpack"` opposite `"unpack"`
(or `"pack"` opposite `"pack"`) is rejected — opening in both directions is
incoherent.

No bare-string shorthand: `.with(S.to, X, "unpack")` would mean a pair where a
bare function means one side (custom rule 3).

## Rule 1: an explicit slot wins

See above. Everything below is what happens when no slot is written.

## Rule 2: a value position packs

A field or array item is a value in the document — nothing asks it to be a
document of its own:

```ts
S.encoder(S.schema({ payload: S.uint8Array }), S.jsonString)({ payload: bytes });
// {"payload":"ZGF0YQ=="}                      base64, not mangled UTF-8
```

A `Blob`/`File` field packs the same way, which makes that **encode async** (a
Blob's bytes are only readable asynchronously) — `S.asyncEncoder`, and the sync
`S.encoder` fails at creation like any other async operation.

A field that should hold a *nested* document says so, and rule 3 takes over:

```ts
S.jsonString.with(S.to, S.schema({
  id: S.string,
  meta: S.jsonString.with(S.to, tagsSchema),   // the field's string IS a document
}));
// decode: meta is parsed, not re-escaped; encode: serialized back into a string
```

## Rule 3: a declared payload unpacks

Writing what's inside is how you say "open it". When the format names its
payload with `.to`, a carrier feeding it unpacks:

```ts
S.file.with(S.to, S.jsonString.with(S.to, configSchema));   // read + parse
S.file.with(S.to, S.jsonString).with(S.to, configSchema);   // same chain, same answer

S.base64.with(S.to, S.jsonString.with(S.to, claimsSchema)); // a JWT segment
```

The check is on the schema shape (`format` + `.to` present), so it is local and
stable: new built-in conversions in later versions can never retract it.

## Rule 4: otherwise, error

```ts
S.file.with(S.to, S.jsonString);        // which reading?
S.uint8Array.with(S.to, S.json);        // base64 value, or UTF-8 + parse?
```

Both fail at **operation creation** (like every conversion error), one message
built from the same pieces the existing unsupported-conversion error uses:

```
Ambiguous conversion from File to JSON string.
Use S.to(from, to, {decode: "unpack" | "pack", encode: ...})
```

There are no per-carrier defaults. When both readings are live, the library
asks.

## Never ambiguous

These skip the rules entirely:

```ts
S.jsonString.with(S.to, S.jsonString);   // identity (same schema)
S.email.with(S.to, S.string);            // widen — a constraint format has no payload
S.base64.with(S.to, S.string);           // widen — its payload is bytes, not a string
S.base64.with(S.to, S.uint8Array);       // payload transfer — one reading
S.file.with(S.to, S.string);             // payload transfer (text read, async)
S.uint8Array.with(S.to, S.string);       // UTF-8 — pack and unpack produce the SAME code
S.jsonString.with(S.to, userSchema);     // parse — a plain type can't be packed into
S.number.with(S.to, S.jsonString);       // pack — a number has nothing to open
```

The two-string contrast is the one thing to memorize, and it follows from the
payloads:

```ts
S.jsonString.with(S.to, S.string);  // parses — a string IS a JSON value
S.base64.with(S.to, S.string);      // identity — a string is NOT bytes
```

## Carriers

| carrier | payload | read | write |
| --- | --- | --- | --- |
| `S.blob`, `S.file` | bytes **and** text | async | sync |
| `S.uint8Array` | bytes | sync | sync |
| `S.base64` | bytes | sync | sync |
| `S.jsonString` (future: toon, env) | a JSON value | sync | sync |
| future: `S.formData`, protobuf | a record / a message | sync | sync |

Packing bytes into a JSON position always produces base64. Packing a `File`
loses its name — the reverse builds `new File([content], "")`; a name option
belongs on `S.file` itself, not on a slot.

## Reversal

`pack` and `unpack` trade places under `S.reverse`, like `parser`/`serializer`:
`{decode: "unpack", encode: "pack"}` reversed is `{decode: "pack",
encode: "unpack"}`, and double reversal restores both. Async rides the carrier:
a `File` unpack is an async decode, its reverse an async encode.

## JSON Schema

Derived from the link — nothing declared twice. Dialect gating follows the
`contentSchema` emit that already exists for `S.jsonString`:

| schema | draft-07 | draft-2020-12 | openapi-3.0 |
| --- | --- | --- | --- |
| `S.jsonString.with(S.to, X)` | `contentMediaType: "application/json"` | + `contentSchema: <X>` | bare string |
| `S.file.with(S.to, X, {decode: "unpack", …})` | + `contentMediaType` | + `contentSchema` | `format: "binary"` |
| bytes packed into a JSON position | `contentEncoding: "base64"` | same | `format: "byte"` |

Emitted through the per-schema `jsonSchema` hook `S.blob`/`S.file` already use.

## Implementation notes

**Resolution is syntactic — no representation tracking.** Every rule reads two
schema fields at operation creation:

| decision | check |
| --- | --- |
| is this a carrier / a content format? | a `content` marker the schema sets on itself |
| rule 2 (value position) | the call site — `fieldPiece` and the item paths already know |
| rule 3 (declared payload) | `schema.content && schema.to !== undefined` |
| nested jsonString fix | same check — `jsonStringDecoder`'s string branch starts asking what its unknown branch already asks |
| `contentSchema` emit | same check (shipping since the jsonString emit landed) |

No new `Val` fields, no hidden-class change, no compile-loop cost, nothing in
generated code that a hand-written converter wouldn't contain. The earlier idea
of tracking what a val carries through the pipeline is dead: `.to` on the format
schema *is* the payload declaration, and it's already there.

**Conversions live on the carrier, not the format** — the existing `S.date`
pattern. `S.uint8Array`'s encoder owns base64; `S.file`'s owns `.text()` /
`.arrayBuffer()`. So `S.jsonString` never references the base64 helper, and a
bundle that never mentions bytes never ships it. The format side checks only the
generic `content` marker — it never names toon, env, or any other format, which
is what keeps each future carrier a self-contained `advanced/` file.

**The base64 helper** feature-detects `Uint8Array.prototype.toBase64` /
`fromBase64` once at module init and embeds the chosen function, so generated
code is a single `e[N](i)` call either way. The fallback allocates an
intermediate string per value — worth a `scenarios.yaml` entry.

## Breaking changes

| today | after |
| --- | --- |
| `S.uint8Array.with(S.to, S.jsonString)` — UTF-8 escape (corrupts non-ASCII) | rule 4 error |
| `{payload: S.uint8Array}` in a JSON document — corrupts | base64 |
| `S.encoder(S.uint8Array.with(S.to, S.number))(42)` — returns `42` typed as `Uint8Array` | error (the decoder's missing fall-through, a standalone soundness fix) |
| every `Blob`/`File` conversion — creation error | works or asks |

All land before the slot values ship. After that release, changes only turn
errors into working code.

## Spec coverage

One spec per **carrier kind × direction**, plus one per format's own codec —
never per pair. Plus the cross-pairs that pin the payload rule, since they are
the least guessable: `base64 → string` vs `jsonString → string`,
`base64 → jsonString` (bare: error; with declared payload: JWT). Bytes carriers
use non-ASCII fixtures in both directions — ASCII-only fixtures are what hid
today's corruption.
