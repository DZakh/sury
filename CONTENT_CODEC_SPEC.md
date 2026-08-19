# Content codec spec

**Implemented.** `CODEC_SPEC.md` governs built-in conversions and custom coders.
This file governs the pairs where **two** built-in readings exist: a carrier (a
value that stores other data) meeting a format that could either rewrite it or
open it.

`B_contentDiffers` and `B_readsPayload` in `src/builder.ts` are the two decisions
below; the conversions themselves live on the carriers
(`src/advanced/uint8Array.ts`, `src/advanced/file.ts`, `S.base64` in
`src/refinements.ts`). `docs/js-usage.md` carries the user-facing version under
"Content", and the `packages/sury/specs/` rows listed at the end snapshot the
behavior.

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

Two values join `CODEC_SPEC.md`'s `Conversion` union:

```ts
type Conversion<A, B> = Coder<A, B> | "auto" | "never" | "pack" | "unpack" | { async: Coder<A, Promise<B>> };
```

Each names what its direction does to its own source:

- `"unpack"` — open the source, hand its payload to the target.
- `"pack"` — store the source's value inside the target.

```ts
// the bytes ARE the JSON text, rather than a value stored in one
S.uint8Array.with(S.to, S.jsonString, { decode: "unpack", encode: "pack" });
// Schema<Uint8Array, string>

// base64 inside a JSON string, into a File
S.jsonString.with(S.to, S.file, { decode: "unpack", encode: "pack" });
```

A slot picks a reading; it does not invent a conversion. What the opened payload
then converts to is still whatever the rules below give, so a target that no
built-in conversion reaches is still rejected — reading a `File` into an object
schema needs the format that turns its text into one
(`S.file.with(S.to, S.jsonString.with(S.to, configSchema))`, rule 3), not a slot.

Mixing with `"never"`, coders and `{async}` follows `CODEC_SPEC.md` — each of
those answers for its own direction. `"auto"` does not: it leaves that direction
asking the question the reading just answered, so a reading has to be paired
with its opposite. `"unpack"` opposite `"unpack"` (or `"pack"` opposite `"pack"`)
is rejected for the mirror reason — opening in both directions leaves no side
holding the payload.

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

S.base64.with(S.to, S.jsonString.with(S.to, claimsSchema)); // a JWT segment
```

The check is on the schema shape (a `content` marker plus `.to` present), so it
is local and stable: new built-in conversions in later versions can never
retract it.

**The payload goes inside, not after.** Each link settles when it is written, so
`S.file.with(S.to, S.jsonString).with(S.to, configSchema)` is rule 4 on its first
link — at that point nothing has said what the JSON string carries, and the
second link can't reach back, because reversal would have lost the distinction
anyway (see the implementation notes). Write the payload inside the format, or
say which reading you meant with a slot.

## Rule 4: otherwise, error

```ts
S.file.with(S.to, S.jsonString);        // which reading?
```

Where no slot would resolve it either — a union, or `S.json` — the pair says it
has no decoder instead of naming a spelling that wouldn't work:

```ts
S.uint8Array.with(S.to, S.json);              // Can't decode Uint8Array to JSON
S.uint8Array.with(S.to, S.optional(S.jsonString));
```

Both fail at **operation creation** (like every conversion error), one message
built from the same pieces the existing unsupported-conversion error uses:

```
Ambiguous conversion from File to JSON string. Use S.to(from, to, {decode: "unpack" | "pack", encode: ...})
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

Derived from the link — nothing declared twice. `toJSONSchema` describes a
schema's *input*, so the annotations land on whichever side of a link is the
string; a `Blob` input has no document, and keeps saying so.

| schema | draft-07 / 2020-12 | openapi-3.0 |
| --- | --- | --- |
| `S.base64` | `contentEncoding: "base64"` | `format: "byte"` |
| `S.jsonString.with(S.to, X)` | `contentMediaType: "application/json"`, plus `contentSchema: <X>` in 2020-12 | bare string |
| `S.string.with(S.to, S.blob)` | `contentMediaType: "application/octet-stream"` | `format: "binary"` |
| `S.jsonString.with(S.to, S.file, {decode: "unpack", …})` | `contentMediaType: "application/json"` | `format: "binary"` |
| `S.base64.with(S.to, S.file)` | both of the above | `format: "byte"` |

A carrier's own emit fills only what the string hasn't already said about
itself, which is why the last two rows keep the encoding they are written in
rather than the medium they end in. The base64 rows round-trip through
`fromJSONSchema`, from either spelling; a `contentMediaType` does not yet, and
comes back as a plain string. The
`contentSchema` emit is gated on a json-format source, so a base64 segment
carrying a document annotates the encoding it is stored in and stops there.

**The axis stops at a union, and at `S.json`.** A union carries neither `content`
nor `.to` of its own, and neither an arm's payload declaration nor a reading
written on the union reaches the dispatch; `S.json` is the document rather than
a rendering of one, so neither reading of a link to it is built. Both are read
far enough to know the pair is not a plain transfer, and then rejected as having
no decoder — which a custom coder still answers. Rule 2 is the exception on both
counts, and only because `jsonEncoderFn` rewrites the arms itself and reads
`content` for the shape a document stores its target as.

**Not yet:** a packed bytes field has no document form — `S.toJSONSchema` of
`S.schema({payload: S.uint8Array})` reports `Expected JSON, received Uint8Array`
rather than describing the base64 string the field becomes. That follows the
existing rule that a carrier has no document of its own (`S.blob` answers the
same way); making `content` answer it instead is a separate change.

## Implementation notes

**Resolution is syntactic, and it happens where the link is written.** Two
schema fields carry it:

| field | meaning |
| --- | --- |
| `content` | the schema this value's payload is stored as inside a JSON document — `S.base64` for every bytes carrier, `S.json` for `S.jsonString` and `S.json` itself. Absent means the value carries no payload. |
| `opens` | the reading a `"pack"`/`"unpack"` slot wrote, on the schema that direction converts *into* — so `reverse`, which copies node by node, carries each direction's slot with it. |

Two schemas that agree on `content` carry the same kind of payload, so a link
between them is a plain transfer; two that disagree have both readings live, and
`B_readsPayload` answers which one applies: the slot if there is one, otherwise
the target naming its own payload with `.to` (rule 3).

`B_contentDiffers` asks rule 4's question at the two places a `.to` link is made
— `codecTo` for a written `S.to`, `getDecoder` for an operation given its own
target — and the empty direction takes a slot that rejects the operation. It
cannot be left to compile time, because reversing a chain turns the target's
payload declaration into just another link: the legal `X -> jsonString -> File`
and the rejected `jsonString -> File` reach the decoder as the same pair.

What the rejection *says* is the caller's, not the question's. `codecTo` names
the slots, because the caller has somewhere to write one; `getDecoder`'s form
has nowhere, so it reports the pair as having no decoder — which a coder still
answers. And `codecTo` says the same for a pair no slot resolves: a union arm's
payload and a reading written on the union both stop short of the dispatch, and
`S.json` has no opened form of its own.

Everything else is the two markers being read where a decision already happened:
rule 2 is the carrier's `encoder` being handed a content-format target,
`jsonEncoderFn`'s fallback uses `content` for the shape a document stores the
target as instead of assuming `string`, and the nested-jsonString fix is
`B_narrowJsonSourcedJsonString` no longer stopping at a field whose jsonString
declares a payload. No new `Val` fields, no compile-loop cost, nothing in
generated code a hand-written converter wouldn't contain.

The one price is the universal path: `getDecoder` is in every bundle, so
`B_contentDiffers` is too, and `copySchema` and `reverse` each carry a line for
the two markers. About 190 gzipped bytes on every export
(`bundleSize.yaml`, where the smallest go 4136 → 4329) — the question and one
closure; the messages ride with the callers. That buys a
creation-time gate on conversions that otherwise corrupt data silently.

**Conversions live on the carrier, not the format** — the existing `S.date`
pattern. `S.uint8Array`'s encoder owns base64; `S.file`'s owns `.text()` /
`.arrayBuffer()`; `S.base64` owns opening itself into text. So `S.jsonString`
never references a base64 helper, and a bundle that never mentions bytes never
ships one. The format side checks only the generic `content` marker — it never
names toon, env, or any other format, which is what keeps each future carrier a
self-contained file.

`S.fromJSONSchema` pays for the round trip: reading `contentEncoding` back means
naming `S.base64`, which brings its pattern and its conversions — +704 gzipped
on that one export, the largest row in `bundleSize.yaml`. Every format costs it
something; this is the first that carries a codec.

**The base64 helpers** feature-detect `Uint8Array.prototype.toBase64` /
`Uint8Array.fromBase64` once at import and embed the chosen function, so
generated code is a single `e[N](i)` call either way. The fallback bridges
through `btoa`/`atob` and an intermediate binary string, in 8KB chunks — the
whole array blows `String.fromCharCode`'s argument limit and a byte at a time is
several times slower than either. `scenarios.yaml`'s `base64-pack` /
`base64-unpack` are what that costs a consumer.

## Breaking changes

| today | after |
| --- | --- |
| `S.uint8Array.with(S.to, S.jsonString)` — UTF-8 escape (corrupts non-ASCII) | rule 4 error |
| `{payload: S.uint8Array}` in a JSON document — corrupts | base64 |
| `S.encoder(S.uint8Array.with(S.to, S.number))(42)` — returns `42` typed as `Uint8Array` | error (the decoder's missing fall-through, a standalone soundness fix) |
| `S.optional(S.string).with(S.to, S.uint8Array)` — the `undefined` arm passed through as bytes | error, which `CODEC_SPEC.md`'s rule 3 already said: a variant with no decoder rejects the operation |
| `S.base64.with(S.trim).with(S.to, S.uint8Array)` — packed the base64 *text* as bytes | the payload, same as untrimmed: a refinement that only reshapes the text carries the marker |
| a `S.jsonString.with(S.to, X)` field of a decoded document — re-escaped its own text, then failed against X | parsed (rule 3) |
| a `noValidation` field of a JSON document — `Can't decode JSON to Date` | decoded: `noValidation` drops the checks, not the conversion |
| every `Blob`/`File` conversion — creation error | its payload conversions work, and the rest asks. `S.file.with(S.to, S.blob)` widens; the other way round is still an error, because not every blob is a file |

All land together. After this release, changes only turn errors into working
code.

## Spec coverage

One spec per **carrier kind × direction**, plus one per format's own codec —
never per pair. Bytes carriers use non-ASCII fixtures in both directions;
ASCII-only fixtures are what hid the corruption above.

| spec | what it pins |
| --- | --- |
| `base64`, `uint8array` | the carriers themselves, and `base64`'s `contentEncoding` emit |
| `codec-uint8array-base64` | the bytes payload transfer |
| `codec-base64-string` vs `codec-jsonstring-string` | the payload rule's least guessable pair — widen vs parse |
| `jsonstring-object-url`, `codec-array-never-jsonstring` | the two shapes that reach jsonString's fallback for a value it can't serialize piecewise |
| `codec-base64-file` | payload transfer in and out of a binary container |
| `codec-uint8array-jsonstring-ambiguous`, `codec-file-jsonstring-ambiguous`, `codec-base64-jsonstring-ambiguous` | rule 4, one per carrier kind |
| `codec-uint8array-json-unsupported`, `codec-uint8array-optional-jsonstring-unsupported` | where the axis stops — `S.json` and a union, rejected without naming a slot |
| `codec-file-blob` | the one instance widening the axis makes legal, and the direction that stays an error |
| `codec-base64-jsonstring-payload` | rule 3, the JWT segment |
| `codec-jsonstring-object-uint8array`, `codec-jsonstring-object-file` | rule 2, both directions |
| `codec-jsonstring-object-optional-uint8array`, `jsonstring-optional-base64-field` | rule 2 through a union arm — the one that needs a hop to the stored form, and the one that already is it |
| `codec-jsonstring-object-optional-file`, `codec-jsonstring-dict-file` | the two things an async encode broke: an object's optional fields, and a dict with no keys |
| `jsonstring-novalidation-base64` | `noValidation` voids the raw-splice proof for a content format too |
| `codec-jsonstring-object-jsonstring`, `codec-jsonstring-jsonstring-payload` | the nested-document field, and the document that carries one directly |
| `codec-email-string`, `codec-jsonstring-object-optional-jsonstring`, `jsonstring-optional-jsonstring-field`, `jsonstring-novalidation-date` | bugs this work turned up and didn't cause, each carrying a `FIXME` that says so |
| `codec-uint8array-jsonstring-packed`, `codec-jsonstring-file-slots` | rule 1, both spellings of the pair |
| `codec-uint8array-number-unsupported`, `codec-optional-string-uint8array-unsupported` | the decoder fall-through the soundness fix added, standalone and through a union arm |
| `string-to-blob` | the conversion that used to be two creation errors |
| `codec-base64-trim-jsonstring-ambiguous` | the marker surviving a `S.trim` link, so the pair still reports rather than guesses |
| `jsonstring-novalidation-date`, `jsonstring-novalidation-format` | a `noValidation` field decoded back out of its document, and `jsonstring-novalidation` for the JSON target that still travels as text |

`tests/content_test.ts` holds the rest, and only because the spec format can't:
a golden can't hold a `Uint8Array`, `Blob` or `File`, and every compiled
operation must run an example — so a conversion that only ever produces one has
no spec to live in. That covers the UTF-8 hop both ways, `S.file` to and from
bytes and text, rule 3 into a payload the caller reads as bytes, the
`"unpack"`/`"pack"` spelling whose encode side lands on bytes, and every round
trip whose far end is a carrier. CONTRIBUTING.md's Spec Harness
Suggestions is where the fix belongs; when it lands, those rows move back.
