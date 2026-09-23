# Content codec spec

**Implemented.** `CODEC_SPEC.md` governs built-in conversions and custom coders.
This file governs the pairs where **two** built-in readings exist: a carrier (a
value that stores other data) meeting a format that could either rewrite it or
open it.

`B_contentDiffers` and `B_rejectUnsettled` in `src/builder.ts` are the two decisions
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

Both are useful. Today the pair silently does a third, broken thing - UTF-8
escape, which corrupts any byte outside ASCII (`[137, 80, 78, 71]` → `"�PNG"` →
decodes back as different bytes, no error at either end).

## The API

Two values join `CODEC_SPEC.md`'s `Conversion` union:

```ts
type Conversion<A, B> = Coder<A, B> | "auto" | "never" | "pack" | "unpack" | { async: Coder<A, Promise<B>> };
```

Each names what its direction does to its own source:

- `"unpack"` - open the source, hand its payload to the target.
- `"pack"` - store the source's value inside the target.

```ts
// the bytes ARE the JSON text, rather than a value stored in one
S.uint8Array.with(S.to, S.jsonString, { decode: "unpack", encode: "pack" });
// Schema<Uint8Array, string>

// base64 inside a JSON string, into a File
S.jsonString.with(S.to, S.file, { decode: "unpack", encode: "pack" });
```

A slot picks a reading; it does not invent a conversion. What the opened payload
then converts to is still whatever the rules below give, so a target that no
built-in conversion reaches is still rejected - reading a `File` into an object
schema needs the format that turns its text into one
(`S.file.with(S.to, S.jsonString.with(S.to, configSchema))`, rule 3), not a slot.

Mixing with `"never"`, coders and `{async}` follows `CODEC_SPEC.md` - each of
those answers for its own direction. `"auto"` does not: it leaves that direction
asking the question the reading just answered, so a reading has to be paired
with its opposite. `"unpack"` opposite `"unpack"` (or `"pack"` opposite `"pack"`)
is rejected for the mirror reason - opening in both directions leaves no side
holding the payload.

A bare `"pack"` or `"unpack"` as the third argument names the decode reading
and takes the opposite on encode - readings must come as a pair, so one word
is enough. A bare function still means decode-only (custom rule 3).

```ts
S.uint8Array.with(S.to, S.jsonString, "unpack");
// ≡ { decode: "unpack", encode: "pack" }
```

A reading declares its source. `"unpack"` needs a source with something to
open - a payload, or the text of a plain string. From a plain string it is
accepted into every target but `S.json`, since what the target does with the
opened text is the target's own conversion: `S.string.with(S.to, S.number,
"unpack")` coerces. That is how an integration handed a schema it did not write
declares its text once:

```ts
S.string.with(S.to, userSchema, "unpack");
// S.number, S.date, S.jsonString, S.jsonString.with(S.to, X): the text is read
// S.schema({ ... }), S.array(...): refused - no conversion reads text into one
// S.json: refused - it is the document, and has no opened form
```

A target that no conversion reads text into refuses with the slot as it does
without one; a document that holds it says how, `S.jsonString.with(S.to, X)`.
A carrier's reading also stops at a union target, which it meets whole.

`"pack"` needs a target that stores, so `S.string.with(S.to, S.number, "pack")`
is rejected, and so is a reading on a source with nothing to open - a number,
or a string format: `S.email.with(S.to, S.jsonString)` stores, and has no other
reading to pick.

## Rule 1: an explicit slot wins

See above. Everything below is what happens when no slot is written.

## Rule 2: a value position packs

A field or array item is a value in the document - nothing asks it to be a
document of its own:

```ts
S.encodeOrThrow(S.schema({ payload: S.uint8Array }), S.jsonString)({ payload: bytes });
// {"payload":"ZGF0YQ=="}                      base64, not mangled UTF-8
```

A `Blob`/`File` field packs the same way, which makes that **encode async** (a
Blob's bytes are only readable asynchronously) - `S.encodeAsPromiseOrReject`, and the sync
`S.encodeOrThrow` fails at creation like any other async operation.

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

S.base64url.with(S.to, S.jsonString.with(S.to, claimsSchema)); // a JWT segment
```

The check is on the schema shape (a `content` marker plus `.to` present), so it
is local and stable: new built-in conversions in later versions can never
retract it.

Both spellings say it. Nesting the payload inside the format, as above, or
chaining it after:

```ts
S.file.with(S.to, S.jsonString).with(S.to, configSchema);   // the same schema
```

The first link is rule 4 on its own - nothing has said yet what the JSON string
carries - and the second link is what settles it. Nothing is decided until the
schema is compiled, so a link is judged with the whole chain in view.

## Rule 4: otherwise, error

```ts
S.file.with(S.to, S.jsonString);        // which reading?
```

Where no slot would resolve it either - a union, or `S.json` - the pair says it
has no decoder instead of naming a spelling that wouldn't work:

```ts
S.uint8Array.with(S.to, S.json);              // Can't decode Uint8Array to JSON
S.uint8Array.with(S.to, S.optional(S.jsonString));
```

Both fail at **operation creation** (like every conversion error), one message
built from the same pieces the existing unsupported-conversion error uses:

```
Ambiguous File -> JSON string. Should the bytes be packed or unpacked? Choose with S.to and "pack" or "unpack"
```

There are no per-carrier defaults. When both readings are live, the library
asks.

## A plain string is both

`S.string` is the one schema that is both a JSON value and text, so its link
into a JSON text format has the two readings a bytes carrier's has, and rules
1 to 4 apply as written:

```ts
S.string.with(S.to, S.jsonString);                        // rule 4: Ambiguous string -> JSON string
S.string.with(S.to, S.jsonString, "unpack");              // the text, checked as JSON
S.string.with(S.to, S.jsonString, "pack");                // '"hi"' - the string is a value
S.string.with(S.to, S.jsonString.with(S.to, config));     // rule 3: parsed
S.jsonString.with(S.to, S.schema({ meta: S.string }));    // rule 2: the field is a value
```

Everything else already knows which it is. A string format (`S.email`), a
literal, a number and a field of a document are values and store. An entry
source (`S.env`, a form field), a parse from `unknown` and a carrier's opened
text are representations and are read.

## Never ambiguous

These skip the rules entirely:

```ts
S.jsonString.with(S.to, S.jsonString);   // identity (same schema)
S.email.with(S.to, S.string);            // widen - a constraint format has no payload
S.base64.with(S.to, S.string);           // widen - its payload is bytes, not a string
S.base64.with(S.to, S.uint8Array);       // payload transfer - one reading
S.file.with(S.to, S.string);             // payload transfer (text read, async)
S.uint8Array.with(S.to, S.string);       // UTF-8 - pack and unpack produce the SAME code
S.jsonString.with(S.to, userSchema);     // parse - a plain type can't be packed into
S.number.with(S.to, S.jsonString);       // pack - a number has nothing to open
S.email.with(S.to, S.jsonString);        // pack - a format can't spell a document
```

The two-string contrast follows from the payloads, and reads one way only:

```ts
S.jsonString.with(S.to, S.string);  // parses - `.to` on a format names its payload
S.base64.with(S.to, S.string);      // identity - a string is NOT bytes
S.string.with(S.to, S.jsonString);  // asks - a string is a JSON value AND text
```

## Carriers

| carrier | payload | read | write |
| --- | --- | --- | --- |
| `S.blob`, `S.file` | bytes **and** text | async | sync |
| `S.uint8Array` | bytes | sync | sync |
| `S.base64` | bytes | sync | sync |
| `S.base64url` | bytes | sync | sync |
| `S.jsonString` (future: toon, env) | a JSON value | sync | sync |
| future: protobuf | a message | sync | sync |

`S.formData` is not a carrier on this axis: a form has no JSON document form
and no format opens into one, so a link to it has one reading or none, and a
`FormData` in a JSON position has no document, the way a `Blob` has none.

Packing bytes into a JSON position always produces base64. Packing a `File`
loses its name - the reverse builds `new File([content], "")`; a name option
belongs on `S.file` itself, not on a slot.

## Reversal

`pack` and `unpack` trade places under `S.reverse`, like `parser`/`serializer`:
`{decode: "unpack", encode: "pack"}` reversed is `{decode: "pack",
encode: "unpack"}`, and double reversal restores both. Async rides the carrier:
a `File` unpack is an async decode, its reverse an async encode.

## JSON Schema

Derived from the link - nothing declared twice. `toJSONSchema` describes a
schema's *input*, so the annotations land on whichever side of a link is the
string; a `Blob` input has no document, and keeps saying so.

| schema | draft-07 / 2020-12 | openapi-3.0 |
| --- | --- | --- |
| `S.base64` | `contentEncoding: "base64"` | `format: "byte"` |
| `S.base64url` | `contentEncoding: "base64url"` | `format: "base64url"` |
| `S.jsonString.with(S.to, X)` | `contentMediaType: "application/json"`, plus `contentSchema: <X>` in 2020-12 | bare string |
| `S.string.with(S.to, S.blob)` | `contentMediaType: "application/octet-stream"` | `format: "binary"` |
| `S.jsonString.with(S.to, S.file, {decode: "unpack", ...})` | `contentMediaType: "application/json"` | `format: "binary"` |
| `S.base64.with(S.to, S.file)` | both of the above | `format: "byte"` |

A carrier's own emit fills only what the string hasn't already said about
itself, which is why the last two rows keep the encoding they are written in
rather than the medium they end in. The base64 rows round-trip through
`fromJSONSchemaOrThrow`, from either spelling; a `contentMediaType` does not yet, and
comes back as a plain string. The
`contentSchema` emit is gated on a json-format source, so a base64 segment
carrying a document annotates the encoding it is stored in and stops there.

**The axis stops at a union, and at `S.json`.** A union carries neither `content`
nor `.to` of its own, and neither an arm's payload declaration nor a reading
written on the union reaches the dispatch; `S.json` is the document rather than
a rendering of one, so neither reading of a link to it is built. Both are read
far enough to know the pair is not a plain transfer, and then rejected as having
no decoder - which a custom coder still answers. Rule 2 is the exception on both
counts, and only because `jsonEncoderFn` rewrites the arms itself and reads
`content` for the shape a document stores its target as.

**Not yet:** a packed bytes field has no document form - `S.toJSONSchema` of
`S.schema({payload: S.uint8Array})` reports `Expected JSON, received Uint8Array`
rather than describing the base64 string the field becomes. That follows the
existing rule that a carrier has no document of its own (`S.blob` answers the
same way); making `content` answer it instead is a separate change.

## Implementation notes

**Resolution is syntactic, and it happens where the link is written.** Bits
of a schema's `flags` carry it (the table is on the field, in `base.ts`):

| bits | meaning |
| --- | --- |
| 1, 2 | the kind of payload this value carries - bytes (every bytes carrier, and `S.base64`, which is how bytes sit in a document) or a JSON value (`S.jsonString` and `S.json` itself). Neither means the value carries no payload. A bytes carrier also names the schema it is stored as, `storedAs`, and a text format its codec, `bytesCodec`. |
| 4, 8 | the reading of the link into this schema, whatever settled it: a slot, a payload gaining its `.to` (rule 3), or the document piece a field is stored into (rule 2). Opens its source, or stores it. One per link; `reverse`, which copies node by node, writes each node's from its forward successor's, swapped. |

Two schemas that agree on the kind carry the same payload, so a link between
them is a plain transfer; two that disagree have both readings live, and the
reading bits say which applies. Every way a reading gets settled writes them: a
slot (rule 1), a document field's position (rule 2, in `fieldPiece`), and a
payload gaining a `.to` (rule 3) - the last written at the two places a `.to`
link is made, `codecTo` and `compileChain`, the moment it becomes true.
Materialized rather than read off `.to !== U`, because reversing a chain
re-points `.to` and would lose it, while `reverse` carries the reading across
like any other slot: the legal `X -> jsonString -> File` and the rejected
`jsonString -> File` reach the decoder as the same pair, and only the slot tells
them apart.

Rule 4 is then asked while compiling, by the schemas that declare a payload -
`json`, `jsonString`, `base64`, `uint8Array`, `file` - each in its own decoder
and encoder, and by a union carrying its `.to` into one, before it splits the
link per arm (`B_rejectUnsettled`). A link between two payloads of different
kinds with no reading on either end is the error; a bundle that ships none of
those schemas ships none of this, and `S.to` is a link and nothing more. The
link is found from the parse loop's own step rather than from the val's source
type: a union case parses its arm from the type narrow and a bytes read from
text types its result as the format singleton, so the source there is a schema
with no `.to` at all.

What the rejection *says* comes from the pair. Two renderings name the slots,
since a slot is what settles them; a pair no slot resolves - a union arm's
payload, a reading written on the union, either side being `S.json`, which has
no opened form of its own - reports the pair as having no decoder, which a
coder still answers. The operation form (`S.decodeOrThrow(a, b, c)`) gets the
same message as the written `S.to`: it is the same chain.

Everything else is the two markers being read where a decision already happened:
rule 2 is the carrier's `encoder` being handed a content-format target,
`jsonEncoderFn`'s fallback uses `content` for the shape a document stores the
target as instead of assuming `string`, and the nested-jsonString fix is
`B_narrowJsonSourcedJsonString` no longer stopping at a field whose jsonString
declares a payload. No new `Val` fields, no compile-loop cost, nothing in
generated code a hand-written converter wouldn't contain.

The one price is the universal path: `getOp` is in every bundle, so
`B_contentDiffers` is too, and `reverse` carries a line for the reading. About 180 gzipped bytes on every export (`bundleSize.yaml`,
where the smallest go 4136 → 4315) - the question and one closure; the messages
ride with the callers, and so does the union walk `B_contentNode` does, which is
why only `codecTo` pays for it. That buys a creation-time gate on conversions
that otherwise corrupt data silently.

The string rule adds about 70 gzipped bytes to the same path (4463 → 4530 on
the smallest exports): `reverse` reads a union's reading off its payload arm
and records that a payload read into text is stored when read back, and
`compileChain` skips rule 3 for a `.to` of `undefined`. The decision itself
rides only with `S.jsonString`, and `S.env`'s record form got smaller: the
unset var is the item's own input, so the field loop no longer guards it.

**Conversions live on the carrier, not the format** - the existing `S.date`
pattern. `S.uint8Array`'s encoder owns base64; `S.file`'s owns `.text()` /
`.arrayBuffer()`; `S.base64` owns opening itself into text. So `S.jsonString`
never references a base64 helper, and a bundle that never mentions bytes never
ships one. The format side checks only the generic `content` marker - it never
names toon, env, or any other format, which is what keeps each future carrier a
self-contained file.

`S.fromJSONSchemaOrThrow` pays for the round trip: reading `contentEncoding` back means
naming `S.base64` and `S.base64url`, which bring their patterns and conversions.
That is the largest row in `bundleSize.yaml`. Every format costs it something;
these are the first that carry a codec.

The keyword is an annotation in both dialects, so a validator that reads it as
one accepts `"hello world"` under `contentEncoding: "base64"`. Sury reads it as
a schema, because `format` - an annotation on the same terms - has always come
back as the validating `S.uuid`, `S.email` and the rest. A round trip through
`fromJSONSchemaOrThrow` produces the schema the keyword names, not a validator for the
document it came from.

**The base64 helpers** feature-detect native `Uint8Array` methods, then Node
`Buffer`, then `btoa`/`atob`, inside a `@__PURE__` factory per alphabet so a
`globalThis.Buffer` read cannot leak into unrelated exports. Generated code is
a single `e[N](i)` call either way. The atob fallback bridges through an
intermediate binary string in 8KB chunks. The whole array blows
`String.fromCharCode`'s argument limit, and a byte at a time is several times
slower than either. `scenarios.yaml`'s `base64-pack` / `base64-unpack` are what
that costs a consumer. Carriers pack through a content node that has the format
refine and `bc` only, so a File bundle does not ship recode or TextEncoder.

## Breaking changes

| today | after |
| --- | --- |
| `S.uint8Array.with(S.to, S.jsonString)` - UTF-8 escape (corrupts non-ASCII) | rule 4 error |
| `S.string.with(S.to, S.jsonString)` - stringified the string as a JSON value | rule 4 error; `"pack"` is that reading, `"unpack"` the text |
| `S.string.with(S.to, S.jsonString.with(S.to, X))` - stringified, then failed against X | parsed (rule 3) |
| `S.record(S.env)` into a `S.nullable` field with the var unset - `received undefined` | `null`, as the single var already read |
| `{payload: S.uint8Array}` in a JSON document - corrupts | base64 |
| `S.encodeOrThrow(S.uint8Array.with(S.to, S.number))(42)` - returns `42` typed as `Uint8Array` | error (the decoder's missing fall-through, a standalone soundness fix) |
| `S.optional(S.string).with(S.to, S.uint8Array)` - the `undefined` arm passed through as bytes | the text packs, `undefined` is rejected: `CODEC_SPEC.md`'s nullish-arm exception to rule 3 |
| `S.base64.with(S.trim).with(S.to, S.uint8Array)` - packed the base64 *text* as bytes | the payload, same as untrimmed: a refinement that only reshapes the text carries the marker |
| a `S.jsonString.with(S.to, X)` field of a decoded document - re-escaped its own text, then failed against X | parsed (rule 3) |
| a `noValidation` field of a JSON document - `Can't decode JSON to Date` | decoded: `noValidation` drops the checks, not the conversion. Only `S.json` itself still travels as text, since its parse *is* its check |
| every `Blob`/`File` conversion - creation error | its payload conversions work, and the rest asks. `S.file.with(S.to, S.blob)` widens; the other way round is still an error, because not every blob is a file |
| - | a union target of a `Blob`/`File` stays an error, where a `S.uint8Array` one works: a union picks its variant before the read resolves, so the arm's checks would run against the promise |

All land together. After this release, changes only turn errors into working
code.

## Spec coverage

One spec per **carrier kind × direction**, plus one per format's own codec -
never per pair. Bytes carriers use non-ASCII fixtures in both directions;
ASCII-only fixtures are what hid the corruption above.

| spec | what it pins |
| --- | --- |
| `base64`, `base64url`, `uint8array` | the carriers themselves, and each alphabet's JSON Schema emit |
| `codec-uint8array-base64`, `codec-uint8array-base64url`, `codec-base64-uint8array` | the bytes payload transfer, both alphabets and both ways round |
| `codec-base64-base64url` | recode between alphabets |
| `codec-base64-trim-base64url` | recode after `S.trim`, which copies `content` but not `bc` |
| `codec-uint8array-jsonstring-pack` | `S.to(from, to, "pack")` as the opposite pair |
| `codec-base64-string` vs `codec-jsonstring-string` | the payload rule's least guessable pair - widen vs parse |
| `jsonstring-object-url`, `codec-array-never-jsonstring` | the two shapes that reach jsonString's fallback for a value it can't serialize piecewise |
| `codec-base64-file` | payload transfer in and out of a binary container |
| `codec-uint8array-jsonstring-ambiguous`, `codec-file-jsonstring-ambiguous`, `codec-base64-jsonstring-ambiguous` | rule 4, one per carrier kind |
| `codec-uint8array-json-unsupported`, `codec-uint8array-optional-jsonstring-unsupported` | where the axis stops - `S.json`, and a union carrying a payload |
| `codec-file-optional-email` | a nullish arm is no choice for an asynchronous read to make: `S.file` reads into `S.optional(S.email)` as into `S.email`, and `None` is rejected in both directions |
| `codec-file-blob` | the one instance widening the axis makes legal, and the direction that stays an error |
| `codec-base64-jsonstring-payload`, `codec-base64url-jsonstring-payload` | rule 3, both alphabets - the base64url one is the JWT segment |
| `codec-jsonstring-object-uint8array`, `codec-jsonstring-object-file` | rule 2, both directions |
| `codec-jsonstring-object-optional-uint8array`, `jsonstring-optional-base64-field` | rule 2 through a union arm - the one that needs a hop to the stored form, and the one that already is it |
| `codec-jsonstring-object-optional-file`, `codec-jsonstring-dict-file` | the two things an async encode broke: an object's optional fields, and a dict with no keys |
| `jsonstring-novalidation-base64` | `noValidation` voids the raw-splice proof for a content format too |
| `codec-jsonstring-object-jsonstring`, `codec-jsonstring-jsonstring-payload` | the nested-document field, and the document that carries one directly |
| `codec-email-string`, `codec-jsonstring-object-optional-jsonstring`, `jsonstring-optional-jsonstring-field`, `jsonstring-novalidation-date` | bugs this work turned up and didn't cause, each carrying a `FIXME` that says so |
| `codec-uint8array-jsonstring-packed`, `codec-jsonstring-file-slots` | rule 1, both spellings of the pair |
| `codec-uint8array-number-unsupported` | the decoder fall-through the soundness fix added |
| `codec-optional-string-uint8array`, `codec-uint8array-optional-string`, `codec-uint8array-optional-base64` | a nullish arm beside a payload: the text converts, the arm is dropped in both directions |
| `codec-array-never-uint8array` | the one source the fall-through has to keep letting through: nothing reaches a `never`, so there is no conversion to reject |
| `string-to-blob` | the conversion that used to be two creation errors |
| `codec-base64-trim-jsonstring-ambiguous` | the marker surviving a `S.trim` link, so the pair still reports rather than guesses |
| `jsonstring-novalidation-date`, `jsonstring-novalidation-format` | a `noValidation` field decoded back out of its document, and `jsonstring-novalidation` for the JSON target that still travels as text |
| `codec-uint8array-string`, `codec-uint8array-string-novalidation`, `codec-string-novalidation-uint8array`, `codec-email-uint8array` | the UTF-8 hop both ways - lossy for bytes that aren't text, and checked by the string target the bytes spell, unless `noValidation` says not to |
| `codec-uint8array-jsonstring-payload` | rule 3 into a payload the caller reads as bytes |
| `codec-uint8array-jsonstring-unpack` | the `"unpack"`/`"pack"` spelling whose encode side lands on bytes |
| `codec-string-jsonstring-ambiguous`, `codec-string-jsonstring-unpack`, `codec-string-jsonstring-pack` | a plain string into a JSON text format: rule 4, and each reading |
| `codec-string-jsonstring-payload`, `codec-string-jsonstring-payload-chained`, `codec-string-optional-jsonstring-payload` | rule 3 from a plain string, both spellings, and through a nullish arm |
| `codec-string-optional-jsonstring-unpack`, `codec-optional-string-jsonstring-unpack` | a reading crossing a union on either side |
| `codec-string-number-unpack` | `"unpack"` declaring the source into a target with one reading |
| `codec-email-jsonstring` | a string format is a value |
| `codec-jsonstring-object-string-unpack` | a field carrying its own reading inside a document |
| `codec-env-optional-jsonstring-payload`, `codec-env-record-nullable` | an entry's text is a representation, and its unset var is the absent arm in a record too |
| `codec-json-object-uint8array` | rule 2 in a JSON value, where the reading is the same as in JSON text |
| `codec-jsonstring-object-text-uint8array`, `codec-jsonstring-object-optional-text-uint8array` | rule 2 through a field storing the same payload as the source - text stays text, with or without the union arm `S.optional` adds |
| `codec-uint8array-optional-base64`, `codec-uint8array-optional-string` | a payload arm keeping its marker through the union narrow, and an arm with no payload still taking the text |
| `codec-base64-trim-uint8array` | the marker surviving a `S.trim` link into bytes |
| `string-to-file`, `file-to-string`, `file-to-uint8array`, `file-to-jsonstring-object` | a `File` to and from text, bytes, and a JSON document it carries |
| `union-file-read-fail`, `object-file-read-fail`, `object-optional-file-read-fail` | a `File` whose read rejects: a union does not treat that as a miss, a required field takes the path, an optional field stays the raw exception |

| `codec-uint8array-optional-jsonstring-payload-unsupported`, `codec-optional-string-optional-jsonstring-payload-ambiguous` | a union target stops a carrier's reading and a union source's alike: neither direction compiles, where `reverse` once lifted the arm's reading and compiled an encode alone |
| `codec-env-jsonstring-or-number` | an entry's text reaches a union's JSON arm as a representation, where it used to be escaped as a value |
| `codec-env-uint8array-one-way`, `codec-literal-object-one-way` | one-way links `fuzz:content` found that predate this axis, each with a `FIXME` |

`pnpm --filter=sury fuzz:content` crosses every source kind with every target
shape and slot, and holds the properties above for all of them at once: both
directions or neither, the reverse reading the same values, a round-trip, and a
slot doing what it declares. The rows above are what it has turned up so far.

`tests/content_test.ts` holds the rest, and only because the spec format can't:
a golden can't hold a `Blob` or `File`, and every compiled operation must run
an example - so a conversion that only ever produces one has no spec to live
in. That covers `S.file` to and from bytes and text, rule 2 with a `File` in
the value position, rule 3 into a payload read out of a file, and the
`"unpack"`/`"pack"` spelling whose decode side lands in one. It also holds
every reading the API rejects outright - a pair that agrees on the payload, a
side that carries none, `S.json`, and two readings naming the same direction -
since those panic while the schema is being built and `ts.schema` never
evaluates. CONTRIBUTING.md's Spec Harness Suggestions is where the fix belongs;
when it lands, those rows move back.
