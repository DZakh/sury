import {
  anyOfTag,
  arrayTag,
  baseSchema,
  bigintTag,
  booleanTag,
  type Builder,
  type Check,
  compilePath,
  conversionSite,
  copySchema,
  type Encoder,
  type ErrorDetails,
  type SuryErrorRecord,
  initSchema,
  inlinedProperty,
  inputExpression,
  instanceTag,
  type Internal,
  noopDecoder,
  numberTag,
  objectTag,
  panic,
  type Path,
  pathConcat,
  refTag,
  s as sury,
  setHas,
  stringify,
  stringTag,
  tagFlags,
  U,
  undefinedTag,
  unknown,
  unknownTag,
  type Val,
} from "../base";
import {
  _notVar,
  _notVarBeforeValidation,
  B_let,
  _var,
  B_embed,
  B_embedPure,
  B_fail,
  B_conversionFail,
  B_hoistDecl,
  B_invalidOperation,
  B_markAsync,
  B_markOutput,
  B_merge,
  B_mergeWithPathPrepend,
  B_pathSnap,
  B_next,
  B_operationArg,
  B_prefixPath,
  B_refine,
  B_scope,
  B_slot,
  B_unsupportedDecode,
  B_varWithoutAllocation,
  failInvalidType,
  rethrowWithPath,
} from "../builder";
import { arrayFactory, B_record, dictFactory, objectDecoder, valAt, valGet } from "../composites";
import { schemaShape } from "../factory";
import { getOp, getOutputSchema, instanceDecoder, optionalMembers, parse } from "../parse";
import { bigint, bool, float, int, integer, string, unit } from "../primitives";
import type { FieldType, ProtobufType, StoredField, WellKnownType } from "./protobufField";
import { fieldMetadata, isContainer, protobufField, wellKnownTakes, wrappers } from "./protobufField";
import { uint8Array } from "./uint8Array";

type Field = {
  number: number;
  type: ProtobufType;
  packed: boolean;
  key: string;
  repeated: boolean;
  optional: boolean;
  wire: number;
  message?: Message;
  // Key type of a map field; unset for anything else.
  map?: ProtobufType;
  oneof?: string;
  // A member of a union oneof or of a typed Any, sharing the property `key`
  // with the others: the name it prints as, and the value it declares.
  name?: string;
  armValue?: Internal;
  // A 64-bit field held as its decimal string, whose zero is "0".
  text?: boolean;
  fromWire: Internal;
  held: Internal;
  // The key path to the value. A oneof member's starts with the oneof's key,
  // an Any member's is the arm's own, and the Any's own two fields have none: the
  // value holds no `typeUrl`, so a failure there is the Any field's.
  path: string[];
  omit?: boolean;
  fromWireIsHeld?: boolean;
};

// A union oneof's members in the order the union lists them, each with the
// field it holds, none for the member that stands for no field set.
type Members = { union: Internal; members: [member: Internal, field: Field | undefined][] };

type Message = {
  fields: Field[];
  // The object the message was compiled from, which every reader of a message
  // needs beside it: the printer names its fields from it, and a chain past it
  // is what a nested field can't run.
  object: Internal;
  // What a well-known message reads into before the value converts: the
  // scalar a wrapper unwraps, the value a codec reads.
  raw: Internal;
  // The refs the walk to the object passed that carry a chain or a
  // refinement of their own, innermost first: they run on the object's value,
  // so a message reached through one is another message.
  links?: Internal[];
  // The name `S.recursive` gave the message, which is what the printer calls it
  // and what an error about it names.
  name?: string;
  // Set once a field under it refers back to it.
  recursive?: true;
  // The Google type it is, printed as an import rather than declared.
  wellKnown?: WellKnownType;
  typeName?: string;
  // A typed Any: the Any message on the wire, whose type URL tells which
  // member its value is. Its `fields` are the Any's own two, then one per
  // member, each holding the member's message, for the frames to name; those
  // are `any.fields`, the member each is for `members` in the same order. The
  // catch-all, `other`, is named google.protobuf.Any, comes last and reads the
  // Any message whole; the rest are `listed`.
  any?: { members: Internal[]; fields: Field[]; listed: Field[]; other?: Field; told: Told | undefined };
  // Whether a hand-written codec (`codecs`) stands in for the functions a
  // message's fields would compile to, for a value that is not the message's
  // shape: a `Date` for a Timestamp, JSON for a Value, Struct or ListValue.
  codec?: true;
  // A message of one field whose value is the field's own: a wrapper's
  // scalar, a FieldMask's paths.
  unwrap?: boolean;
  oneofs?: Record<string, Members>;
  async?: boolean;
};

// One compile of a message tree.
type Ctx = {
  // Per object, not the last name a walk passed: `toProtoOrThrow` resolves the
  // wire object in one walk and compiles it in another.
  names: Map<Internal, string | undefined>;
  // By object, or by well-known type and object: one `{ seconds, nanos }`
  // can be both a Timestamp and a Duration.
  messages: Map<unknown, Message>;
  // The messages currently building: a field whose message is on it closes a
  // cycle.
  stack: Message[];
  // A reader's messages, whose values run the chains and refinements of the
  // refs a walk passes (`Message.links`).
  reads?: boolean;
  writes?: boolean;
};

const newCtx = (reads?: boolean, writes?: boolean): Ctx => ({
  names: new Map(),
  messages: new Map(),
  stack: [],
  reads,
  writes,
});

const deref = (ref: Internal, ctx: Ctx): Internal | undefined => {
  const def = ref.definition!();
  if (def !== U) ctx.names.set(def, ref.name);
  return def;
};

// The object a schema's chain starts with: what the wire speaks, the rest of
// the chain running on the value after. A `$ref` stands for its definition,
// `S.recursive` being the only way a message contains itself.
const firstObject = (schema: Internal, ctx: Ctx): Internal | undefined => {
  let current: Internal | undefined = schema;
  // Only the refs are counted, and bounded the way the parse loop is: a `.to`
  // chain is finite by construction, where a definition that is itself a `$ref`
  // back to the one naming it - `S.recursive("a", (a) => a)` - never reaches an
  // object at all.
  for (let refs = 0; current !== U && current.type !== objectTag; ) {
    if (current.type !== refTag) current = current.to;
    else if (++refs > 50) return panic("S.protobuf: a $ref resolves to itself, so it names no message");
    else current = deref(current, ctx);
  }
  return current;
};

const lastObject = (schema: Internal, ctx: Ctx): Internal | undefined => {
  let object = firstObject(schema, ctx);
  while (object?.to !== U) {
    const next = firstObject(object.to, ctx);
    if (next === U) return panic(`S.protobuf: ${inputExpression(object)} converts with S.to into something other than a message`);
    object = next;
  }
  return object;
};

// `whole` walks on past the object: reversed, a chain's message is its last
// link. Bounded as `firstObject` is: a definition can be a ref back to itself.
const typeNameOf = (schema: Internal, ctx: Ctx, whole?: boolean): string | undefined => {
  for (let current: Internal | undefined = schema, steps = 0; current !== U && ++steps <= 50; current = current.type === refTag ? deref(current, ctx) : current.to) {
    if (current.protobufTypeName !== U) return current.protobufTypeName;
    if (!whole && current.type === objectTag) return U;
  }
  return U;
};

// The object the wire speaks for a schema `toProtoOrThrow` is given: beside
// `S.protobuf` when the chain reaches it (`A.with(S.to, B).with(S.to,
// S.protobuf)` converts to B before the wire), else the chain's first.
const wireObject = (schema: Internal, ctx: Ctx): Internal | undefined => {
  let last: Internal | undefined = U;
  for (let current: Internal | undefined = schema; current !== U; current = current.to) {
    if (current.flags & 256) return last || firstObject(current.to!, ctx);
    // A ref counts as the object it names, a recursive message reaching the
    // wire as one - and as nothing when it names no object, which is how a
    // `S.json` in the chain clears the candidate and the printer comes to
    // refuse what the wire refuses. A link that is no ref at all is stepped
    // over: `S.unknown` between a message and the wire changes neither.
    last = current.type === refTag ? firstObject(current, ctx) : current.type === objectTag ? current : last;
  }
  return firstObject(schema, ctx);
};

const packable: Record<ProtobufType, boolean> = {
  double: true,
  float: true,
  int32: true,
  int64: true,
  uint32: true,
  uint64: true,
  sint32: true,
  sint64: true,
  fixed32: true,
  fixed64: true,
  sfixed32: true,
  sfixed64: true,
  bool: true,
  string: false,
  bytes: false,
  enum: true,
  message: false,
};

const wireType = (type: ProtobufType): number => {
  if (type === "double" || type === "fixed64" || type === "sfixed64") return 1;
  if (type === "string" || type === "bytes" || type === "message") return 2;
  if (type === "float" || type === "fixed32" || type === "sfixed32") return 5;
  return 0;
};

// Splits `T | undefined` into T, the presence flag, and the arm that
// supplies a default on absence, if any. Several members left over (an
// optional enum) become a union of their own.
const unwrapOptional = (schema: Internal): [Internal, boolean, Internal | undefined] => {
  const output = getOutputSchema(schema);
  if (output.type !== anyOfTag || output.anyOf === U) return [schema, false, U];
  const [values, hasUndefined] = optionalMembers(output);
  if (!hasUndefined || values.length === 0) return [output, false, U];
  const absent = output.anyOf.find((member) => member.type === undefinedTag && member.to !== U);
  return [values.length === 1 ? values[0]! : anyOf(values), true, absent];
};

// A union as a type descriptor only: the raw and normalized sides share it,
// so nothing ever converts through it, and the union compiler stays out of
// the bundle.
const anyOf = (members: Internal[]): Internal => {
  const mut = baseSchema(anyOfTag, false, noopDecoder);
  mut.anyOf = members;
  mut.has = {};
  for (let idx = 0; idx < members.length; idx++) setHas(mut.has, members[idx]!);
  return mut;
};

const bytesSchema: Internal = /* @__PURE__ */ initSchema(instanceTag, instanceDecoder, (s) => {
  s.class = Uint8Array;
});

const scalarSchema = (type: ProtobufType): Internal => {
  if (type === "string") return string;
  if (type === "bytes") return bytesSchema;
  if (type === "bool") return bool;
  if (
    type === "int64" ||
    type === "uint64" ||
    type === "sint64" ||
    type === "fixed64" ||
    type === "sfixed64"
  ) return bigint;
  if (type === "float" || type === "double") return float;
  if (type === "int32" || type === "sint32" || type === "sfixed32" || type === "enum") return int;
  return integer;
};

// Parses the present variant of `input` into `target`: the code, the
// expression it leaves the value in, and whether it transformed the value.
// Nothing is materialized, so a parse with nothing to do costs nothing.
const presentParse = (input: Val, source: Internal, target: Internal): [string, string, boolean] => {
  const presentIn = B_scope(input);
  presentIn.io = false;
  presentIn.s = source;
  presentIn.e = target;
  presentIn.u = true;
  const presentOut = parse(presentIn);
  return [B_merge(presentOut), presentOut.i, !!presentOut.t];
};

const optionalMessageEncoder: Encoder = (input, target) => {
  const source = input.s.anyOf![0]!;
  // Probed first, so a present side with nothing to do materializes no var;
  // one with work is generated again against the var it then assigns.
  const [probe, probeResult, transformed] = presentParse(input, source, target);
  if (probe === "" && probeResult === input.i && !transformed) {
    const output = B_next(input, input.i, getOutputSchema(target), target);
    output.t = U;
    return output;
  }
  const v = input.v();
  const [code, result] = presentParse(input, source, target);
  const body = code + (result === v ? "" : `${v}=${result};`);
  const output = B_next(input, v, getOutputSchema(target), target);
  output.v = _var;
  output.io = true;
  output.cp = body === "" ? "" : `if(${v}!==void 0){${body}}`;
  // Reads the nested parse materialized without converting anything are
  // not a transform; reporting one would make the parent rebuild itself.
  if (!transformed) output.t = U;
  return output;
};

// A present nested object converts field-wise into the (non-optional)
// normalized target and an absent one stays absent. A real `T | undefined`
// union on either side would make the pipeline re-validate the whole nested
// object, or refuse the object-to-union conversion outright.
const optionalMessage = (raw: Internal): Internal => {
  const mut = baseSchema(anyOfTag, false, noopDecoder);
  mut.anyOf = [raw, unit];
  mut.has = { [undefinedTag]: true };
  setHas(mut.has, raw);
  mut.encoder = optionalMessageEncoder;
  mut.flags = mut.flags | 64;
  return mut;
};

// `S.optional(T, default)`: the raw side is `T | undefined` whose encoder
// parses a present value into `present` and an absent one through the
// union's default arm, so the decoded value is never re-validated and an
// enum stays open.
const defaultedOptional = (raw: Internal, present: Internal | undefined, absent: Internal): Internal => {
  const mut = optionalMessage(raw);
  mut.encoder = (input, target) => {
    const v = input.v();
    const [code, result] = present === U ? ["", v] : presentParse(input, raw, present);
    const body = code + (result === v ? "" : `${v}=${result};`);
    const absentIn = B_scope(input);
    absentIn.io = false;
    absentIn.s = unit;
    absentIn.e = absent;
    absentIn.u = true;
    const absentOut = parse(absentIn);
    const supply = `${B_merge(absentOut)}${v}=${absentOut.i};`;
    const output = B_next(input, v, getOutputSchema(target), target);
    output.v = _var;
    output.io = true;
    output.cp = body === "" ? `if(${v}===void 0){${supply}}` : `if(${v}!==void 0){${body}}else{${supply}}`;
    return output;
  };
  return mut;
};

// Both refinements, in the order they were written: one schema holds one
// refiner, and a refiner answers with the checks to emit, so running two is
// asking each in turn. `internalRefine` spells the same join inline, which is
// cheaper for every refinement export than sharing one helper with this.
type Refiner = (input: Val) => Check[];
const bothRefine = (first: Refiner | undefined, then: Refiner | undefined): Refiner | undefined =>
  first === then || then === U ? first
  : first === U ? then
  : (input) => [...first(input), ...then(input)];

// A codec's value with the refinements the field's schema carries, beside the
// ones the codec's own schema does. Both spellings, since `S.reverse` swaps
// them.
const refinedAs = (from: Internal, standin: Internal): Internal => {
  const refiner = bothRefine(standin.refiner, from.refiner);
  const inputRefiner = bothRefine(standin.inputRefiner, from.inputRefiner);
  if (refiner === standin.refiner && inputRefiner === standin.inputRefiner) return standin;
  const mut = copySchema(standin);
  if (refiner !== U) mut.refiner = refiner;
  if (inputRefiner !== U) mut.inputRefiner = inputRefiner;
  return mut;
};

// A message absent from the wire decodes to its default instance, so a required
// singular message field builds one unasked - and a cycle of such fields builds
// one forever, which is also a type no finite value has. Walked after the tree
// is built rather than while: a message compiles once however many fields reach
// it, so the edge that closes the cycle need not be the one that compiled it.
const rejectEndless = (ctx: Ctx): void => {
  const path = new Set<Message>();
  const clean = new Set<Message>();
  const walk = (message: Message): void => {
    if (clean.has(message)) return;
    path.add(message);
    for (const field of message.fields) {
      const nested = field.message;
      // The edges are the fields that fill: a repeated, optional or map field
      // leaves an absent message absent, and ends the chain there.
      if (nested === U || field.repeated || field.optional || field.map !== U) continue;
      if (path.has(nested)) {
        panic(`S.protobuf: field "${field.key}" makes "${nested.name}" hold itself with no way to end. Make it optional or repeated`);
      }
      walk(nested);
    }
    path.delete(message);
    clean.add(message);
  };
  // From every message a cycle closed on, the root or not: the first message of
  // a cycle the build entered is still building when the cycle comes back to it,
  // so every cycle has one, and one under an optional field is reached too.
  ctx.messages.forEach((message) => message.recursive && walk(message));
};

type Arm = { name: string; key: string | undefined; value: Internal; member: Internal; metadata: StoredField };

type Oneof = { arms: Arm[]; unset: Internal | undefined; optional: boolean; all: Internal[] };

// `undefined` and nothing else: the `value?: undefined` of protobuf-es's arm for
// no member set.
const holdsNothing = (schema: Internal): boolean => {
  const output = getOutputSchema(schema);
  return output.type === undefinedTag || (output.anyOf !== U && output.anyOf.every(holdsNothing));
};

const entriesOf = (member: Internal): Record<string, Internal> | undefined =>
  typeof member.additionalItems === objectTag ? U
  : member.type === objectTag ? member.properties
  : member.type === arrayTag && member.items !== U ? ({ ...member.items } as {})
  : U;

// Per arm, the property that tells it from the members of its own type, and
// that property's literal.
type Told = { tags: (string | undefined)[]; literals: unknown[] };

// `keys` are the arms', which come first in `members`.
const tagAmong = (members: Internal[], keys: (string | undefined)[]): string | undefined => {
  const first = keys[0] === U ? U : entriesOf(members[0]!);
  return first && Object.keys(first).find((property) => {
    const literals: unknown[] = [];
    for (let idx = 0; idx < members.length; idx++) {
      const arm = idx < keys.length;
      const entries = arm && keys[idx] === U ? U : entriesOf(members[idx]!);
      if (entries === U) {
        if (arm) return false;
        // A bare literal for no member set meets `v&&v[tag]` too, so what that
        // reads off it can't equal an arm's literal either.
        const bare = literalOf(members[idx]) as Record<string, unknown> | undefined;
        literals.push(bare && bare[property]);
        continue;
      }
      const literal = property === keys[idx] ? U : literalOf(entries[property]);
      if (entries[property] === U || (arm && literal === U)) return false;
      literals.push(literal);
    }
    return new Set(literals).size === literals.length;
  });
};

// The runtime type a bare arm's value has.
const typeOf = (schema: Internal, message: boolean): string | undefined => {
  const output = getOutputSchema(schema);
  const type = output.type;
  if (type === anyOfTag) {
    const types = new Set(output.anyOf!.filter((member) => member.type !== undefinedTag).map((member) => typeOf(member, message)));
    return types.size === 1 ? [...types][0] : U;
  }
  return type === stringTag || type === numberTag || type === bigintTag || type === booleanTag || type === arrayTag || type === objectTag ? type
    : type === refTag ? (message ? objectTag : U)
    : type === instanceTag && (output.class === Uint8Array || output.class === Date) ? (output.class as Function).name
    : U;
};

// An encode writes the member a value is, so one it can't tell from another
// would come back from the wire as that other: members are told apart by a
// literal each holds a different one of, else by runtime type and, among the
// members of one type, a literal. `keys` are the arms', which come first in
// `members`; a bare literal after them, for no member set, can't be of a bare
// arm's type. `messages` says which bare arm holds a message.
const tellApart = (members: Internal[], keys: (string | undefined)[], messages?: boolean[]): Told | undefined => {
  const told = (tags: (string | undefined)[]): Told => ({ tags, literals: tags.map((tag, idx) => (tag === U ? U : literalOf(entriesOf(members[idx]!)![tag]))) });
  const tag = tagAmong(members, keys);
  if (tag !== U) return told(keys.map(() => tag));
  const types = members.map((member, idx) => (idx < keys.length && keys[idx] === U ? typeOf(member, !!messages?.[idx]) : entriesOf(member) && member.type));
  const last = members[keys.length];
  const bare = last !== U && entriesOf(last) === U ? literalOf(last) : U;
  const tags: (string | undefined)[] = [];
  for (let idx = 0; idx < keys.length; idx++) {
    const type = types[idx];
    const same = (_: unknown, other: number) => types[other] === type;
    if (type === U || (keys[idx] === U && bare !== U && typeof bare === type)) return U;
    const group = members.filter(same);
    if (group.length > 1 && (tags[idx] = tagAmong(group, keys.filter(same))) === U) return U;
  }
  return told(tags);
};

// A oneof is a union of arms, each holding one numbered value: an object or a
// tuple holding it beside literals, or the value itself (protobuf-es's
// `{ case, value }` is an object). The arm for no member set holds only
// literals, is a bare literal, or is `undefined`.
const oneofOf = (key: string, property: Internal): Oneof | undefined => {
  const output = getOutputSchema(property);
  if (output.type !== anyOfTag || output.anyOf === U) return U;
  const arms: Arm[] = [];
  let unset: Internal | undefined;
  let optional = false;
  let stray = false;
  let twice = false;
  let numbered = false;
  const all: Internal[] = [];
  let defaulted = false;
  for (let idx = 0; idx < output.anyOf.length; idx++) {
    const declared = output.anyOf[idx]!;
    const member = getOutputSchema(declared);
    // A default's `undefined` arm converts into the item it defaults to, which
    // read as an arm would look like one holding only literals. One converting
    // to a literal is an arm for no member set like any other.
    if (declared.type === undefinedTag && declared.to !== U && literalOf(member) === U) {
      defaulted = true;
      continue;
    }
    all.push(member);
    if (member.type === undefinedTag) {
      optional = true;
      continue;
    }
    // A numbered member is the value itself, even a literal: a one-value enum
    // (`google.protobuf.NullValue`).
    const own = fieldMetadata(declared);
    if (own !== U) {
      arms.push({ name: `${key}_${own.number}`, key: U, value: declared, member, metadata: own });
      continue;
    }
    const entries = entriesOf(member);
    const values = entries && Object.keys(entries).filter((entry) => fieldMetadata(entries[entry]!) !== U || (literalOf(entries[entry]) === U && !holdsNothing(entries[entry]!)));
    // A bare literal is the arm for no member set too: a ReScript constructor
    // with no payload is its tag's string.
    if (values === U ? literalOf(member) !== U : values.length === 0) {
      if (unset !== U) twice = true;
      unset = member;
      continue;
    }
    const metadata = values?.length === 1 ? fieldMetadata(entries![values[0]!]!) : U;
    if (metadata === U) {
      // Still a oneof, one arm of which holds a numbered value beside another.
      if (values?.some((entry) => fieldMetadata(entries![entry]!) !== U)) numbered = true;
      stray = true;
      continue;
    }
    arms.push({ name: member.type === arrayTag ? `${key}_${metadata.number}` : values![0]!, key: values![0]!, value: entries![values![0]!]!, member, metadata });
  }
  if (!arms.length && !numbered) return U;
  if (defaulted) return panic(`S.protobuf: oneof "${key}" can't have a default. Drop it and add an arm for no member set`);
  if (stray) return panic(`S.protobuf: oneof "${key}" has an arm that doesn't hold one numbered value beside literals`);
  if (twice) return panic(`S.protobuf: oneof "${key}" has two arms for no member set, which a decode can't choose between. Keep one`);
  const members = arms.map((arm) => arm.member);
  if (unset !== U) members.push(unset);
  const told = tellApart(members, arms.map((arm) => arm.key), arms.map((arm) => arm.metadata.type === "message"));
  if ((arms.length > 1 || unset !== U) && told === U) {
    return panic(`S.protobuf: oneof "${key}" can't tell its arms apart. Give each a literal, such as case: "email", beside the value it holds`);
  }
  arms.forEach((arm, idx) => {
    const literal = told?.literals[idx];
    // Arms told apart by a number or a boolean share their value's key.
    arm.name = typeof literal === "string" ? literal : literal !== U ? `${key}_${arm.metadata.number}` : arm.name;
  });
  return { arms, unset, optional, all };
};

const literalSource = (value: unknown): string =>
  value === U ? "void 0" : typeof value === "bigint" ? `${value}n` : typeof value === "number" ? (Object.is(value, -0) ? "-0" : `${value}`) : JSON.stringify(value);

const armSource = (member: Internal, key: string | undefined, value: string): string => {
  if (key === U) return value;
  const definition = armDefinition(member, key, value) as Record<string, unknown>;
  const entries = Object.keys(definition);
  const sources = entries.map((entry) => (entry === key ? value : literalSource(definition[entry])));
  return Array.isArray(definition) ? `[${sources}]`
    : `{${entries.map((entry, idx) => `${entry === "__proto__" ? '["__proto__"]' : JSON.stringify(entry)}:${sources[idx]}`)}}`;
};

// A scalar whose value lands as the schema declares it, with no conversion: an
// enum of literals, unknown numbers included (the open enum proto3 specifies),
// and a float admitting NaN (`S.union([S.number, S.schema(NaN)])`, where
// `S.number` alone refuses it).
const keepsShape = (type: ProtobufType, shape: Internal): boolean =>
  (type === "enum" && (shape.type === anyOfTag || shape.const !== U)) ||
  ((type === "double" || type === "float") && !!shape.anyOf?.some((member) => member.const !== member.const));

const anyMessage = (shape: Internal, ctx: Ctx, where: string): Message => {
  const memo = "any" + shape.seq;
  let msg = ctx.messages.get(memo);
  if (msg !== U) return msg;
  // Numbered as an Any, a value with no message to unpack into is a mistake,
  // where `S.protobufAny.with(S.to, ...)` alone converts it as an object.
  const known = anyArms(shape, ctx);
  if (known === U) {
    return panic(`S.protobuf: ${where} is an Any, which takes a message S.protobufTypeName names, or an object or tuple holding one beside literals`);
  }
  const [arms, other, told] = known;
  const own = (number: number, key: string, type: ProtobufType): Field => ({ number, type, packed: true, key, repeated: false, optional: false, wire: wireType(type), fromWire: unknown, held: unknown, path: [] });
  const any: NonNullable<Message["any"]> = { members: [], fields: [], listed: [], told };
  msg = { fields: [own(1, "typeUrl", "string"), own(2, "value", "bytes")], object: shape, raw: shape, links: [], typeName: "google.protobuf.Any", any };
  ctx.messages.set(memo, msg);
  for (const [name, payload, key, member] of other === U ? arms : [...arms, other]) {
    const message = compileMessage(payload, ctx);
    if (message === U) return panic(`S.protobuf: ${where} holds ${name}, whose schema is not an object`);
    const field: Field = { number: msg.fields.length + 1, type: "message", packed: true, key: "c", repeated: false, optional: true, wire: 2, name, message, fromWire: payload, held: payload, path: key === U ? [] : [key], fromWireIsHeld: true };
    any.members.push(member);
    any.fields.push(field);
    msg.fields.push(field);
    if (name === "google.protobuf.Any") any.other = field;
    else any.listed.push(field);
  }
  return msg;
};

const wellKnownOf = (type: FieldType): WellKnownType | undefined => (type.startsWith("google.") ? (type as WellKnownType) : U);

// `wellKnown` is set for a Timestamp or Duration written as its own message,
// whose fields `seconds` and `nanos` are numbered by name, and for Empty.
const compileMessage = (schema: Internal, ctx: Ctx, wellKnown?: WellKnownType): Message | undefined => {
  const output = firstObject(schema, ctx);
  if (output === U || output.properties === U) return U;
  if (typeof output.additionalItems === objectTag) return U;
  const links: Internal[] = [];
  for (let current: Internal = schema; current !== output; ) {
    if (current.type !== refTag) current = current.to!;
    else {
      if (ctx.reads && (current.to !== U || current.refiner !== U || current.inputRefiner !== U)) links.unshift(current);
      current = current.definition!()!;
    }
  }
  const memo = wellKnown !== U ? wellKnown + output.seq : links.length ? `${links.map((link) => link.seq)}:${output.seq}` : output;
  const typeName = typeNameOf(schema, ctx);
  const built = ctx.messages.get(memo);
  if (built !== U) {
    // A recursive message reached first through a ref the user didn't name.
    built.typeName ??= typeName;
    if (ctx.stack.includes(built)) built.recursive = true;
    return built;
  }
  const msg: Message = {
    fields: [],
    object: output,
    raw: output,
    links,
    name: ctx.names.has(output) ? ctx.names.get(output) : output.name,
    wellKnown,
    typeName,
  };
  ctx.messages.set(memo, msg);
  ctx.stack.push(msg);
  const fields: Field[] = [];
  const numbers = new Set<number>();
  // A compiled message's reader hands back the value `at` declares, which
  // nothing converts again; a scalar, a wrapper's scalar and a codec's value
  // convert from what the wire read.
  // `whole` is what an Any holds, absent included.
  const holding = (m: StoredField, at: Internal, shape: Internal, where: string, whole = shape): [Message | undefined, Internal, Internal] => {
    const known = wellKnownOf(m.type);
    if (known === "google.protobuf.Any" && typeNameOf(shape, ctx) !== known) return [anyMessage(whole, ctx, where), at, at];
    if (m.type !== "message" && known === U) return [U, keepsShape(m.type as ProtobufType, shape) ? shape : scalarSchema(m.type as ProtobufType), at];
    const message = known !== U ? wellKnownMessage(known, shape, ctx, where) : compileMessage((ctx.writes && lastObject(at, ctx)) || at, ctx);
    if (message === U) return panic(`S.protobuf: ${where} is a message but its schema is not an object`);
    const raw = message.unwrap || message.codec ? message.raw : at;
    return [message, raw, message.codec ? refinedAs(at, raw) : at];
  };
  const keys = Object.keys(output.properties);
  for (let idx = 0; idx < keys.length; idx++) {
    const key = keys[idx]!;
    const property = output.properties[key]!;
    const metadata = fieldMetadata(property) ?? (wellKnown !== U ? secondsNanos[key] : U);
    if (metadata === U) {
      const group = oneofOf(key, property);
      if (group === U) return panic(`S.protobuf: field "${key}" has no field number. Give it one with S.protobufField, or make it a oneof: a union of arms, each holding one numbered value`);
      const { arms, unset: unsetArm, optional: groupOptional, all } = group;
      if (unsetArm !== U && groupOptional) {
        return panic(`S.protobuf: oneof "${key}" is both optional and has an arm for no member set, which the wire can't tell apart. Drop S.optional or that arm`);
      }
      const armFields: Field[] = [];
      for (let armIdx = 0; armIdx < arms.length; armIdx++) {
        const arm = arms[armIdx]!;
        const m = arm.metadata;
        const where = `oneof "${key}" case "${arm.name}"`;
        if (numbers.has(m.number)) return panic(`S.protobuf: field number ${m.number} of "${key}" is already taken`);
        numbers.add(m.number);
        const [armValue, armOptional] = unwrapOptional(arm.value);
        const armShape = getOutputSchema(armValue);
        const known = wellKnownOf(m.type);
        if (armOptional && known !== U && wrappers[known] !== U) {
          return panic(`S.protobuf: ${where} can't unwrap ${known}, which protobuf-es keeps as its message in a oneof. Hold the message, such as ${known.slice(16)}Schema from sury/protobuf/wkt`);
        }
        if (armOptional || (isContainer(armShape) && !(known !== U && wellKnownTakes(known, armShape)))) {
          return panic(`S.protobuf: ${where} must hold one value, not an optional, a list or a map`);
        }
        const [message, raw, read] = holding(m, arm.value, armShape, where);
        const type = message !== U ? "message" : (m.type as ProtobufType);
        const field: Field = {
          number: m.number,
          type,
          packed: m.packed,
          key,
          repeated: false,
          // Absent unless its case is set, so it never builds a default instance.
          optional: true,
          message,
          oneof: key,
          wire: wireType(type),
          name: arm.name,
          armValue: arm.value,
          text: armShape.type === stringTag && type.includes("64"),
          fromWire: raw,
          held: read,
          path: arm.key === U ? [key] : [key, arm.key],
          fromWireIsHeld: raw === read,
        };
        fields.push(field);
        armFields.push(field);
      }
      (msg.oneofs ??= {})[key] = { union: getOutputSchema(property), members: all.map((member) => [member, armFields[arms.findIndex((arm) => arm.member === member)]]) };
      continue;
    }
    if (numbers.has(metadata.number)) return panic(`S.protobuf: field number ${metadata.number} of "${key}" is already taken`);
    numbers.add(metadata.number);
    const [propertyValue, optional, absent] = unwrapOptional(property);
    let shape = getOutputSchema(propertyValue);
    const container = shape;
    let repeated = false;
    let map: ProtobufType | undefined;
    const known = wellKnownOf(metadata.type);
    // A well-known value can be a list or a map itself - a FieldMask, a
    // Struct - so it is one of the type before it is a container of them.
    const single = known !== U && wellKnownTakes(known, shape);
    if (!single && shape.type === arrayTag && typeof shape.additionalItems === objectTag) {
      if (optional) return panic(`S.protobuf: repeated field "${key}" can't be optional. An absent list decodes to []`);
      repeated = true;
      shape = getOutputSchema(shape.additionalItems as Internal);
    } else if (!single && shape.type === objectTag && typeof shape.additionalItems === objectTag) {
      if (optional) return panic(`S.protobuf: map field "${key}" can't be optional. An absent map decodes to {}`);
      map = metadata.key;
      shape = getOutputSchema(shape.additionalItems as Internal);
    }
    const item = repeated || map !== U ? (container.additionalItems as Internal) : U;
    let [message, raw, read] = holding(metadata, item ?? propertyValue, shape, `field "${key}"`, getOutputSchema(item ?? property));
    const done = raw === read;
    // A repeated or map field reads into the user's container, its length
    // checks included.
    if (repeated || map !== U) {
      if (done || message === U) read = property;
      else {
        const items = copySchema(container);
        items.additionalItems = read;
        read = items;
      }
      raw = done ? read : repeated ? arrayFactory(raw) : dictFactory(raw);
    } else if (absent !== U) {
      read = property;
      raw = defaultedOptional(raw, done ? U : message ? read : getOutputSchema(propertyValue), absent);
    } else if (optional) {
      if (done) read = raw = property;
      // A literal or refined scalar has a check to run, which only a present
      // value can pass.
      else if (raw !== getOutputSchema(propertyValue)) raw = optionalMessage(raw);
    }
    const type = message !== U ? "message" : (metadata.type as ProtobufType);
    const field: Field = {
      number: metadata.number,
      type,
      packed: metadata.packed,
      key,
      repeated,
      optional,
      message,
      map,
      oneof: metadata.oneof,
      wire: wireType(type),
      text: shape.type === stringTag && type.includes("64"),
      fromWire: raw,
      held: read,
      path: [key],
      omit: optional && absent === U,
      fromWireIsHeld: done,
    };
    fields.push(field);
  }
  fields.sort((a, b) => a.number - b.number);
  msg.fields = fields;
  ctx.stack.pop();
  if (ctx.stack.length === 0) rejectEndless(ctx);
  return msg;
};

const textDecoder = /* @__PURE__ */ new TextDecoder("utf-8", { fatal: true, ignoreBOM: true });
const textEncoder = /* @__PURE__ */ new TextEncoder();

// Every `new` here is annotated: esbuild keeps an unannotated constructor
// call, and with it this whole module, in every consumer's bundle.
const scratch = /* @__PURE__ */ new Uint8Array(8);
const scratchView = /* @__PURE__ */ new DataView(scratch.buffer);

// A protobuf failure is a Sury conversion failure from the throw on, sited
// here until `guarded` moves it onto the operation's own: no throw site knows
// the schemas the operation converts between. An encode failure is thrown with
// no reason, only the value, the element of a packed list, and `kind` (1 a map
// key, 2 a oneof member set beside another): a write can't name its field - a
// message's encoder is shared by every field that holds it - so the frame of
// the encoder it unwinds through settles the rest (`encodeFrame`). An unsited
// record already reads as an S.Error, so nothing between a throw and `guarded`
// may catch Sury errors.
const unsited = /* @__PURE__ */ conversionSite(U as never, U as never);

type Failure = SuryErrorRecord & { value?: unknown; index?: number; kind?: number };

const isFailure = (x: unknown): x is Failure => x != null && Object.getPrototypeOf(x) === unsited;

const failure = (reason?: string): Failure => {
  const record = Object.create(unsited) as Failure;
  record.code = "invalid_conversion";
  record.path = [];
  if (reason !== U) record.reason = reason;
  return record;
};

const fail = (reason: string): never => {
  throw failure(reason);
};

const truncated = (): never => fail("Truncated protobuf message");

// Every varint reader accepts the full 10-byte form and keeps the bits it
// has room for: that is what makes `int32` -1 (10 bytes on the wire) and a
// `uint32` written from a 64-bit value decode the way C++ does.
export class Reader {
  pos = 0;
  buf: Uint8Array;
  limit: number;
  constructor(buf: Uint8Array) {
    this.buf = buf;
    this.limit = buf.length;
  }
  busy = false;
  // Same reentrancy rule as Writer.acquire: a decode started from inside
  // another gets its own reader. Released by the generated code.
  acquire(buf: Uint8Array): Reader {
    // A decode skips validation, so anything can arrive here. Without this an
    // input with no `length` (an `ArrayBuffer`, a plain object) would set the
    // limit to `undefined`, leave the tag loop on the first test and hand back
    // an all-defaults message no caller could tell from a genuinely empty one.
    if (!(buf instanceof Uint8Array)) fail(`Expected Uint8Array, received ${stringify(buf)}`);
    const reader = this.busy ? new Reader(buf) : this;
    reader.busy = true;
    reader.buf = buf;
    reader.pos = 0;
    reader.limit = buf.length;
    reader.n = 0;
    return reader;
  }
  varint32(): number {
    const buf = this.buf;
    const limit = this.limit;
    let pos = this.pos;
    if (pos >= limit) truncated();
    let byte = buf[pos++]!;
    let value = byte & 127;
    if (byte > 127) {
      if (pos >= limit) truncated();
      byte = buf[pos++]!;
      value |= (byte & 127) << 7;
      if (byte > 127) {
        if (pos >= limit) truncated();
        byte = buf[pos++]!;
        value |= (byte & 127) << 14;
        if (byte > 127) {
          if (pos >= limit) truncated();
          byte = buf[pos++]!;
          value |= (byte & 127) << 21;
          if (byte > 127) {
            if (pos >= limit) truncated();
            byte = buf[pos++]!;
            value |= (byte & 15) << 28;
            let extra = 5;
            while (byte > 127) {
              if (pos >= limit) truncated();
              if (++extra > 10) fail("Varint exceeds 10 bytes");
              byte = buf[pos++]!;
            }
          }
        }
      }
    }
    this.pos = pos;
    return value >>> 0;
  }
  // A length is its whole varint. A value keeps the bits it has room for, but a
  // length with a bit above 32 is one no message holds, and read the way a
  // value is it wraps to a small one that does: `2^32` reading as 0.
  len(): number {
    const start = this.pos;
    const value = this.varint32();
    if (this.pos - start > 4) {
      const buf = this.buf;
      let high = buf[start + 4]! & 112;
      for (let i = start + 5; i < this.pos; i++) high |= buf[i]! & 127;
      if (high) truncated();
    }
    return value;
  }
  // Unlike a value, a tag is held to 5 bytes and 32 bits: the conformance
  // suite rejects an overlong tag that a lenient read would accept.
  tag(): number {
    const buf = this.buf;
    const limit = this.limit;
    let pos = this.pos;
    let value = 0;
    let shift = 0;
    let byte: number;
    do {
      if (pos >= limit) truncated();
      if (shift > 28) fail("Invalid protobuf tag");
      byte = buf[pos++]!;
      value |= (byte & 127) << shift;
      shift += 7;
    } while (byte > 127);
    if (shift > 28 && byte > 15) fail("Invalid protobuf tag");
    this.pos = pos;
    return value >>> 0;
  }
  varint64(): bigint {
    const buf = this.buf;
    const limit = this.limit;
    let pos = this.pos;
    let lo = 0;
    let hi = 0;
    let shift = 0;
    let byte: number;
    do {
      if (pos >= limit) truncated();
      if (shift > 63) fail("Varint exceeds 10 bytes");
      byte = buf[pos++]!;
      if (shift < 28) lo |= (byte & 127) << shift;
      else if (shift === 28) {
        lo |= (byte & 15) << 28;
        hi = (byte & 127) >>> 4;
      } else hi |= (byte & 127) << (shift - 32);
      shift += 7;
    } while (byte > 127);
    this.pos = pos;
    return hi >>> 21 === 0 ? BigInt(hi * 4294967296 + (lo >>> 0)) : (BigInt(hi >>> 0) << 32n) | BigInt(lo >>> 0);
  }
  // Signed reading of the same varint: a value within 2^53 either way is
  // built from one BigInt of a float instead of a shift and an or.
  int64(): bigint {
    const buf = this.buf;
    const limit = this.limit;
    let pos = this.pos;
    let lo = 0;
    let hi = 0;
    let shift = 0;
    let byte: number;
    do {
      if (pos >= limit) truncated();
      if (shift > 63) fail("Varint exceeds 10 bytes");
      byte = buf[pos++]!;
      if (shift < 28) lo |= (byte & 127) << shift;
      else if (shift === 28) {
        lo |= (byte & 15) << 28;
        hi = (byte & 127) >>> 4;
      } else hi |= (byte & 127) << (shift - 32);
      shift += 7;
    } while (byte > 127);
    this.pos = pos;
    if (hi >>> 21 === 0) return BigInt(hi * 4294967296 + (lo >>> 0));
    if (hi >>> 21 === 2047) return BigInt(hi * 4294967296 + (lo >>> 0));
    return BigInt.asIntN(64, (BigInt(hi >>> 0) << 32n) | BigInt(lo >>> 0));
  }
  bool(): boolean {
    const buf = this.buf;
    const limit = this.limit;
    let pos = this.pos;
    let set = 0;
    let byte: number;
    let count = 0;
    do {
      if (pos >= limit) truncated();
      if (++count > 10) fail("Varint exceeds 10 bytes");
      byte = buf[pos++]!;
      set |= byte & 127;
    } while (byte > 127);
    this.pos = pos;
    return set !== 0;
  }
  u32(): number {
    const pos = this.pos;
    if (pos + 4 > this.limit) truncated();
    const buf = this.buf;
    this.pos = pos + 4;
    return (buf[pos]! | (buf[pos + 1]! << 8) | (buf[pos + 2]! << 16) | (buf[pos + 3]! << 24)) >>> 0;
  }
  f32(): number {
    const pos = this.pos;
    if (pos + 4 > this.limit) truncated();
    const buf = this.buf;
    scratch[0] = buf[pos]!;
    scratch[1] = buf[pos + 1]!;
    scratch[2] = buf[pos + 2]!;
    scratch[3] = buf[pos + 3]!;
    this.pos = pos + 4;
    return scratchView.getFloat32(0, true);
  }
  load64(): void {
    const pos = this.pos;
    if (pos + 8 > this.limit) truncated();
    const buf = this.buf;
    scratch[0] = buf[pos]!;
    scratch[1] = buf[pos + 1]!;
    scratch[2] = buf[pos + 2]!;
    scratch[3] = buf[pos + 3]!;
    scratch[4] = buf[pos + 4]!;
    scratch[5] = buf[pos + 5]!;
    scratch[6] = buf[pos + 6]!;
    scratch[7] = buf[pos + 7]!;
    this.pos = pos + 8;
  }
  f64(): number {
    this.load64();
    return scratchView.getFloat64(0, true);
  }
  u64(): bigint {
    this.load64();
    return scratchView.getBigUint64(0, true);
  }
  i64(): bigint {
    this.load64();
    return scratchView.getBigInt64(0, true);
  }
  // A packed varint field read in one method per kind: the loop keeps `pos`
  // in a local where generated code would pay a property read and write on
  // the reader per element, and each kind owns its `push` site so the
  // arrays it fills stay monomorphic. One and two bytes are read in the loop,
  // which is every value under 16384 (a tile's geometry); a longer element
  // takes the ordinary reader path.
  u32s(out: number[]): void {
    const buf = this.buf;
    const limit = this.limit;
    let pos = this.pos;
    while (pos < limit) {
      const byte = buf[pos]!;
      let next: number;
      if (byte < 128) {
        pos++;
        out.push(byte);
      } else if (pos + 1 < limit && (next = buf[pos + 1]!) < 128) {
        pos += 2;
        out.push((byte & 127) | (next << 7));
      } else {
        this.pos = pos;
        out.push(this.varint32());
        pos = this.pos;
      }
    }
    this.pos = pos;
  }
  i32s(out: number[]): void {
    const buf = this.buf;
    const limit = this.limit;
    let pos = this.pos;
    while (pos < limit) {
      const byte = buf[pos]!;
      let next: number;
      if (byte < 128) {
        pos++;
        out.push(byte);
      } else if (pos + 1 < limit && (next = buf[pos + 1]!) < 128) {
        pos += 2;
        out.push((byte & 127) | (next << 7));
      } else {
        this.pos = pos;
        out.push(this.varint32() | 0);
        pos = this.pos;
      }
    }
    this.pos = pos;
  }
  s32s(out: number[]): void {
    const buf = this.buf;
    const limit = this.limit;
    let pos = this.pos;
    while (pos < limit) {
      let value = buf[pos]!;
      let next: number;
      if (value < 128) pos++;
      else if (pos + 1 < limit && (next = buf[pos + 1]!) < 128) {
        pos += 2;
        value = (value & 127) | (next << 7);
      } else {
        this.pos = pos;
        value = this.varint32();
        pos = this.pos;
      }
      out.push(((value >>> 1) ^ -(value & 1)) | 0);
    }
    this.pos = pos;
  }
  bools(out: boolean[]): void {
    const buf = this.buf;
    const limit = this.limit;
    let pos = this.pos;
    while (pos < limit) {
      const byte = buf[pos]!;
      if (byte < 128) {
        pos++;
        out.push(byte !== 0);
      } else {
        this.pos = pos;
        out.push(this.bool());
        pos = this.pos;
      }
    }
    this.pos = pos;
  }
  // Packed fixed-width fields: an aligned span is read through a typed array
  // over the input, anything else through a DataView, both created once per
  // field instead of a scratch copy per element. `kind` is 0 double, 1 float,
  // 2 fixed32, 3 sfixed32, 4 fixed64, 5 sfixed64.
  fixeds(out: unknown[], kind: number): void {
    const buf = this.buf;
    const start = this.pos;
    const end = this.limit;
    const size = kind === 1 || kind === 2 || kind === 3 ? 4 : 8;
    // A torn last element fails after the whole ones before it are in, so the
    // failure's path names the element that was torn.
    const torn = (end - start) % size;
    const len = end - start - torn;
    const n = len / size;
    const offset = buf.byteOffset + start;
    this.pos = end;
    if (offset % size === 0) {
      const view =
        kind === 0 ? new Float64Array(buf.buffer, offset, n)
        : kind === 1 ? new Float32Array(buf.buffer, offset, n)
        : kind === 2 ? new Uint32Array(buf.buffer, offset, n)
        : kind === 3 ? new Int32Array(buf.buffer, offset, n)
        : kind === 4 ? new BigUint64Array(buf.buffer, offset, n)
        : new BigInt64Array(buf.buffer, offset, n);
      for (let i = 0; i < n; i++) out.push(view[i]);
    } else {
      const view = new DataView(buf.buffer, offset, len);
      for (let i = 0; i < len; i += size) {
        out.push(
          kind === 0 ? view.getFloat64(i, true)
          : kind === 1 ? view.getFloat32(i, true)
          : kind === 2 ? view.getUint32(i, true)
          : kind === 3 ? view.getInt32(i, true)
          : kind === 4 ? view.getBigUint64(i, true)
          : view.getBigInt64(i, true),
        );
      }
    }
    if (torn) truncated();
  }
  // Skips a field a well-known type's codec does not read, as a compiled
  // message skips one it does not declare.
  skipTag(tag: number): void {
    if (tag < 8) fail("Invalid protobuf field number 0");
    skip(this, tag & 7, tag >>> 3, 0);
  }
  // The payloads a field seen again was given past its first, kept for the
  // read after the loop: each as its range and the next one's index, the
  // first also holding the last's. Reused from one decode to the next, so a
  // merge allocates nothing once it has grown.
  x: number[] = [];
  n = 0;
  more(first?: number): number {
    const x = this.x;
    const at = this.n;
    this.n = at + 4;
    x[at] = this.pos;
    x[at + 1] = this.limit;
    x[at + 2] = -1;
    x[at + 3] = at;
    if (first === U) return at;
    x[x[first + 3]! + 2] = at;
    x[first + 3] = at;
    return first;
  }
  // Enters a length-delimited field: narrows `limit` to it and returns the
  // outer limit for the caller to restore. Bounds checks against `limit`
  // are what stop a nested read escaping its field.
  sub(): number {
    const len = this.len();
    const end = this.pos + len;
    if (end > this.limit) truncated();
    const outer = this.limit;
    this.limit = end;
    return outer;
  }
  string(): string {
    const len = this.len();
    const start = this.pos;
    const end = start + len;
    if (end > this.limit) truncated();
    const buf = this.buf;
    this.pos = end;
    // ASCII up to 48 bytes builds the string eight chars a call, which beats
    // TextDecoder's fixed cost; a byte over 127 hands the span to it.
    if (len < 48) {
      let s = "";
      let i = start;
      let ascii = 0;
      for (; i + 8 <= end; i += 8) {
        const a = buf[i]!, b = buf[i + 1]!, c = buf[i + 2]!, d = buf[i + 3]!;
        const e = buf[i + 4]!, f = buf[i + 5]!, g = buf[i + 6]!, h = buf[i + 7]!;
        ascii |= a | b | c | d | e | f | g | h;
        if (ascii > 127) break;
        s += String.fromCharCode(a, b, c, d, e, f, g, h);
      }
      if (ascii < 128) {
        for (; i < end; i++) {
          const c = buf[i]!;
          if (c > 127) break;
          s += String.fromCharCode(c);
        }
        if (i === end) return s;
      }
    }
    try {
      return textDecoder.decode(buf.subarray(start, end));
    } catch {
      return fail("Protobuf string is not valid UTF-8");
    }
  }
  bytes(): Uint8Array {
    const len = this.len();
    const end = this.pos + len;
    if (end > this.limit) truncated();
    const value = this.buf.slice(this.pos, end);
    this.pos = end;
    return value;
  }
}

const scratchReader = /* @__PURE__ */ new Reader(/* @__PURE__ */ new Uint8Array(0));

// Messages are written back to back into one slab and handed out as views
// of it, the way Node's Buffer pool works: a typed array over 64 bytes is
// allocated off-heap, and that allocation cost more than encoding the
// message it held. `base` is where the message being written starts.
export class Writer {
  buf = new Uint8Array(8192);
  pos = 0;
  base = 0;
  // Growth keeps every position: a length hole a caller holds is an absolute
  // index, so the buffer is copied from 0 rather than from `base`.
  ensure(n: number): void {
    if (this.pos + n <= this.buf.length) return;
    const next = new Uint8Array(Math.max(this.buf.length * 2, this.pos + n));
    next.set(this.buf.subarray(0, this.pos));
    this.buf = next;
  }
  varint32(value: number): void {
    this.ensure(5);
    const buf = this.buf;
    let pos = this.pos;
    value >>>= 0;
    while (value > 127) {
      buf[pos++] = (value & 127) | 128;
      value >>>= 7;
    }
    buf[pos++] = value;
    this.pos = pos;
  }
  // A negative int32 is sign-extended to 64 bits on the wire: the low 32
  // bits, then 0xF0-masked byte 5, four 0xFF bytes and a closing 0x01.
  int32(value: number): void {
    if (value >= 0) return this.varint32(value);
    this.ensure(10);
    const buf = this.buf;
    let pos = this.pos;
    buf[pos++] = (value & 127) | 128;
    buf[pos++] = ((value >>> 7) & 127) | 128;
    buf[pos++] = ((value >>> 14) & 127) | 128;
    buf[pos++] = ((value >>> 21) & 127) | 128;
    buf[pos++] = (value >>> 28) | 240;
    buf[pos++] = 255;
    buf[pos++] = 255;
    buf[pos++] = 255;
    buf[pos++] = 255;
    buf[pos++] = 1;
    this.pos = pos;
  }
  varint64(value: bigint): void {
    if (value >= 0n && value < 2147483648n) return this.varint32(Number(value));
    let lo: number;
    let hi: number;
    // Within 2^53 the halves come from float arithmetic; `>>> 0` on the
    // negative high word is the two's complement the wire wants.
    if (value >= -9007199254740992n && value <= 9007199254740992n) {
      const num = Number(value);
      hi = Math.floor(num / 4294967296);
      lo = num - hi * 4294967296;
      hi >>>= 0;
    } else {
      value = BigInt.asUintN(64, value);
      lo = Number(value & 4294967295n);
      hi = Number(value >> 32n);
    }
    this.ensure(10);
    const buf = this.buf;
    let pos = this.pos;
    while (hi) {
      buf[pos++] = (lo & 127) | 128;
      lo = ((lo >>> 7) | (hi << 25)) >>> 0;
      hi >>>= 7;
    }
    while (lo > 127) {
      buf[pos++] = (lo & 127) | 128;
      lo >>>= 7;
    }
    buf[pos++] = lo;
    this.pos = pos;
  }
  // The packed-loop twin of Reader.varints, with the buffer sized once for
  // the whole field. `kind` is 0 uint32, 1 int32/enum, 2 sint32, 3 bool.
  varints(values: ArrayLike<number | boolean>, kind: number): void {
    const n = values.length;
    this.ensure(n * 10);
    const buf = this.buf;
    let pos = this.pos;
    for (let i = 0; i < n; i++) {
      let value = Number(values[i]);
      if (kind === 3) value = value ? 1 : 0;
      else if (kind === 0 ? value < 0 || value > 4294967295 : value < -2147483648 || value > 2147483647) {
        fieldFailure(value, i);
      }
      if (kind === 2) value = ((value << 1) ^ (value >> 31)) >>> 0;
      else if (kind === 1 && value < 0) {
        buf[pos++] = (value & 127) | 128;
        buf[pos++] = ((value >>> 7) & 127) | 128;
        buf[pos++] = ((value >>> 14) & 127) | 128;
        buf[pos++] = ((value >>> 21) & 127) | 128;
        buf[pos++] = (value >>> 28) | 240;
        buf[pos++] = 255;
        buf[pos++] = 255;
        buf[pos++] = 255;
        buf[pos++] = 255;
        buf[pos++] = 1;
        continue;
      } else value >>>= 0;
      while (value > 127) {
        buf[pos++] = (value & 127) | 128;
        value >>>= 7;
      }
      buf[pos++] = value;
    }
    this.pos = pos;
  }
  // Packed fixed-width twin of Reader.fixeds: one typed-array `set` when the
  // slab position is aligned, a DataView loop otherwise.
  fixeds(values: ArrayLike<number | bigint>, kind: number): void {
    const n = values.length;
    const size = kind === 1 || kind === 2 || kind === 3 ? 4 : 8;
    this.ensure(n * size);
    const buf = this.buf;
    const offset = buf.byteOffset + this.pos;
    if (kind === 1) {
      for (let i = 0; i < n; i++) {
        const value = values[i] as number;
        if (Number.isFinite(value) && Math.abs(value) > 3.4028234663852886e38) fieldFailure(value, i);
      }
    } else if (kind) {
      const min = [0, -2147483648, 0n, -9223372036854775808n][kind - 2]!;
      const max = [4294967295, 2147483647, 18446744073709551615n, 9223372036854775807n][kind - 2]!;
      for (let i = 0; i < n; i++) checked(values[i], min, max, i);
    }
    if (offset % size === 0) {
      (kind === 0 ? new Float64Array(buf.buffer, offset, n)
        : kind === 1 ? new Float32Array(buf.buffer, offset, n)
        : kind === 2 ? new Uint32Array(buf.buffer, offset, n)
        : kind === 3 ? new Int32Array(buf.buffer, offset, n)
        : kind === 4 ? new BigUint64Array(buf.buffer, offset, n)
        : new BigInt64Array(buf.buffer, offset, n)
      ).set(values as never);
    } else {
      const view = new DataView(buf.buffer, offset, n * size);
      for (let i = 0; i < n; i++) {
        const value = values[i]!;
        if (kind === 0) view.setFloat64(i * 8, value as number, true);
        else if (kind === 1) view.setFloat32(i * 4, value as number, true);
        else if (kind === 2) view.setUint32(i * 4, value as number, true);
        else if (kind === 3) view.setInt32(i * 4, value as number, true);
        else if (kind === 4) view.setBigUint64(i * 8, value as bigint, true);
        else view.setBigInt64(i * 8, value as bigint, true);
      }
    }
    this.pos += n * size;
  }
  bytes(value: Uint8Array): void {
    // Anything else has a `length` or not and writes zeros or nothing, and
    // either one decodes back as some other value with no failure at all.
    if (!(value instanceof Uint8Array)) fieldFailure(value);
    const n = value.length;
    this.ensure(5 + n);
    if (n < 128) this.buf[this.pos++] = n;
    else this.varint32(n);
    this.buf.set(value, this.pos);
    this.pos += n;
  }
  string(v: string): void {
    // Otherwise a non-string fails deep in the length arithmetic, as a
    // `RangeError` that names nothing.
    if (typeof v !== "string") fieldFailure(v);
    this.text(v);
  }
  text(v: string): void {
    const len = v.length;
    if (len < 32) {
      let i = 0;
      for (; i < len; i++) {
        if (v.charCodeAt(i) > 127) break;
      }
      if (i === len) {
        this.ensure(1 + len);
        const buf = this.buf;
        let pos = this.pos;
        buf[pos++] = len;
        for (i = 0; i < len; i++) buf[pos++] = v.charCodeAt(i);
        this.pos = pos;
        return;
      }
    }
    // The length prefix is written after the text, so the text goes in at
    // the widest prefix it could need and slides back when it is shorter.
    const max = len * 3;
    const width = max < 128 ? 1 : max < 16384 ? 2 : max < 2097152 ? 3 : max < 268435456 ? 4 : 5;
    this.ensure(width + max);
    const start = this.pos + width;
    const n = textEncoder.encodeInto(v, this.buf.subarray(start, start + max)).written;
    let used = 1;
    for (let rest = n >>> 7; rest; rest >>>= 7) used++;
    if (used !== width) this.buf.copyWithin(this.pos + used, start, start + n);
    this.varint32(n);
    this.pos += n;
  }
  float32(value: number): void {
    if (Number.isFinite(value) && Math.abs(value) > 3.4028234663852886e38) fieldFailure(value);
    scratchView.setFloat32(0, value, true);
    this.ensure(4);
    const buf = this.buf;
    const pos = this.pos;
    buf[pos] = scratch[0]!;
    buf[pos + 1] = scratch[1]!;
    buf[pos + 2] = scratch[2]!;
    buf[pos + 3] = scratch[3]!;
    this.pos = pos + 4;
  }
  float64(value: number): void {
    scratchView.setFloat64(0, value, true);
    this.store64();
  }
  bits32(value: number): void {
    this.ensure(4);
    const buf = this.buf;
    const pos = this.pos;
    buf[pos] = value & 255;
    buf[pos + 1] = (value >>> 8) & 255;
    buf[pos + 2] = (value >>> 16) & 255;
    buf[pos + 3] = value >>> 24;
    this.pos = pos + 4;
  }
  bits64(value: bigint): void {
    scratchView.setBigUint64(0, BigInt.asUintN(64, value), true);
    this.store64();
  }
  store64(): void {
    this.ensure(8);
    const buf = this.buf;
    const pos = this.pos;
    buf[pos] = scratch[0]!;
    buf[pos + 1] = scratch[1]!;
    buf[pos + 2] = scratch[2]!;
    buf[pos + 3] = scratch[3]!;
    buf[pos + 4] = scratch[4]!;
    buf[pos + 5] = scratch[5]!;
    buf[pos + 6] = scratch[6]!;
    buf[pos + 7] = scratch[7]!;
    this.pos = pos + 8;
  }
  finish(): Uint8Array {
    const out = this.buf.subarray(this.base, this.pos);
    this.base = this.pos;
    this.busy = false;
    return out;
  }
  // Opens a length-delimited field with a 5-byte hole for the prefix;
  // `end` writes the real prefix and slides the payload back over the slack.
  begin(): number {
    this.ensure(5);
    const hole = this.pos;
    this.pos += 5;
    return hole;
  }
  end(hole: number): void {
    const start = hole + 5;
    const len = this.pos - start;
    let used = 1;
    for (let rest = len >>> 7; rest; rest >>>= 7) used++;
    if (used !== 5) {
      this.buf.copyWithin(hole + used, start, this.pos);
      this.pos -= 5 - used;
    }
    this.pos = hole;
    this.varint32(len);
    this.pos += len;
  }
  busy = false;
  // The writer an encode started in the middle of this one takes - a field
  // getter's, a custom coder's, an Any packing its message - kept for the
  // next, so a nested encode allocates no buffer once warm.
  inner?: Writer;
  // A busy scratch writer hands out the next level rather than resetting
  // under the outer call.
  acquire(): Writer {
    if (this.busy) return (this.inner ??= new Writer()).acquire();
    this.busy = true;
    if (this.buf.length - this.base < 1024) {
      this.buf = new Uint8Array(8192);
      this.base = 0;
    }
    this.pos = this.base;
    return this;
  }
}

const scratchWriter = /* @__PURE__ */ new Writer();

const skip = (reader: Reader, wire: number, fieldNumber: number, depth: number): void => {
  if (wire === 0) reader.varint64();
  else if (wire === 1) reader.load64();
  else if (wire === 2) {
    const outer = reader.sub();
    reader.pos = reader.limit;
    reader.limit = outer;
  } else if (wire === 5) reader.u32();
  else if (wire === 3) {
    if (depth >= 100) fail("Protobuf group nesting limit exceeded");
    while (reader.pos < reader.limit) {
      const tag = reader.tag();
      const number = tag >>> 3;
      const nestedWire = tag & 7;
      if (number === 0) fail("Invalid protobuf field number 0");
      if (nestedWire === 4) {
        if (number !== fieldNumber) fail("Mismatched protobuf end group");
        return;
      }
      skip(reader, nestedWire, number, depth + 1);
    }
    fail("Unterminated protobuf group");
    // Wire 4 only ever closes a group, and the loop above consumes the one
    // that does. Reaching it here means an end tag no start tag opened.
  } else if (wire === 4) fail("Unmatched protobuf end group");
  else fail(`Invalid protobuf wire type ${wire}`);
};

// A Timestamp as a `Date`: seconds (1) and nanos (2) since the epoch, read to
// the millisecond a Date holds. Seen again, it merges into the one before
// field by field. Neither end checks Google's year 1 to 9999: the binary format
// never has, and a Date past it is still a Date.
const readDate = (r: Reader, _d: number, prev?: unknown): Date => {
  let ms = prev === U ? 0 : (prev as Date).getTime();
  let seconds = Math.floor(ms / 1000);
  let nanos = (ms - seconds * 1000) * 1e6;
  while (r.pos < r.limit) {
    const tag = r.tag();
    if (tag === 8) seconds = Number(r.int64());
    else if (tag === 16) nanos = r.varint32() | 0;
    else r.skipTag(tag);
  }
  ms = seconds * 1000 + Math.trunc(nanos / 1e6);
  if (!(Math.abs(ms) <= 864e13)) fail("Protobuf Timestamp is outside the range of a Date");
  return new Date(ms);
};

const writeDate = (w: Writer, value: unknown): void => {
  const ms = value instanceof Date ? value.getTime() : NaN;
  if (ms !== ms) fail(`Expected Date, received ${value instanceof Date ? "invalid Date" : stringify(value)}`);
  const seconds = Math.floor(ms / 1000);
  const nanos = (ms - seconds * 1000) * 1e6;
  if (seconds) {
    w.varint32(8);
    w.varint64(BigInt(seconds));
  }
  if (nanos) {
    w.varint32(16);
    w.varint32(nanos);
  }
};

const isStruct = (value: unknown): value is Record<string, unknown> =>
  value !== null && typeof value === "object" && !Array.isArray(value);

// A Struct's fields are a `map<string, Value>`: a key repeated later replaces
// the entry, and one named `__proto__` stays an own property. Seen again, a
// Struct merges into the one before.
const readStruct = (r: Reader, d: number, prev?: unknown): Record<string, unknown> => {
  const struct = isStruct(prev) ? prev : {};
  while (r.pos < r.limit) {
    const tag = r.tag();
    if (tag !== 10) {
      r.skipTag(tag);
      continue;
    }
    const entry = r.sub();
    let key = "";
    let value: unknown = null;
    while (r.pos < r.limit) {
      const inner = r.tag();
      if (inner === 10) key = r.string();
      else if (inner === 18) {
        const end = r.sub();
        value = readValue(r, d + 1, value);
        r.limit = end;
      } else r.skipTag(inner);
    }
    r.limit = entry;
    if (key === "__proto__") Object.defineProperty(struct, key, { value, enumerable: true, writable: true, configurable: true });
    else struct[key] = value;
  }
  return struct;
};

// Seen again, a ListValue appends to the one before, as a repeated field does.
const readList = (r: Reader, d: number, prev?: unknown): unknown[] => {
  const list = Array.isArray(prev) ? prev : [];
  while (r.pos < r.limit) {
    const tag = r.tag();
    if (tag === 10) {
      const end = r.sub();
      list.push(readValue(r, d + 1));
      r.limit = end;
    } else r.skipTag(tag);
  }
  return list;
};

// A Value is the JSON it holds, a Struct its objects and a ListValue its
// arrays. One with no kind set reads as `null`, the kind `null_value` names.
// Seen again, it merges the way a oneof member does: a Struct into a Struct and
// a list onto a list, anything else replaced.
const readValue = (r: Reader, d: number, prev: unknown = null): unknown => {
  if (d >= 100) fail("Protobuf message nesting limit exceeded");
  let value = prev;
  while (r.pos < r.limit) {
    const tag = r.tag();
    if (tag === 8) {
      r.varint64();
      value = null;
    } else if (tag === 17) value = r.f64();
    else if (tag === 26) value = r.string();
    else if (tag === 32) value = r.bool();
    else if (tag === 42 || tag === 50) {
      const end = r.sub();
      value = tag === 42 ? readStruct(r, d + 1, value) : readList(r, d + 1, value);
      r.limit = end;
    } else r.skipTag(tag);
  }
  return value;
};

const writeStruct = (w: Writer, value: unknown): void => {
  const keys = Object.keys(value as object);
  for (let i = 0; i < keys.length; i++) {
    w.varint32(10);
    const entry = w.begin();
    w.varint32(10);
    w.text(keys[i]!);
    w.varint32(18);
    const item = w.begin();
    writeValue(w, (value as Record<string, unknown>)[keys[i]!]);
    w.end(item);
    w.end(entry);
  }
};

const writeList = (w: Writer, value: unknown): void => {
  for (let i = 0; i < (value as unknown[]).length; i++) {
    w.varint32(10);
    const item = w.begin();
    writeValue(w, (value as unknown[])[i]);
    w.end(item);
  }
};

const writeValue = (w: Writer, value: unknown): void => {
  if (value === null) {
    // A oneof member is written even at its default.
    w.varint32(8);
    w.varint32(0);
  } else if (typeof value === "number") {
    w.varint32(17);
    w.float64(value);
  } else if (typeof value === "string") {
    w.varint32(26);
    w.text(value);
  } else if (typeof value === "boolean") {
    w.varint32(32);
    w.varint32(value ? 1 : 0);
  } else if (typeof value === "object") {
    const list = Array.isArray(value);
    w.varint32(list ? 50 : 42);
    const hole = w.begin();
    (list ? writeList : writeStruct)(w, value);
    w.end(hole);
  } else fail(`Expected JSON, received ${stringify(value)}`);
};

// Each called the way a compiled message is: the caller frames the length, and
// reading nothing is the default instance. Reached only from an operation, so
// `toProtoOrThrow` never carries them.
const codecs: Partial<Record<WellKnownType, [read: (r: Reader, d: number, prev?: unknown) => unknown, write: (w: Writer, value: unknown) => void]>> = {
  "google.protobuf.Timestamp": [readDate, writeDate],
  "google.protobuf.Value": [readValue, writeValue],
  "google.protobuf.Struct": [readStruct, writeStruct],
  "google.protobuf.ListValue": [readList, writeList],
};

// A codec's reader over every payload a field was given, as a compiled
// message's reads them (`readParts`): each continues from what the one before
// read.
const ranged = (read: (r: Reader, d: number, prev?: unknown) => unknown) => (r: Reader, d: number, x?: number): unknown => {
  let value = read(r, d);
  for (; x! >= 0; x = r.x[x! + 2]) {
    r.pos = r.x[x!]!;
    r.limit = r.x[x! + 1]!;
    value = read(r, d, value);
  }
  return value;
};

const secondsNanos: Record<string, StoredField> = {
  seconds: { number: 1, type: "int64", packed: true, key: "string", numberedAs: bigint },
  nanos: { number: 2, type: "int32", packed: true, key: "string", numberedAs: int },
};

// A well-known type as the message it is on the wire, one per type and value
// schema. `{ seconds, nanos }` and Empty compile as the message they spell; a
// wrapper and a FieldMask as the one field they hold; the rest take a codec,
// whose raw side is the value's schema without the checks the caller put on
// it, which run when the value is parsed into that schema after.
const wellKnownMessage = (type: WellKnownType, shape: Internal, ctx: Ctx, where: string): Message | undefined => {
  if (shape.type === objectTag && typeof shape.additionalItems !== objectTag) return compileMessage(shape, ctx, type);
  const memo = type + shape.seq;
  let msg = ctx.messages.get(memo);
  if (msg !== U) return msg;
  const scalar = wrappers[type];
  if (scalar !== U || type === "google.protobuf.FieldMask") {
    const raw = scalar !== U ? scalarSchema(scalar) : arrayFactory(string);
    const field: Field = {
      number: 1,
      type: scalar ?? "string",
      packed: true,
      key: scalar !== U ? "value" : "paths",
      repeated: scalar === U,
      optional: false,
      wire: wireType(scalar ?? "string"),
      fromWire: raw,
      held: raw,
      path: [],
    };
    msg = { fields: [field], object: shape, raw, wellKnown: type, unwrap: true };
  } else {
    msg = { fields: [], object: shape, raw: unchecked(shape), wellKnown: type, codec: true };
  }
  ctx.messages.set(memo, msg);
  return msg;
};

// `lists` are the message's repeated fields in field order, as `decodeFnSource`
// passes them, and the length a list has reached is the index of the element
// being read: whole elements go in before a failure.
//
// A number the message does not declare adds nothing: an unknown field under
// `S.strict` and a field number of zero name the number themselves, and there
// is no property to put in a path.
const wireFrame = (failure: unknown, msg: Message, tag: number, key?: unknown, ...lists: unknown[][]): unknown => {
  const number = tag >>> 3;
  const field = tag < 0 ? U : msg.fields.find((f) => f.number === number);
  if (field === U) return failure;
  const at: (string | number)[] = [...field.path];
  // A list names its element only under a tag it reads: under any other the
  // failure is in bytes skipped or refused, which are no element of it.
  if (field.repeated) {
    if (tag === number * 8 + field.wire || (packable[field.type] && tag === number * 8 + 2)) {
      at.push(lists[msg.fields.filter((f) => f.repeated).indexOf(field)]!.length);
    }
  }
  // An entry whose value failed before its key was read has nowhere in the
  // value to be: it fails at the map.
  else if (field.map !== U) {
    if (key === U && isFailure(failure)) failure.path = [];
    else if (key !== U) at.push("" + key);
  }
  return framed(failure, at);
};

// A failure under a field: the codec's own records the path in place, one
// the core raised in a message's value is copied with it in front.
const framed = (failure: unknown, at: (string | number)[]): unknown =>
  isFailure(failure) ? ((failure.path = [...at, ...failure.path]), failure)
  : failure && (failure as Failure).s === sury ? B_prefixPath(failure as Failure, at)
  : failure;

// A number the message declares only gets here under a wire type its field
// can't hold, which is what it says instead.
const unknownField = (msg: Message, tag: number): never => {
  const number = tag >>> 3;
  const wire = tag & 7;
  const field = msg.fields.find((f) => f.number === number);
  return fail(
    field === U ? `Unrecognized protobuf field ${number}`
    : `Expected wire type ${field.map !== U ? 2 : field.repeated && packable[field.type] ? `${field.wire} or 2` : field.wire}, received ${wire}`,
  );
};

// A packed writer's throw carries its element `index`: its field's frame has
// no loop counter of its own to read.
const fieldFailure = (value: unknown, index?: number, kind?: number): never => {
  const record = failure();
  record.value = value;
  record.index = index;
  record.kind = kind;
  throw record;
};

const oneofConflict = (): never => fieldFailure(U, U, 2);

// The bounds' own type is the one the value must have: a 64-bit field holds a
// bigint, the rest an integer number.
const checked = <T>(value: T, min: number | bigint, max: number | bigint, index?: number): T => {
  if (typeof value !== typeof min || (typeof value === "number" && !Number.isInteger(value)) || (value as number) < min || (value as number) > max) {
    fieldFailure(value, index);
  }
  return value;
};

// A 64-bit field held as a string (`jstype = JS_STRING`) is written from its
// decimal digits.
const digits = (value: unknown, min: bigint, max: bigint): bigint => {
  const n = typeof value === "string" && /^-?\d+$/.test(value) ? BigInt(value) : U;
  return n !== U && n >= min && n <= max ? n : fieldFailure(value);
};

// The key a failure names is the property name, not what `+` or `BigInt` made
// of it.
const mapKey = (key: string, min: number | bigint, max: number | bigint): number | bigint => {
  try {
    return checked(typeof min === "number" ? +key : BigInt(key), min, max);
  } catch {
    return fieldFailure(key, U, 1);
  }
};

// The one frame of a message encoder: `f` is the index of the field it was
// writing, `j` its loop counter, `a` a map's keys. A failure its own write threw
// gets its reason here; one a nested encoder already settled only gets the
// field and element in front. `encodeBody` sets `f` before any write that can
// throw.
const encodeFrame = (x: unknown, msg: Message, f: number, j: number, a?: string[]): unknown => {
  const field = msg.fields[f]!;
  let item: string | number | undefined = field.map !== U ? a![j - 1] : field.repeated ? j - 1 : U;
  if (isFailure(x) && x.reason === U) {
    if (x.index !== U) item = x.index;
    x = failure(
      x.kind === 2 ? `Protobuf oneof "${field.oneof}" has more than one member set`
      : `Expected ${x.kind ? `${field.map} key` : field.type}, received ${stringify(x.value)}`,
    );
  }
  return framed(x, item === U ? field.path : [...field.path, item]);
};

const writeTag = (tag: number): string =>
  tag < 128 ? `w.pos<w.buf.length?w.buf[w.pos++]=${tag}:w.varint32(${tag})` : `w.varint32(${tag})`;

const writeVarint32 = (expr: string): string =>
  `${expr}<128&&w.pos<w.buf.length?w.buf[w.pos++]=${expr}:w.varint32(${expr})`;

const ownerOf = (told: Told | undefined, idx: number): string => {
  const tag = told?.tags[idx];
  return tag !== U ? `${/^\d+$/.test(tag) ? `[${tag}]` : tag}: ${literalSource(told!.literals[idx])}` : "another member";
};

// The member set, written as the Any holding it: its type URL, then its
// message, left out when empty as protobuf-es leaves it. A type another member
// names belongs to that member, which a decode would read it back as.
const anyEncodeBody = (msg: Message, fns: Map<Message, string>, read: Read): string => {
  const { fields, listed, other, told } = msg.any!;
  let body = "";
  fields.forEach((field, idx) => {
    const write = fns.get(field.message!)!;
    body += `f=${idx + 2};v=${read(field).expr};if(v!==void 0){${
      field !== other
        ? `${writeTag(10)};w.string(${JSON.stringify(`type.googleapis.com/${field.name}`)});q=w.pos;${writeTag(18)};g=w.begin();${write}(w,v);w.end(g);if(w.pos===q+2)w.pos=q`
        : `${listed.length ? `g=v.typeUrl;switch(g.slice(g.lastIndexOf("/")+1)){${listed.map((owned, idx) => `case ${JSON.stringify(owned.name)}:fail(${JSON.stringify(`Protobuf Any holds ${owned.name}, which ${ownerOf(told, idx)} is for`)});`).join("")}}` : ""}${write}(w,v)`
    }}`;
  });
  return `var q;${body}`;
};

const encodeHelpers = { check: checked, digits, key: mapKey, oneof: oneofConflict };
type Helper = keyof typeof encodeHelpers;
type Encoding = (helper: Helper) => string;

const bounds = (type: ProtobufType): [string, string] => {
  const unsigned = type[0] === "u" || type[0] === "f";
  return type.includes("64")
    ? unsigned ? ["0n", "18446744073709551615n"] : ["-9223372036854775808n", "9223372036854775807n"]
    : unsigned ? ["0", "4294967295"] : ["-2147483648", "2147483647"];
};

// `known`: 2 a map key `mapKey` has already held to the type's range, 1 a
// value `fieldLive` has coerced, 3 a 64-bit value held as its digits.
const writeCall = (type: ProtobufType, v: string, enc: Encoding, known = 0): string => {
  if (type === "bool") return `s=${v}?1:0;w.pos<w.buf.length?w.buf[w.pos++]=s:w.varint32(s)`;
  if (type === "float") return `w.float32(${v})`;
  if (type === "double") return `w.float64(${v})`;
  if (type === "string") return `w.string(${v})`;
  if (type === "bytes") return `w.bytes(${v})`;
  const [min, max] = bounds(type);
  const check = (expr: string) => (known === 2 ? expr : `${enc(known === 3 ? "digits" : "check")}(${expr},${min},${max})`);
  if (type === "sint64") return `s=${check(v)};w.varint64((s<<1n)^(s>>63n))`;
  if (type === "int64" || type === "uint64") return `w.varint64(${check(v)})`;
  if (type.includes("64")) return `w.bits64(${check(v)})`;
  if (type.includes("fixed")) return `w.bits32(${check(v)})`;
  // Coerced as `fieldLive` coerces a field: a bigint would otherwise reach the
  // byte write. A decode doesn't validate, so a number schema holds no promise.
  const test = known === 2 ? `s=${v};`
    : `s=${known ? v : `Number(${v})`};if(s<${min}||s>${max})${check("s")};`;
  if (type === "uint32") return test + writeVarint32("s");
  if (type === "sint32") return `${test}s=((s<<1)^(s>>31))>>>0;${writeVarint32("s")}`;
  return `${test}s>=0?${writeVarint32("s")}:w.int32(s)`;
};

const fixedKind = (type: ProtobufType): number | undefined =>
  type === "double" ? 0
  : type === "float" ? 1
  : type === "fixed32" ? 2
  : type === "sfixed32" ? 3
  : type === "fixed64" ? 4
  : type === "sfixed64" ? 5
  : U;

const varintKind = (type: ProtobufType): number | undefined =>
  type === "uint32" ? 0
  : type === "int32" || type === "enum" ? 1
  : type === "sint32" ? 2
  : type === "bool" ? 3
  : U;

// A one-byte varint is read inline.
const readCall = (type: ProtobufType): string => {
  const varint32 = "(r.pos<r.limit&&(q=r.buf[r.pos])<128?(r.pos++,q):r.varint32())";
  if (type === "bool") return "r.bool()";
  if (type === "uint32") return varint32;
  if (type === "int32" || type === "enum") return `${varint32}|0`;
  if (type === "sint32") return `((q=${varint32})>>>1^-(q&1))|0`;
  if (type === "int64") return "r.int64()";
  if (type === "uint64") return "r.varint64()";
  if (type === "sint64") return "(q=r.varint64(),(q>>1n)^-(q&1n))";
  if (type === "double") return "r.f64()";
  if (type === "float") return "r.f32()";
  if (type === "fixed64") return "r.u64()";
  if (type === "sfixed64") return "r.i64()";
  if (type === "fixed32") return "r.u32()";
  if (type === "sfixed32") return "r.u32()|0";
  if (type === "string") return "r.string()";
  return "r.bytes()";
};

const scalarDefault = (type: ProtobufType): string =>
  type === "string" ? '""'
  : type === "bytes" ? "new Uint8Array"
  : type === "bool" ? "!1"
  : type.includes("64") ? "0n"
  : "0";

const emitDefault = (field: Field): string => {
  if (field.repeated) return "[]";
  if (field.map !== U) return "{}";
  if (field.optional || field.type === "message") return "void 0";
  return scalarDefault(field.type);
};

// A map key travels as a string property name; these convert it to and
// from the key type on the wire.
const keyToWire = (type: ProtobufType, enc: Encoding): string =>
  type === "string" ? ""
  : type === "bool" ? 'k=k==="true";'
  : `k=${enc("key")}(k,${bounds(type).join(",")});`;

// `numeric`: the value is known to be a number already (a validated field
// val), so the write skips the coercion a nested encoder's untyped read needs.
// `Number` rather than `+`, which throws on a bigint: that coerces as a string
// does, and a value out of range still fails at the field.
const fieldLive = (field: Field, numeric: boolean): string =>
  field.optional ? "v!=null"
  : field.text ? "v&&v!=0"
  : field.type === "bytes" ? "v.length"
  : field.type === "float" || field.type === "double" ? "v||v!==v||Object.is(v,-0)"
  : field.type === "string" || field.type === "bool" || field.type.includes("64") || numeric ? "v"
  : "(v=Number(v))";

type Read = (field: Field) => { expr: string; numeric: boolean };

// `f=` before a field's writes is what the message's frame (`encodeFrame`)
// reads to name the field a failure hit.
const encodeBody = (msg: Message, fns: Map<Message, string>, read: Read, enc: Encoding): string => {
  const body: string[] = [];
  const writer = (field: Field): string => fns.get(field.message!)!;
  // proto3 lets at most one member of a oneof be set, and decode enforces it
  // by clearing the siblings. Encode is handed a value the schema cannot
  // constrain - every member is an independent optional property - so it
  // checks here rather than writing two members of one oneof, which is a
  // message no reader can read back: the second member wins and the first is
  // gone. `o` carries one bit per oneof; the first member of a group has
  // nothing to collide with and the last has no later member to tell.
  const first = new Map<string, Field>();
  const last = new Map<string, Field>();
  for (const field of msg.fields) {
    if (field.oneof === U || field.name !== U) continue;
    if (!first.has(field.oneof)) first.set(field.oneof, field);
    last.set(field.oneof, field);
  }
  const bits = new Map<string, number>();
  first.forEach((field, name) => {
    if (field !== last.get(name)) bits.set(name, 1 << bits.size);
  });
  const guard = (field: Field): string => {
    const name = field.oneof;
    const bit = name === U ? U : bits.get(name);
    if (bit === U) return "";
    // One group needs no mask: no other bit can be set.
    const test = bits.size > 1 ? `o&${bit}` : "o";
    return (first.get(name!) === field ? "" : `${test}&&${enc("oneof")}();`) +
      (last.get(name!) === field ? "" : `o|=${bit};`);
  };
  for (let idx = 0; idx < msg.fields.length; idx++) {
    const field = msg.fields[idx]!;
    const tag = field.number * 8 + field.wire;
    const { expr: src, numeric } = read(field);
    const oneof = guard(field);
    const at = `f=${idx};`;
    if (field.map !== U) {
      const keyType = field.map;
      const entryTag = writeTag(field.number * 8 + 2);
      const keyPart = `${keyToWire(keyType, enc)}${writeTag(8 + wireType(keyType))};${writeCall(keyType, "k", enc, 2)}`;
      const valuePart = field.type === "message"
        ? `${writeTag(16 + 2)};g=w.begin();${writer(field)}(w,c);w.end(g)`
        : `${writeTag(16 + field.wire)};${writeCall(field.type, "c", enc, field.text ? 3 : 0)}`;
      body.push(`${at}v=${src};a=Object.keys(v);n=a.length;j=0;while(j<n){k=a[j++];c=v[k];${entryTag};h=w.begin();${keyPart};${valuePart};w.end(h)}`);
    } else if (field.repeated) {
      let loop: string;
      if (packable[field.type] && field.packed) {
        const kind = varintKind(field.type);
        const fixed = fixedKind(field.type);
        const packed = kind !== U ? `w.varints(v,${kind});`
          : fixed !== U ? `w.fixeds(v,${fixed});`
          : `j=0;while(j<n){${writeCall(field.type, "v[j++]", enc, field.text ? 3 : 0)}}`;
        loop = `${writeTag(field.number * 8 + 2)};h=w.begin();${packed}w.end(h)`;
      } else if (field.type === "message") {
        loop = `j=0;while(j<n){${writeTag(tag)};h=w.begin();${writer(field)}(w,v[j++]);w.end(h)}`;
      } else {
        loop = `j=0;while(j<n){${writeTag(tag)};${writeCall(field.type, "v[j++]", enc, field.text ? 3 : 0)}}`;
      }
      body.push(`${at}v=${src};n=v.length;if(n){${loop}}`);
    } else if (field.type === "message") {
      // `null` is a value to a well-known type: JSON's own.
      body.push(`${at}v=${src};if(${field.message!.codec !== U ? "v!==void 0" : "v!=null"}){${oneof}${writeTag(tag)};h=w.begin();${writer(field)}(w,v);w.end(h)}`);
    } else {
      body.push(`${at}v=${src};if(${fieldLive(field, numeric)}){${oneof}${writeTag(tag)};${writeCall(field.type, "v", enc, field.text ? 3 : numeric ? 0 : 1)}}`);
    }
  }
  return (bits.size ? "o=0;" : "") + body.join(";");
};

const nameMessages = (message: Message, fns: Map<Message, string>): void => {
  if (fns.has(message)) return;
  fns.set(message, `m${fns.size}`);
  for (let idx = 0; idx < message.fields.length; idx++) {
    const field = message.fields[idx]!;
    if (field.message) nameMessages(field.message, fns);
  }
};

// Message codecs are built once per operation with `Function` and embedded
// as values, so the operation body calls a top-level function instead of
// allocating a closure per call. What the core compiles into them shares one
// set of embeds, `e`.
const compileEncoders = (root: Message, fns: Map<Message, string>, flag: number): Record<string, Function> => {
  let src = "";
  const messages: Message[] = [];
  const g = B_operationArg(unknown, unknown, flag).g;
  const params: string[] = [...Object.keys(encodeHelpers), "at", "M", "fail", "e"];
  const args: unknown[] = [...Object.values(encodeHelpers), encodeFrame, messages, fail, g.e];
  const names: string[] = [];
  // The root's fields are read from the vals the operation already has, so its
  // body is inlined there rather than called; it needs a function of its own
  // only where a field under it refers back to it.
  fns.forEach((name, msg) => {
    if (msg === root && root.recursive === U) return;
    names.push(name);
    if (msg.codec !== U) {
      params.push(name);
      args.push(codecs[msg.wellKnown!]![1]);
    } else {
      let head = "";
      let read: Read = () => ({ expr: "value", numeric: false });
      if (!msg.unwrap) {
        const value = B_operationArg(msg.object, msg.object, flag);
        value.g = g;
        value.i = "value";
        const [code, reads] = writesOf(value, msg);
        head = code;
        read = (field) => ({ expr: reads.get(field)!, numeric: false });
      }
      const body = msg.any !== U ? anyEncodeBody(msg, fns, read) : encodeBody(msg, fns, read, (helper) => helper);
      src += `function ${name}(w,value){${head}var v,j,n,s,h,a,k,g,c,o${msg.fields.length ? `,f;try{${body}}catch(x){throw at(x,M[${messages.push(msg) - 1}],f,j,a)}` : `;${body}`}}`;
    }
  });
  return new Function(...params, `${src}return {${names.join(",")}}`)(...args);
};

// A message's read loop and the reads after it. `local` names what outlives
// the loop by kind (`f` a field's value, `c` the number of the oneof member
// set) and the index of the field, or of a oneof's first member; `fn` names the
// function that reads a message.
//
// A message field seen again is the parse of every payload it was given, back
// to back, which is what merging it is: the first payload is noted as `a` and
// `b`, the rest in the reader's `x` from the index in `x`, and the message is
// read once after the loop, its reader continuing from one payload into the
// next. A message's own reader takes that index as `x`.
const readParts = (msg: Message, local: (kind: string, idx: number) => string, fn: (msg: Message) => string) => {
  const fields = msg.fields;
  const locals: [name: string, init: string][] = [];
  const temps: string[] = [];
  const cases: string[] = [];
  const sets = new Map<string, string>();
  let after = "";
  const zero = 'fail("Invalid protobuf field number 0")';
  const note = (a: string, b: string, x: string): string => `if(${a}<0){${a}=r.pos;${b}=r.limit}else ${x}=r.more(${x});r.pos=r.limit;`;
  // An absent message reads the empty range where the loop ended: its default.
  const read = (a: string, b: string, x: string, value: string, message: Message): string =>
    `g=r.limit;${a}<0?r.pos=g:(r.pos=${a},r.limit=${b});${value}=${fn(message)}(r,d+1,${x});r.pos=r.limit=g`;
  const range = (idx: number): [string, string, string] => {
    temps.push(`a${idx}=-1`, `b${idx}=0`, `x${idx}`);
    return [`a${idx}`, `b${idx}`, `x${idx}`];
  };
  for (let idx = 0; idx < fields.length; idx++) {
    const field = fields[idx]!;
    // The loop switches on the whole tag, so a known number arriving with a
    // wire type its field does not take matches no case and is skipped, as an
    // unknown field is.
    const tag = field.number * 8 + field.wire;
    const value = local("f", idx);
    if (field.name !== U) {
      let set = sets.get(field.key);
      if (set === U) {
        sets.set(field.key, (set = local("c", idx)));
        locals.push([set, "0"]);
      }
      locals.push([value, "void 0"]);
      if (field.type === "message") {
        const [a, b, x] = range(idx);
        // A member seen again merges only while it still is the one set.
        cases.push(`case ${tag}:p=r.sub();if(${set}!==${field.number}){${set}=${field.number};${a}=-1;${x}=void 0}${note(a, b, x)}r.limit=p;continue;`);
        after += `if(${set}===${field.number}){t=${tag};${read(a, b, x, value, field.message!)}}`;
      } else cases.push(`case ${tag}:${set}=${field.number};${value}=${readCall(field.type)};continue;`);
      continue;
    }
    locals.push([value, emitDefault(field)]);
    let clear = "";
    if (field.oneof !== U) {
      for (let other = 0; other < fields.length; other++) {
        const member = fields[other]!;
        if (other === idx || member.oneof !== field.oneof) continue;
        clear += member.type === "message" ? `a${other}=-1;x${other}=void 0;` : `${local("f", other)}=void 0;`;
      }
    }
    if (field.map !== U) {
      const keyType = field.map;
      // The entry reads its tags into `y`, so a failure inside it is still
      // framed by the map field's tag in `t`. `k` is unset until the key is
      // read, which is what tells the frame whether to name the entry: the key
      // may follow the value.
      const entryLoop = (body: string) =>
        `while(r.pos<r.limit){y=r.buf[r.pos];if(y<128)r.pos++;else y=r.tag();if(y<8)${zero};${body}else skip(r,y&7,y>>>3,0)}`;
      const keyDefault = `if(k===void 0)k=${scalarDefault(keyType)};`;
      const readKey = `if(y===${8 + wireType(keyType)})k=${readCall(keyType)};`;
      const store = keyType === "string"
        ? `k==="__proto__"?Object.defineProperty(${value},k,{value:c,enumerable:!0,writable:!0,configurable:!0}):${value}[k]=c`
        : `${value}[k]=c`;
      let entry: string;
      if (field.type === "message") {
        // A repeated key replaces the earlier entry whole, as C++ does, while a
        // value repeated inside one entry merges.
        if (!temps.includes("ma")) temps.push("ma", "mb", "mx");
        entry = `ma=-1;mx=void 0;${entryLoop(`${readKey}else if(y===18){g=r.sub();${note("ma", "mb", "mx")}r.limit=g}`)}${keyDefault}${read("ma", "mb", "mx", "c", field.message!)}`;
      } else {
        entry = `c=${scalarDefault(field.type)};${entryLoop(`${readKey}else if(y===${16 + field.wire})c=${readCall(field.type)};`)}${keyDefault}`;
      }
      // A map field is length-delimited whatever its value's wire type.
      cases.push(`case ${field.number * 8 + 2}:k=void 0;p=r.sub();${entry};r.limit=p;${store};continue;`);
    } else if (field.type === "message" && field.repeated) {
      cases.push(`case ${tag}:p=r.sub();${value}.push(${fn(field.message!)}(r,d+1));r.limit=p;continue;`);
    } else if (field.type === "message") {
      const [a, b, x] = range(idx);
      cases.push(`case ${tag}:${clear}p=r.sub();${note(a, b, x)}r.limit=p;continue;`);
      // A required message absent on the wire is its default instance, so
      // the schema's type holds; `S.optional` is how presence is asked for.
      after += field.optional ? `if(${a}>=0){t=${tag};${read(a, b, x, value, field.message!)}}` : `t=${tag};${read(a, b, x, value, field.message!)};`;
    } else if (field.repeated && packable[field.type]) {
      const kind = varintKind(field.type);
      const fixed = fixedKind(field.type);
      const packedLoop = kind !== U ? `r.${["u32s", "i32s", "s32s", "bools"][kind]}(${value})`
        : fixed !== U ? `r.fixeds(${value},${fixed})`
        : `while(r.pos<r.limit)${value}.push(${readCall(field.type)})`;
      cases.push(`case ${field.number * 8 + 2}:p=r.sub();${packedLoop};r.limit=p;continue;case ${tag}:${value}.push(${readCall(field.type)});continue;`);
    } else if (field.repeated) {
      cases.push(`case ${tag}:${value}.push(${readCall(field.type)});continue;`);
    } else {
      cases.push(`case ${tag}:${clear}${value}=${readCall(field.type)};continue;`);
    }
  }
  const lists = fields.flatMap((field, idx) => (field.repeated ? [`,${local("f", idx)}`] : [])).join("");
  const frame = lists || fields.some((field) => field.map !== U) ? `,k${lists}` : "";
  const loop = (miss: string, ranges?: boolean) => tagLoop(cases.join(""), miss, ranges) + after;
  return { locals, temps, frame, loop };
};

// `t=-1` before a tag of several bytes is read: one that fails part way is
// no field's, and must not frame the error with the tag before it.
const tagLoop = (cases: string, miss: string, ranges?: boolean): string => {
  const body = `while(r.pos<r.limit){t=r.buf[r.pos];if(t<128)r.pos++;else{t=-1;t=r.tag()}switch(t){${cases}}if(t<8)fail("Invalid protobuf field number 0");${miss}}`;
  return ranges ? `for(;;){${body}if(x===void 0||x<0)break;r.pos=r.x[x];r.limit=r.x[x+1];x=r.x[x+2]}` : body;
};

// An Any's reader: its type URL and the range of its value, then the member
// the type names, read from that range - or from the whole Any again, for the
// catch-all. `o` and `l` are where the Any is, `z` the copies after it, which
// the loop's own walk through them uses up.
const anyReadParts = (msg: Message, fns: Map<Message, string>): ReturnType<typeof readParts> => {
  const { fields, listed, other } = msg.any!;
  const expected = listed.map((field) => field.name).join(" or ");
  const read = (field: Field): string => {
    const idx = msg.fields.indexOf(field);
    return `t=${field.number * 8 + 2};c2=${field.number};${
      field === other ? `r.pos=o;r.limit=l;f${idx}=${fns.get(field.message!)}(r,d+1,z)` : `a<0?r.pos=r.limit:(r.pos=a,r.limit=b);f${idx}=${fns.get(field.message!)}(r,d+1)`
    }`;
  };
  const pick =
    listed.map((field) => `if(n===${JSON.stringify(field.name)}){${read(field)}}else `).join("") +
    (other !== U ? `{${read(other)}}` : `fail(n?${JSON.stringify("Protobuf Any holds ")}+n+${JSON.stringify(`, expected ${expected}`)}:${JSON.stringify(`Protobuf Any has no type URL, expected ${expected}`)})`);
  const cases = `case 10:n=r.string();continue;case 18:p=r.sub();a=r.pos;b=r.limit;r.pos=r.limit;r.limit=p;continue;`;
  return {
    locals: [["c2", "0"], ...fields.map((_, idx) => [`f${idx + 2}`, "void 0"] as [string, string])],
    temps: ["o=r.pos", "l=r.limit", ...(other !== U ? ["z=x"] : []), 'n=""', "a=-1", "b=0"],
    frame: "",
    // The type URL read, a failure picking the member is no field's.
    loop: (miss, ranges) => `${tagLoop(cases, miss, ranges)}t=-1;n=n.slice(n.lastIndexOf("/")+1);${pick};r.pos=r.limit=l`,
  };
};

// A message whose value converts asynchronously hands back a promise, whose
// failure is framed by the field holding it as a sync one is by the reader's
// frame. A list or a map of them is the promise of all of them.
const awaited = (value: unknown, path: Path): unknown =>
  value instanceof Promise ? value.catch((x) => rethrowWithPath(x, path)) : value;

const awaitedList = (list: unknown[], path: Path): Promise<unknown[]> =>
  Promise.all(list.map((item, idx) => awaited(item, [...path, idx])));

const awaitedMap = (map: Record<string, unknown>, path: Path): Promise<Record<string, unknown>> => {
  const keys = Object.keys(map);
  return Promise.all(keys.map((key) => awaited(map[key], [...path, key]))).then((values) => {
    keys.forEach((key, idx) => (map[key] = values[idx]));
    return map;
  });
};

const slotOf = (at: Val, msg: Message, field: Field, local: (kind: string, idx: number) => string, held: Internal = field.held): Val => {
  const { fromWire: raw, fromWireIsHeld: done, path, repeated, map, message, name, key } = field;
  const slot = B_slot(at, local("f", msg.fields.indexOf(field)), raw, held, path);
  if (done && message?.async) {
    if (name !== U && msg.any === U) B_invalidOperation(at, `S.protobuf: oneof "${key}" holds a message that converts asynchronously, which a oneof member can't`);
    if (name !== U && msg.object.type === anyOfTag) B_invalidOperation(at, `S.protobuf: an Any holds a message that converts asynchronously, which a member of several can't`);
    slot.i = `${B_embed(at, repeated ? awaitedList : map !== U ? awaitedMap : awaited)}(${slot.i},${JSON.stringify(path)})`;
    slot.v = _notVar;
    slot.f = 1;
  }
  return slot;
};

const locationOf = (msg: Message, field: Field): string[] => (msg.any !== U ? field.path : field.path.slice(1));

const armsValue = (at: Val, msg: Message, union: Internal, arms: [member: Internal, field: Field | undefined][], set: string, path: Path, local: (kind: string, idx: number) => string): Val => {
  const g = at.g;
  const out = B_varWithoutAllocation(g);
  const branches: string[] = [];
  let rest = "";
  let async = 0;
  // A oneof with neither an arm for no member set nor S.optional is required:
  // none set reads as `undefined`, which the union then refuses, as any other
  // absent field is its default for the field's schema to check.
  const required = msg.any === U && !arms.some(([, field]) => field === U);
  arms.forEach(([member, field], idx) => {
    let value: Val;
    if (field === U) {
      if (member.type === undefinedTag) return;
      value = B_slot(at, "void 0", unit, schemaShape(unit, () => armDefinition(member, U, U)), path);
      value.io = true;
    } else {
      const placed = schemaShape<Internal>(field.fromWireIsHeld ? getOutputSchema(field.held) : field.held, (leaf) => armDefinition(member, locationOf(msg, field)[0], leaf));
      value = slotOf(at, msg, field, local, placed);
      if (field.fromWireIsHeld) {
        value.s = field.held;
        value.io = true;
      }
    }
    value = parse(value);
    // A bare arm is its member's value, which its own read checked; an arm
    // built around one is checked here, as the shape it was built through
    // checks only what it holds.
    if (field === U || locationOf(msg, field)[0] !== U) {
      const arm = B_refine(value, U, U, member);
      arm.path = pathConcat(at.path, path);
      value = B_markOutput(arm);
    }
    async |= value.f & 1;
    const body = `${B_merge(value)}${out}=${value.i}`;
    if (field === U || (!required && rest === "" && idx === arms.length - 1 && !arms.some(([, other]) => other === U))) rest = body;
    else branches.push(`if(${set}===${field.number}){${body}}`);
  });
  if (required) {
    const none = B_slot(at, "void 0", unit, union, path);
    rest = B_fail(none, failInvalidType(none), "void 0");
  }
  const value = B_slot(at, out, union, union, path);
  value.cp = B_let(g, out) + branches.join("else ") + (!branches.length ? `${rest};` : rest === "" ? "" : `else{${rest}}`);
  value.f = async;
  return B_markOutput(value);
};

const valueOf = (base: Val, msg: Message, local: (kind: string, idx: number) => string): Val => {
  const at = B_next(base, "", msg.object, msg.object);
  let output: Val;
  if (msg.any !== U) {
    const { members, fields } = msg.any;
    output = armsValue(at, msg, msg.object, members.map((member, idx) => [member, fields[idx]]), local("c", 2), [], local);
  } else {
    output = B_record(at, (record) => {
      for (const key in msg.object.properties) {
        const oneof = msg.oneofs?.[key];
        if (oneof !== U) {
          const first = msg.fields.findIndex((field) => field.key === key);
          record(key, () => armsValue(at, msg, oneof.union, oneof.members, local("c", first), [key], local));
          continue;
        }
        const field = msg.fields.find((field) => field.key === key)!;
        const { fromWire: raw, held, omit, repeated, map } = field;
        const slot = slotOf(at, msg, field, local);
        const io = raw === held;
        if (io) slot.io = true;
        record(key, () => {
          const out = !io ? parse(slot) : repeated || map !== U ? B_markOutput(slot) : slot;
          out.o = omit;
          return out;
        });
      }
    });
    output = parse(output);
  }
  for (const link of msg.links!) {
    const value = B_refine(output, link, U, link);
    output = parse(B_markOutput(value));
  }
  return output;
};

// Every message's reader but the root's, which reads in the operation, unless
// a field under it refers back to it.
const compileDecoder = (root: Message, fns: Map<Message, string>, flag: number): Record<string, Function> => {
  let src = "";
  // `M` is read only on a failure, so the field lookup a frame needs stays
  // out of the generated code and off the decode path.
  const messages: Message[] = [];
  const g = B_operationArg(unknown, unknown, flag).g;
  const params = ["skip", "at", "unknown", "fail", "M", "e"];
  const args: unknown[] = [skip, wireFrame, unknownField, fail, messages, g.e];
  const names: string[] = [];
  const readers = new Map<Message, string>();
  const local = (kind: string, idx: number) => kind + idx;
  // A message holding one whose value converts asynchronously hands back a
  // promise too, which it learns only once that one is compiled: compiled
  // again until no message's answer changes.
  for (let changed = true; changed; ) {
    changed = false;
    fns.forEach((name, msg) => {
      if ((msg === root && root.recursive === U) || msg.codec !== U || msg.unwrap) return;
      const base = B_operationArg(unknown, unknown, flag);
      base.g = g;
      const value = valueOf(base, msg, local);
      readers.set(msg, `${B_merge(value)}return ${value.i}`);
      if (value.f & 1 && !msg.async) changed = msg.async = true;
    });
  }
  fns.forEach((name, msg) => {
    if (msg === root && root.recursive === U) return;
    if (msg.codec !== U) {
      params.push(name);
      args.push(ranged(codecs[msg.wellKnown!]![0]));
    } else {
      const slot = messages.push(msg) - 1;
      const { locals, temps, frame, loop } = msg.any !== U ? anyReadParts(msg, fns) : readParts(msg, local, (m) => fns.get(m)!);
      const vars = [...locals.map(([local, init]) => `${local}=${init}`), ...temps, "t=-1,p,k,c,g,q,y"];
      const miss = msg.object.additionalItems === "strict" ? `unknown(M[${slot}],t)` : "skip(r,t&7,t>>>3,0)";
      src += `function ${name}(r,d,x){if(d>=100)fail("Protobuf message nesting limit exceeded");var ${vars};try{${loop(miss, true)}}catch(x){throw at(x,M[${slot}],t${frame})}${readers.get(msg) ?? "return f0"}}`;
    }
    names.push(name);
  });
  return new Function(...params, `${src}return {${names.join(",")}}`)(...args);
};

// The declared object, with the field metadata on its properties. A parsed
// val's `s` is the object rebuilt from what each field parsed to, and a
// union field parses to a fresh schema that lost its `pb`, so the expected
// side of the chain is searched first.
// A ref counts: a recursive message speaks its definition, and a chain through
// one puts the ref where the object would otherwise sit.
const isMessageShape = (schema: Internal | undefined): schema is Internal =>
  schema !== U && (schema.type === refTag || (schema.type === objectTag && schema.properties !== U));

const objectSchemaOf = (input: Val, shape: (schema: Internal | undefined) => boolean = isMessageShape): Internal => {
  let current: Val | undefined = input.prev;
  while (current !== U) {
    if (shape(current.e)) return current.e;
    current = current.prev;
  }
  current = input;
  while (current !== U) {
    if (shape(current.s)) return current.s;
    current = current.prev;
  }
  return input.s;
};

const fieldValsOf = (input: Val): Record<string, Val> | undefined => {
  let current: Val | undefined = input;
  while (current !== U) {
    if (current.d !== U) return current.d;
    current = current.prev;
  }
  return U;
};

const protobufDecoder = (input: Val): Val => {
  // Bytes in, bytes out: another `S.protobuf`, or an `unknown` source - which
  // is what `parse` is, and the wire side of a chain is a Uint8Array like any
  // other, so it validates as one instead of planning a message it has no
  // value for.
  if (input.s.encoder === protobufEncoder || tagFlags[input.s.type]! & 1) return instanceDecoder(input);
  const message = compileMessage(objectSchemaOf(input), newCtx(false, true));
  if (message === U) return B_unsupportedDecode(input, input.s, input.e);
  const fns = new Map<Message, string>();
  nameMessages(message, fns);
  const encoders = compileEncoders(message, fns, input.g.o);
  const names = new Map<Message, string>();
  fns.forEach((name, msg) => {
    const fn = encoders[name];
    names.set(msg, fn === U ? name : B_embedPure(input, fn));
  });
  const validated = fieldValsOf(input);
  const value = B_scope(input);
  value.s = message.object;
  value.d = validated;
  const [head, reads] = writesOf(value, message);
  const readRoot: Read = (field) => {
    const fv = validated?.[field.path[0]!];
    return { expr: reads.get(field)!, numeric: field.path.length === 1 && fv !== U && fv.s.type === numberTag && fv.s.format !== U };
  };
  const embeds: Partial<Record<Helper, string>> = {};
  const body = encodeBody(message, names, readRoot, (helper) => (embeds[helper] ??= B_embed(input, encodeHelpers[helper])));
  const fails = message.fields.length > 0;
  const outVar = B_varWithoutAllocation(input.g);
  const output = B_next(input, outVar, input.e, input.e);
  output.v = _var;
  // Braced: the borrowed writer keeps the short name that ships on every field
  // write, and a second `S.protobuf` in the same operation - two of them in one
  // object - declares its own rather than colliding with this one.
  // The root's frame is the operation's catch: its locals are declared outside
  // the `try` for it to read.
  const frame = fails ? `x=${B_embedPure(input, encodeFrame)}(x,${B_embedPure(input, message)},f,j,a);` : "";
  output.cp = `${head}${B_let(input.g, outVar)}{let w,v,j,n,s,h,a,k,g,c,o${fails ? ",f" : ""};${guarded(input, output, input.e, `w=${B_embedPure(input, scratchWriter)}.acquire();${body};${outVar}=w.finish()`, `w&&(w.busy=false);${frame}`)}}`;
  output.io = true;
  return output;
};

// A wire or value failure has no `cause`: nothing the user wrote threw. Anything else - a getter's throw - is reported the way
// B_conversion reports a coder's. In a union either one is what hands the value
// to the next case.
const guarded = (input: Val, output: Val, target: Internal, code: string, release: string): string => {
  const foreign = B_conversionFail(input, target);
  let site: object | undefined;
  const failure = B_fail(
    output,
    (x: unknown, path?: Path) => {
      if (!isFailure(x)) return foreign(x, path);
      Object.setPrototypeOf(x, (site ??= conversionSite(input.s, target)));
      x.path = pathConcat(path ?? compilePath(input.path), x.path);
      return x as unknown as ErrorDetails;
    },
    "x",
  );
  return `try{${code}}catch(x){${release}${failure}}`;
};

const armDefinition = (member: Internal, key: string | undefined, leaf: unknown): unknown => {
  const entries = entriesOf(member);
  if (leaf !== U && key === U) return leaf;
  if (entries === U) return literalOf(member);
  const tuple = member.type === arrayTag;
  const definition: Record<string, unknown> = tuple ? ([] as never) : Object.create(null);
  for (const entry of Object.keys(entries)) {
    if (entry === key) definition[entry] = leaf;
    // An optional property holding nothing is left out, as a decode leaves out
    // an absent field.
    else if (tuple || !(getOutputSchema(entries[entry]!).type === anyOfTag && holdsNothing(entries[entry]!))) definition[entry] = literalOf(entries[entry]);
  }
  return definition;
};

const sinkOf = (location: string[], local: string): Internal =>
  baseSchema(unknownTag, false, (input) => {
    const value = B_scope(valAt(input, location));
    const out = B_refine(input);
    out.cp = `${B_merge(value)}${local}=${value.i};`;
    return out;
  });

const unchecked = (schema: Internal): Internal => {
  const mut = copySchema(schema);
  delete mut.refiner;
  delete mut.inputRefiner;
  return mut;
};

const opaque = /* @__PURE__ */ dictFactory(unknown);

// The arm a union holds is the union's own to tell, each arm continuing into
// the local of its member, so the writer writes every member as an optional
// field of which only the one set holds a value.
const writesOf = (value: Val, msg: Message): [code: string, reads: Map<Field, string>] => {
  const g = value.g;
  const reads = new Map<Field, string>();
  let code = "";
  const dispatch = (from: Val, union: Internal, arms: (Field | undefined)[]): void => {
    if (union.type !== anyOfTag) {
      const at = locationOf(msg, arms[0]!)[0];
      const read = at === U ? B_scope(from) : valGet(from, at);
      code += B_merge(read);
      reads.set(arms[0]!, read.i);
      return;
    }
    // The value was decoded already: each arm is what it holds as output, so
    // the union tells it by its literals and converts and checks nothing again.
    const target = copySchema(union);
    target.anyOf = union.anyOf!.map((variant, idx) => {
      const arm = unchecked(getOutputSchema(variant));
      const field = arms[idx];
      if (field === U) return arm;
      const local = B_varWithoutAllocation(g);
      code += B_let(g, local);
      reads.set(field, local);
      const location = locationOf(msg, field);
      const at = location[0];
      // The writer checks a member's value as it writes it, so an arm hands
      // its message over as it stands: decoded here, a message would be walked
      // whole once for every level it nests at. A bare one is told from its
      // siblings by its type alone (`tellApart`), so one held as anything but
      // an object, a Timestamp's Date, keeps its own.
      const held = field.type !== "message" ? unchecked(getOutputSchema(field.armValue!)) : at === U && typeOf(arm, true) !== objectTag ? arm : copySchema(opaque);
      const mut = at === U ? held : arm;
      if (at !== U) {
        if (mut.items !== U) mut.items = mut.items.map((item, idx) => (`${idx}` === at ? held : item));
        else mut.properties = Object.assign(Object.create(null), mut.properties, { [at]: held });
      }
      mut.to = sinkOf(location, local);
      return mut;
    });
    // One target per arm, which the union's encoder pairs with its own arms.
    target.flags |= 64;
    from.s = union;
    from.e = target;
    from.io = U;
    code += B_merge(parse(from));
  };
  if (msg.any !== U) {
    const { members, fields } = msg.any;
    const union = msg.object;
    dispatch(value, union, union.type === anyOfTag ? union.anyOf!.map((variant) => fields[members.indexOf(variant)]) : [fields[0]]);
  } else {
    for (const key in msg.object.properties) {
      const read = valGet(value, key);
      const oneof = msg.oneofs?.[key];
      if (oneof !== U) dispatch(read, oneof.union, oneof.members.map(([, field]) => field));
      else {
        code += B_merge(read);
        reads.set(msg.fields.find((field) => field.key === key)!, read.i);
      }
    }
  }
  // The reads above declare the vars they hold on `value`.
  return [B_merge(value) + code, reads];
};

const protobufEncoder = (input: Val, target: Internal): Val => {
  // Bytes into `S.protobuf` - a union case converts into its member whole - are
  // those bytes, and the parse loop carries them on down its `.to`. Decoding
  // here would hand that link a decoded value where it reads bytes.
  if (target.flags & 256) return input;
  const message = compileMessage(target, newCtx(true));
  // Another instance (`S.arrayBuffer`, say) takes the bytes as they are.
  if (message === U) return (tagFlags[target.type]! & 8192) ? input : B_unsupportedDecode(input, input.s, target);
  const fns = new Map<Message, string>();
  nameMessages(message, fns);
  const readers = compileDecoder(message, fns, input.g.o);
  const locals: Record<string, string> = Object.create(null);
  const local = (kind: string, idx: number): string => (locals[kind + idx] ??= B_varWithoutAllocation(input.g));
  // The helpers by the names the nested readers call them, so the root's loop
  // is spelled as theirs is.
  let names = `,d=0,fail=${B_embedPure(input, fail)},skip=${B_embedPure(input, skip)}`;
  const used = new Set<Message>();
  const { locals: inits, temps, frame, loop } = readParts(message, local, (msg) => {
    const name = fns.get(msg)!;
    if (!used.has(msg)) {
      used.add(msg);
      names += `,${name}=${B_embedPure(input, readers[name])}`;
    }
    return name;
  });
  const at = B_embedPure(input, message);
  for (const [name, init] of inits) B_hoistDecl(input, name, init);
  const read = B_next(input, "", target, target);
  read.cp = `{let r,${[...temps, "t=-1,p,k,c,g,q,y"]}${names};${guarded(
    input,
    read,
    target,
    `r=${B_embed(input, scratchReader)}.acquire(${input.v()});${loop(message.object.additionalItems === "strict" ? `${B_embedPure(input, unknownField)}(${at},t)` : "skip(r,t&7,t>>>3,0)")}r.busy=false`,
    `r&&(r.busy=false);x=${B_embedPure(input, wireFrame)}(x,${at},t${frame});`,
  )}}`;
  return valueOf(read, message, local);
};

// `key` unset: the member is the message itself.
type AnyArm = [name: string, payload: Internal, key: string | undefined, member: Internal];

const anyArm = (member: Internal, ctx: Ctx): AnyArm | undefined => {
  // A tuple member's `.to` is where a union converts it, not its own type.
  const name = member.type === arrayTag ? U : typeNameOf(member, ctx);
  if (name !== U) return [name, member, U, member];
  const properties = entriesOf(member);
  if (properties === U) return U;
  const values = Object.keys(properties).filter((property) => getOutputSchema(properties[property]!).const === U);
  const key = values.find((property) => typeNameOf(properties[property]!, ctx, true) !== U);
  if (key === U) return U;
  if (values.length > 1) return panic(`S.protobufAny: a member holds two values, "${values[0]}" and "${values[1]}"`);
  return [typeNameOf(properties[key]!, ctx, true)!, properties[key]!, key, member];
};

const literalOf = (schema: Internal | undefined): unknown =>
  schema === U ? U : getOutputSchema(schema).const;

// The members a conversion to `target` unpacks into: `undefined` where one
// holds no message. Refused where a value couldn't come back as the member it
// left: two members holding one message, or several with no literal telling
// them apart, which is all an encode has to go by.
type AnyArms = [arms: AnyArm[], other: AnyArm | undefined, told: Told | undefined];

const anyArms = (target: Internal, ctx: Ctx): AnyArms | undefined => {
  const arms: AnyArm[] = [];
  let other: AnyArm | undefined;
  for (const member of target.type === anyOfTag ? target.anyOf! : [target]) {
    // An absent Any never reaches a conversion.
    if (getOutputSchema(member).type === undefinedTag) continue;
    const arm = anyArm(member, ctx);
    if (arm === U) return U;
    if (arm[0] === "google.protobuf.Any" ? other !== U : arms.some((known) => known[0] === arm[0])) {
      return panic(`S.protobufAny: ${arm[0]} is held by two members`);
    }
    if (arm[0] !== "google.protobuf.Any") arms.push(arm);
    else if (["typeUrl", "value"].every((key) => getOutputSchema(arm[1]).properties?.[key] !== U)) other = arm;
    else return panic(`S.protobufAny: a member keeps other types in google.protobuf.Any, which has to be { typeUrl: string, value: Uint8Array }`);
  }
  const all = other === U ? arms : [...arms, other];
  if (all.length === 0) return U;
  let told: Told | undefined;
  if (all.length > 1) {
    // A message held bare is only ever the lone member: converted on its own
    // the core union can't tell it from a tuple member, though a field can.
    told = all.some((arm) => arm[2] === U) ? U : tellApart(all.map((arm) => arm[3]), all.map((arm) => arm[2]));
    if (told === U) {
      return panic(`S.protobufAny can't tell its members apart. Give each a literal, such as type: "user", beside the message it holds`);
    }
  }
  return [arms, other, told];
};

const anyFailure = (input: Val, target: Internal, reason: (name: string) => string): ((name: string, path?: Path) => ErrorDetails) => {
  const snap = B_pathSnap(input);
  let site: object;
  return (name, path) => {
    const error = Object.setPrototypeOf(failure(reason(name)), (site ??= conversionSite(input.s, target))) as Failure;
    error.path = snap ?? path!;
    return error as unknown as ErrorDetails;
  };
};

// An absent Any field arrives as the empty Any, which only a catch-all takes.
const anyUnpack: Encoder = (input, target) => {
  const ctx = newCtx();
  if (typeNameOf(target, ctx) === "google.protobuf.Any") return input;
  const known = anyArms(target, ctx);
  // No message to unpack into: the Any converts as the object it is.
  if (known === U) return input;
  const [arms, other] = known;
  const g = input.g;
  // Reads the vals the preceding validation left, so the object is built only
  // for a catch-all that keeps it whole.
  const fields = fieldValsOf(input);
  const field = (key: string): string => fields?.[key]?.i ?? inlinedProperty(input.v(), key);
  const name = B_varWithoutAllocation(g);
  const out = B_varWithoutAllocation(g);
  let code = `${B_let(g, name, field("typeUrl"))}${name}=${name}.slice(${name}.lastIndexOf("/")+1);${B_let(g, out)}`;
  // Each arm as its code and the value it lands, a promise where it is async.
  const convert = (arm: AnyArm, read: string, source: Internal): [code: string, value: string, async: boolean] => {
    if (source === protobuf && arm[1].type === refTag) {
      const callIn = B_scope(input);
      if (arm[2] !== U) callIn.path = pathConcat(input.path, [arm[2]]);
      const x = B_varWithoutAllocation(g);
      const call = B_next(callIn, x, arm[1], arm[1]);
      const decode = unpackOf(arm[1]);
      call.cp = `${x}=${B_embed(input, decode)}(${read});`;
      call.prev = U;
      return [`${B_let(g, x)}${B_mergeWithPathPrepend(call, callIn)}`, x, false];
    }
    const valueIn = B_scope(input);
    valueIn.i = read;
    valueIn.v = _notVarBeforeValidation;
    valueIn.io = false;
    valueIn.s = source;
    valueIn.e = arm[1];
    valueIn.d = U;
    if (arm[2] !== U) valueIn.path = pathConcat(input.path, [arm[2]]);
    const valueOut = parse(valueIn);
    return [B_merge(valueOut), valueOut.i, !!(valueOut.f & 1)];
  };
  const converted = arms.map((arm) => convert(arm, field("value"), protobuf));
  const rest = other !== U ? convert(other, input.i, protobufAny) : U;
  // One async arm makes the whole value a promise, the sync ones resolved.
  const async = converted.some((arm) => arm[2]) || !!rest?.[2];
  const land = (arm: AnyArm, [code, value, isAsync]: [string, string, boolean]): string =>
    `${code}${out}=${
      isAsync ? `${value}.then(x=>(${armSource(arm[3], arm[2], "x")}))` : async ? `Promise.resolve(${armSource(arm[3], arm[2], value)})` : armSource(arm[3], arm[2], value)
    };`;
  arms.forEach((arm, idx) => {
    code += `${idx ? "else " : ""}if(${name}===${JSON.stringify(arm[0])}){${land(arm, converted[idx]!)}}`;
  });
  const expected = arms.map((arm) => arm[0]).join(" or ");
  const last = rest !== U ? land(other!, rest) : `${B_fail(input, anyFailure(input, target, (name) => `Protobuf Any ${name ? `holds ${name}` : "has no type URL"}, expected ${expected}`), name)};`;
  code += arms.length ? `else{${last}}` : last;
  const output = B_next(input, out, target, target);
  output.v = _var;
  output.cp = code;
  if (async) B_markAsync(input, output);
  return B_markOutput(output, input);
};

// The union whose conversion leads into this val's target, if one does.
const convertingUnion = (input: Val): Internal | undefined => {
  const stack: (Val | undefined)[] = [input];
  const seen = new Set<Val>();
  while (stack.length) {
    const val = stack.pop();
    if (val === U || seen.has(val)) continue;
    seen.add(val);
    if (val.e.type === anyOfTag && val.e.to === input.e) return val.e;
    stack.push(val.prev, val.b);
  }
  return U;
};

// Packs one member, which is what reaches it: a union converts into it member
// by member. Anything else is the Any's own object shape.
const anyPack: Builder = (input) => {
  // A tuple member is found as the object one is: the expected side first,
  // since what a conversion inside it rebuilt has lost the type names.
  const source = input.s.type === arrayTag ? objectSchemaOf(input, (schema) => schema?.type === arrayTag && schema.items !== U) : objectSchemaOf(input);
  const ctx = newCtx();
  const arm = source.type !== arrayTag && typeNameOf(source, ctx) === "google.protobuf.Any" ? U : anyArm(source, ctx);
  if (arm === U) {
    const output = objectDecoder(input);
    // `objectDecoder` rebuilds the schema from the fields it parsed, without the
    // unpack a `.to` past it needs; the rest of what the rebuild says stays.
    if (input.e.to !== U) {
      output.s = copySchema(output.s);
      output.s.encoder = anyUnpack;
    }
    return output;
  }
  const [name, payload, key] = arm;
  // The val the union's validation left, read once.
  const read = key === U ? input : fieldValsOf(input)?.[key];
  const valueIn = B_scope(read ?? input);
  if (read === U) {
    valueIn.i = inlinedProperty(input.v(), key!, true);
    valueIn.v = _notVarBeforeValidation;
    // The member's field vals would be read as the payload's.
    valueIn.d = U;
    valueIn.path = pathConcat(input.path, [key!]);
  }
  valueIn.io = false;
  valueIn.s = payload;
  valueIn.e = name === "google.protobuf.Any" ? input.e : protobuf;
  const valueOut = parse(valueIn);
  const packed = (value: string): string =>
    name === "google.protobuf.Any" ? value : `{typeUrl:${JSON.stringify(`type.googleapis.com/${name}`)},value:${value}}`;
  const async = valueOut.f & 1;
  const output = B_next(input, async ? `${valueOut.i}.then(x=>(${packed("x")}))` : packed(valueOut.i), input.e, input.e);
  output.cp = B_merge(valueOut);
  if (async) B_markAsync(input, output);
  // A union hands over one member at a time, so its siblings - which the bytes
  // must not come back as - are found in the value's history.
  const union = convertingUnion(input);
  const known = union === U ? U : anyArms(union, ctx);
  if (known !== U && name === "google.protobuf.Any" && known[0].length) {
    const held = B_varWithoutAllocation(input.g);
    const owners = new Map(known[0].map((arm, idx) => [arm[0], ownerOf(known[2], idx)]));
    output.cp +=
      `${B_let(input.g, held, `${valueIn.i}.typeUrl`)}${held}=${held}.slice(${held}.lastIndexOf("/")+1);` +
      B_fail(valueIn, anyFailure(valueIn, valueIn.e, (name) => `Protobuf Any holds ${name}, which ${owners.get(name)} is for`), held, `!(${known[0].map((arm) => `${held}===${JSON.stringify(arm[0])}`).join("||")})`);
  }
  return B_markOutput(output, input);
};

export const protobufAny: Internal = /* @__PURE__ */ initSchema(objectTag, anyPack, (schema) => {
  schema.encoder = anyUnpack;
  schema.properties = {
    typeUrl: protobufField(string, { number: 1, type: "string" }),
    value: protobufField(uint8Array, { number: 2, type: "bytes" }),
  };
  schema.required = ["typeUrl", "value"];
  schema.additionalItems = "strip";
  schema.name = "Any";
  schema.protobufTypeName = "google.protobuf.Any";
});

type Op = (value: unknown) => unknown;

const unpacks = /* @__PURE__ */ new WeakMap<Internal, Op>();

// Compiled on first use, once per message, so creating an operation that
// unpacks one compiles no second operation until a value needs it. Each starts
// its reader's nesting count over, which a payload can't exploit: only the
// standalone S.protobufAny unpacks this way, and a message's own Any field is
// read by its reader, counted.
const unpackOf = (schema: Internal): Op => {
  let unpack = unpacks.get(schema);
  if (unpack === U) {
    let op: Op | undefined;
    unpacks.set(schema, (unpack = (value) => (op ??= getOp(0, 2, protobuf, schema))(value)));
  }
  return unpack;
};

export const protobuf: Internal = /* @__PURE__ */ initSchema(instanceTag, protobufDecoder, (schema) => {
  schema.class = Uint8Array;
  schema.encoder = protobufEncoder;
  schema.flags = schema.flags | 256;
});

type ProtoOptions = { name?: string; package?: string };

// A printed line, or the comment lines a declaration settles once every use
// of it is known.
type Line = string | (() => string[]);

// A field with the user's property and its value schema past optional,
// repeated and map wrapping: what names, comments and enum members print from.
type Use = { field: Field; schema: Internal; shape: Internal; stored: StoredField };

// The meta each use says its type has: a direct use's is what the schema
// carried when it was numbered, a use through `S.optional`, `S.array` or
// `S.record` reaches the declaration and reads it there.
type Meta = { description?: string; deprecated?: boolean };
type TypeDecl = { metas: Meta[] };

type Proto = {
  top: Line[][];
  names: Map<unknown, string>;
  decls: Map<unknown, TypeDecl>;
  // The top-level names: protoc resolves a reference innermost-first, so a
  // nested name may not repeat one, while siblings under different parents
  // may share theirs.
  used: Set<string>;
  // Value names of the top-level enums: enum values live in the scope that
  // holds the enum, beside its fields and other enums' values.
  members: Set<string>;
  ids: Map<object, number>;
  uses: Map<Message, Use[]>;
  // The files a well-known type is imported from.
  imports: Set<string>;
};

const usesOf = (proto: Proto, message: Message): Use[] => {
  let uses = proto.uses.get(message);
  if (uses === U) {
    uses = message.fields.map((field) => {
      const schema = field.armValue || message.object.properties![field.key]!;
      let value = unwrapOptional(schema)[0];
      if (field.repeated || field.map !== U) value = getOutputSchema(value).additionalItems as Internal;
      const shape = field.message !== U ? field.message.object : getOutputSchema(value);
      return { field, schema, shape, stored: fieldMetadata(schema)! };
    });
    proto.uses.set(message, uses);
  }
  return uses;
};

const snakeCase = (name: string): string =>
  name.replace(/([A-Z])([A-Z][a-z])/g, "$1_$2").replace(/([a-z0-9])([A-Z])/g, "$1_$2").toLowerCase();

// A camelCase key prints snake_case, which every generator turns back into the
// same lowerCamel property. Anything else is made an identifier verbatim.
const protoFieldName = (key: string): string =>
  /^[a-z][a-zA-Z0-9]*$/.test(key) ? snakeCase(key) : key.replace(/[^A-Za-z0-9_]/g, "_").replace(/^(?=[0-9])/, "_") || "_";

const protoTypeName = (name: string): string =>
  (
    protoFieldName(name)
      .split("_")
      .map((part) => part.charAt(0).toUpperCase() + part.slice(1))
      .join("") || "Type"
  ).replace(/^(?=[0-9])/, "_");

const uniqueName = (name: string, used: Set<string>, taken?: (name: string) => boolean): string => {
  let candidate = name;
  for (let n = 2; used.has(candidate) || taken?.(candidate); n++) candidate = name + n;
  used.add(candidate);
  return candidate;
};

// The members of an enum shape, a union of integer literals.
const enumValues = (shape: Internal, values = new Set<number>()): number[] => {
  if (shape.anyOf === U) values.add(shape.const as number);
  else for (let idx = 0; idx < shape.anyOf.length; idx++) {
    const member = getOutputSchema(shape.anyOf![idx]!);
    if (member.type === numberTag) values.add(member.const as number);
    else if (member.type === anyOfTag) enumValues(member, values);
  }
  return [...values].sort((a, b) => a - b);
};

const isEnumShape = ({ field, shape }: Use): boolean => field.type === "enum" && (shape.type === anyOfTag || shape.const !== U);

// What a field's message or enum is the same type as. `.with` copies a schema
// but shares its properties, so a message is its properties object, or that
// and its name once renamed into another type. A named enum is one type
// wherever its name and members recur; an unnamed one belongs to its field,
// so the next field's identical members stay separate and can diverge without
// a breaking change.
const messageKey = (proto: Proto, properties: object, name: string | undefined): unknown => {
  if (!name) return properties;
  let id = proto.ids.get(properties);
  if (id === U) proto.ids.set(properties, (id = proto.ids.size));
  return `${id}:${name}`;
};

// The file Google declares each of its types in, by the top-level type's name.
const googleFiles: Record<string, string> = {
  Any: "any",
  Api: "api",
  Method: "api",
  Mixin: "api",
  Duration: "duration",
  Empty: "empty",
  FieldMask: "field_mask",
  SourceContext: "source_context",
  Struct: "struct",
  Value: "struct",
  ListValue: "struct",
  NullValue: "struct",
  Timestamp: "timestamp",
  Type: "type",
  Field: "type",
  Enum: "type",
  EnumValue: "type",
  Option: "type",
  Syntax: "type",
  DoubleValue: "wrappers",
  FloatValue: "wrappers",
  Int64Value: "wrappers",
  UInt64Value: "wrappers",
  Int32Value: "wrappers",
  UInt32Value: "wrappers",
  BoolValue: "wrappers",
  StringValue: "wrappers",
  BytesValue: "wrappers",
};

const googleFile = (type: string): string => {
  const top = type.slice(16).split(".")[0]!;
  return Object.hasOwn(googleFiles, top)
    ? `google/protobuf/${googleFiles[top]}.proto`
    : panic(`S.toProtoOrThrow: ${type} is no well-known type, so there is no file to import it from`);
};

// A message or enum `S.protobufTypeName` names `google.protobuf.X` - every
// schema sury/protobuf/wkt exports - is Google's own, imported rather than
// declared, as is a value a well-known type holds as something other than its
// message.
const googleType = (use: Use): string | undefined => {
  const message = use.field.message;
  const name =
    message !== U ? message.wellKnown ?? message.typeName
    : isEnumShape(use) ? chainMeta(declaredThrough(use) || use.schema, use.shape).protobufTypeName
    : U;
  return name?.startsWith("google.protobuf.") ? name : U;
};

const typeKey = (proto: Proto, use: Use): unknown => {
  const { field, shape } = use;
  const name = nameOf(use);
  if (googleType(use) !== U) return U;
  if (field.message !== U) return messageKey(proto, shape.properties!, name);
  if (!isEnumShape(use)) return U;
  return name ? `${name}:${enumValues(shape).join(",")}` : field;
};

// The meta a schema carries down its `.to` chain to the object it produces:
// `S.meta` writes to the schema it is called on, so meta set after a `.to`
// sits above the shape.
const chainMeta = (schema: Internal, shape: Internal): Meta & { name?: string; protobufTypeName?: string } => {
  const meta: Meta & { name?: string; protobufTypeName?: string } = {};
  for (let current: Internal | undefined = schema; current !== U; current = current.to) {
    if (meta.name === U) meta.name = current.name;
    if (meta.protobufTypeName === U) meta.protobufTypeName = current.protobufTypeName;
    if (meta.description === U) meta.description = current.description;
    if (meta.deprecated === U) meta.deprecated = current.deprecated;
    if (current === shape) break;
  }
  return meta;
};

// The schema a wrapper holds the type's declaration as, when the field's
// schema reaches the declaration through `S.optional`, `S.array` or
// `S.record` rather than being a copy of it.
const declaredThrough = ({ schema, shape }: Use): Internal | undefined => {
  const output = getOutputSchema(schema);
  if (output === shape) return U;
  const [members, hasUndefined] = optionalMembers(output);
  const member = hasUndefined ? (members.length === 1 ? members[0] : U) : (output.additionalItems as Internal | undefined);
  if (member === U || typeof member !== objectTag) return U;
  for (let current: Internal | undefined = member; current !== U; current = current.to) if (current === shape) return member;
  return U;
};

// The name a use gives its type: down the chain of its own schema for a
// direct use, of the wrapped declaration otherwise. A `$ref` breaks the chain,
// so a message `S.recursive` named falls back to that name - which is what
// makes a self-reference key to the message it points back at.
const nameOf = (use: Use): string | undefined =>
  chainMeta(declaredThrough(use) || use.schema, use.shape).name ??
  (use.field.message && use.field.message.name);

const useType = (decl: TypeDecl, use: Use): void => {
  decl.metas.push(chainMeta(declaredThrough(use) || use.stored.numberedAs, use.shape));
};

// The value most of the uses that carry one agree on, the first on a tie;
// what a field carries beyond it prints on the field, so nothing is lost.
const typeMeta = <TKey extends keyof Meta>(decl: TypeDecl, key: TKey): Meta[TKey] => {
  const counts = new Map<Meta[TKey], number>();
  let best: Meta[TKey] = U;
  let bestCount = 0;
  for (const meta of decl.metas) {
    const value = meta[key];
    if (value === U) continue;
    const count = (counts.get(value) || 0) + 1;
    counts.set(value, count);
    if (count > bestCount) (best = value), (bestCount = count);
  }
  return best;
};

// A field's own meta: what its schema carries, or the declaration it wraps,
// beyond the type's.
const fieldMeta = <TKey extends keyof Meta>(use: Use, decl: TypeDecl | undefined, key: TKey): Meta[TKey] => {
  const own = use.schema[key] !== U ? use.schema[key] : use.shape[key];
  return decl !== U && own === typeMeta(decl, key) ? U : own;
};

const commentLines = (text: string | undefined, indent: string): string[] => {
  const trimmed = text ? text.replace(/\s+$/, "") : "";
  return trimmed
    ? trimmed.split("\n").map((line) => {
        const content = line.replace(/\s+$/, "");
        return `${indent}//${content ? ` ${content}` : ""}`;
      })
    : [];
};

const deprecatedLine = (decl: TypeDecl, indent: string): string[] =>
  typeMeta(decl, "deprecated") ? [`${indent}  option deprecated = true;`] : [];

// Named types claim their names before anything prints, so an unnamed type
// nested earlier can't take a name a schema asked for, and a copy used
// without the name still prints under it.
const reserveNames = (proto: Proto, message: Message, seen: Set<unknown>): void => {
  if (seen.has(message.object.properties)) return;
  seen.add(message.object.properties);
  for (const use of usesOf(proto, message)) {
    const key = typeKey(proto, use);
    const name = key !== U ? nameOf(use) : U;
    if (name && !proto.names.has(key)) proto.names.set(key, uniqueName(protoTypeName(name), proto.used));
    if (use.field.message !== U && googleType(use) === U) reserveNames(proto, use.field.message, seen);
  }
};

// Declares the message or enum a field refers to: a named schema goes to the
// top level and an unnamed one nests inside the message that first used it.
// Returns the name the field refers to it by.
const declareType = (
  proto: Proto,
  key: unknown,
  use: Use,
  parent: string,
  indent: string,
  nested: Line[],
  scope: Set<string>,
  fieldNames: Map<string, string>,
  members: Set<string>,
  body: (decl: TypeDecl, name: string, indent: string, members: Set<string>) => Line[]
): string => {
  let decl = proto.decls.get(key);
  if (decl !== U) {
    useType(decl, use);
    return proto.names.get(key)!;
  }
  const reserved = proto.names.get(key);
  decl = { metas: [] };
  useType(decl, use);
  proto.decls.set(key, decl);
  if (reserved !== U) {
    proto.top.push(body(decl, reserved, "", proto.members));
    return reserved;
  }
  // Fields and nested types share a message's scope.
  const typeName = uniqueName(protoTypeName(use.field.name ?? use.field.key), scope, (n) => proto.used.has(n) || fieldNames.has(n));
  const qualified = `${parent}.${typeName}`;
  proto.names.set(key, qualified);
  nested.push(...body(decl, typeName, `${indent}  `, members));
  return qualified;
};

const enumBody = (
  decl: TypeDecl,
  shape: Internal,
  name: string,
  indent: string,
  members: Set<string>,
  taken: (name: string) => boolean
): Line[] => {
  const values = enumValues(shape);
  // proto3 requires the first member to be zero, and buf lint the suffix.
  const entries: [string, number][] = [["UNSPECIFIED", 0]];
  for (const value of values) if (value !== 0) entries.push([value < 0 ? `MINUS_${-value}` : `${value}`, value]);
  const base = snakeCase(name).toUpperCase();
  let prefix = base;
  for (let n = 2; entries.some(([v]) => members.has(`${prefix}_${v}`) || taken(`${prefix}_${v}`)); n++) prefix = base + n;
  const lines: Line[] = [
    () => commentLines(typeMeta(decl, "description"), indent),
    `${indent}enum ${name} {`,
    () => deprecatedLine(decl, indent),
  ];
  for (const [suffix, value] of entries) {
    const member = `${prefix}_${suffix}`;
    members.add(member);
    lines.push(`${indent}  ${member} = ${value};`);
  }
  lines.push(`${indent}}`);
  return lines;
};

const messageBody = (
  proto: Proto,
  decl: TypeDecl,
  message: Message,
  name: string,
  qualified: string,
  indent: string
): Line[] => {
  const nested: Line[] = [];
  const entries: (string | Line[])[] = [];
  const oneofs = new Map<string, Line[]>();
  const fieldNames = new Map<string, string>();
  const jsonNames = new Map<string, string>();
  const scope = new Set<string>();
  const members = new Set<string>();
  // protoc keeps fields apart by their default JSON name, an underscore
  // capitalizing what follows, so `a_b` and `aB` are one field to it (protoc
  // 22 and later; the 3.x line also folded case); a oneof's name only has to
  // be a symbol of its own.
  const claim = (printed: string, key: string, json = printed): void => {
    const taken = fieldNames.get(printed) ?? jsonNames.get(json);
    if (taken !== U) {
      return panic(`S.toProtoOrThrow: "${taken}" and "${key}" of ${qualified} collide as "${json}"`);
    }
    jsonNames.set(json, key);
    fieldNames.set(printed, key);
  };
  const printedNames = new Map<Field, string>();
  const oneofNames = new Map<string, string>();
  for (const field of message.fields) {
    const own = field.name ?? field.key;
    const printed = protoFieldName(own);
    printedNames.set(field, printed);
    claim(printed, own, printed.replace(/_+(.)?/g, (_, next: string | undefined) => (next ? next.toUpperCase() : "")));
  }
  for (const field of message.fields) {
    if (field.oneof !== U && !oneofs.has(field.oneof)) {
      oneofs.set(field.oneof, []);
      const printed = protoFieldName(field.oneof);
      oneofNames.set(field.oneof, printed);
      // Symbol only: protoc doesn't give a oneof a JSON name.
      const taken = fieldNames.get(printed);
      if (taken !== U) return panic(`S.toProtoOrThrow: "${taken}" and oneof "${field.oneof}" of ${qualified} collide as "${printed}"`);
      fieldNames.set(printed, `oneof ${field.oneof}`);
    }
  }
  for (const use of usesOf(proto, message)) {
    const { field } = use;
    const key = typeKey(proto, use);
    let type: string;
    const google = googleType(use);
    if (google !== U) {
      type = google;
      proto.imports.add(googleFile(google));
    } else if (field.message !== U) {
      type = declareType(proto, key, use, qualified, indent, nested, scope, fieldNames, members, (fieldDecl, typeName, inner) =>
        messageBody(proto, fieldDecl, field.message!, typeName, proto.names.get(key)!, inner)
      );
    } else if (key !== U) {
      type = declareType(proto, key, use, qualified, indent, nested, scope, fieldNames, members, (fieldDecl, typeName, inner, values) =>
        enumBody(fieldDecl, use.shape, typeName, inner, values, values === members ? (n) => fieldNames.has(n) : (n) => proto.used.has(n))
      );
    } else type = field.type === "enum" ? "int32" : field.type;
    // A type nested in this message is referred to by its local name.
    if (type.startsWith(`${qualified}.`) && !type.includes(".", qualified.length + 1)) {
      type = type.slice(qualified.length + 1);
    }
    const fieldDecl = proto.decls.get(key);
    const inner = field.oneof === U ? `${indent}  ` : `${indent}    `;
    const fieldName = printedNames.get(field)!;
    const line = (): string[] => {
      const options: string[] = [];
      if (field.repeated && !field.packed && packable[field.type]) options.push("packed = false");
      if (fieldMeta(use, fieldDecl, "deprecated")) options.push("deprecated = true");
      const suffix = options.length ? ` [${options.join(", ")}]` : "";
      const rule =
        field.oneof !== U ? "" : field.map !== U ? `map<${field.map}, ` : field.repeated ? "repeated " : field.optional ? "optional " : "";
      const close = field.map !== U ? ">" : "";
      return [...commentLines(fieldMeta(use, fieldDecl, "description"), inner), `${inner}${rule}${type}${close} ${fieldName} = ${field.number}${suffix};`];
    };
    if (field.oneof !== U) {
      const oneof = oneofs.get(field.oneof)!;
      if (!oneof.length) entries.push(field.oneof);
      oneof.push(line);
    } else entries.push([line]);
  }
  const lines: Line[] = [
    () => commentLines(typeMeta(decl, "description"), indent),
    `${indent}message ${name} {`,
    () => deprecatedLine(decl, indent),
  ];
  if (nested.length) lines.push(...nested, "");
  for (const entry of entries) {
    if (typeof entry === "string") {
      lines.push(`${indent}  oneof ${oneofNames.get(entry)} {`, ...oneofs.get(entry)!, `${indent}  }`);
    } else lines.push(...entry);
  }
  lines.push(`${indent}}`);
  return lines;
};

const render = (lines: Line[]): string =>
  lines.flatMap((line) => (typeof line === "string" ? line : line())).join("\n");

// @__NO_SIDE_EFFECTS__
export const toProtoOrThrow = (schema: Internal, options?: ProtoOptions): string => {
  if (options?.package && !/^[A-Za-z_]\w*(\.[A-Za-z_]\w*)*$/.test(options.package)) {
    return panic(`S.toProtoOrThrow: "${options.package}" is not a package name`);
  }
  if (options?.name && !/^[A-Za-z_]\w*$/.test(options.name)) {
    return panic(`S.toProtoOrThrow: "${options.name}" is not a message name`);
  }
  // A message is printed as a writer is handed it where the schema converts
  // into the wire, and as a reader reads it otherwise, as the root is.
  let writes = false;
  for (let current = schema.to; current !== U; current = current.to) if (current.flags & 256) writes = true;
  const ctx = newCtx(false, writes);
  const wire = wireObject(schema, ctx);
  const message = wire === U ? U : compileMessage(wire, ctx);
  if (message === U) return panic("S.toProtoOrThrow: the schema is not an object");
  const output = message.object;
  const rootMeta = chainMeta(schema, output);
  const proto: Proto = {
    top: [],
    names: new Map(),
    decls: new Map(),
    used: new Set(),
    members: new Set(),
    ids: new Map(),
    uses: new Map(),
    imports: new Set(),
  };
  const decl: TypeDecl = { metas: [rootMeta] };
  const key = messageKey(proto, output.properties!, rootMeta.name);
  proto.decls.set(key, decl);
  // A root named by the caller or its meta claims first; the default yields
  // to whatever the fields named.
  const given = options?.name || (rootMeta.name && protoTypeName(rootMeta.name));
  if (given) proto.names.set(key, uniqueName(given, proto.used));
  reserveNames(proto, message, new Set());
  const name = proto.names.get(key) || uniqueName("Message", proto.used);
  proto.names.set(key, name);
  const root = messageBody(proto, decl, message, name, name, "");
  const imports = [...proto.imports].sort().map((file) => `import "${file}";\n`).join("");
  const header = `syntax = "proto3";\n${options?.package ? `\npackage ${options.package};\n` : ""}${imports && `\n${imports}`}`;
  return `${header}\n${[root, ...proto.top].map(render).join("\n\n")}\n`;
};
