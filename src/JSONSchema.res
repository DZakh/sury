@@uncurried

%%private(external magic: 'a => 'b = "%identity")

module Arrayable = {
  type t<'item>
  type tagged<'item> = Single('item) | Array(array<'item>)

  external array: array<'item> => t<'item> = "%identity"
  external single: 'item => t<'item> = "%identity"

  external isArray: t<'item> => bool = "Array.isArray"

  let classify = (arrayable: t<'item>): tagged<'item> => {
    if arrayable->isArray {
      Array(arrayable->(magic: t<'item> => array<'item>))
    } else {
      Single(arrayable->(magic: t<'item> => 'item))
    }
  }
}

/**
 * Primitive type
 * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.1.1
 */
type typeName = [
  | #string
  | #number
  | #integer
  | #boolean
  | #object
  | #array
  | #null
]

/**
 * Meta schema
 *
 * Recommended values:
 * - 'http://json-schema.org/schema#'
 * - 'http://json-schema.org/hyper-schema#'
 * - 'http://json-schema.org/draft-07/schema#'
 * - 'http://json-schema.org/draft-07/hyper-schema#'
 *
 * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-5
 */
type version = string

/**
 * JSON Schema v7
 * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01
 */
type rec t = {
  @as("$id")
  id?: string,
  @as("$ref")
  ref?: string,
  @as("$schema")
  schema?: version,
  /**
   * @see https://datatracker.ietf.org/doc/html/draft-bhutton-json-schema-00#section-8.2.4
   * @see https://datatracker.ietf.org/doc/html/draft-bhutton-json-schema-validation-00#appendix-A
   */
  @as("$defs")
  defs?: Js.Dict.t<definition>,
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.1
   */
  @as("type")
  type_?: Arrayable.t<typeName>,
  enum?: array<Js.Json.t>,
  const?: Js.Json.t,
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.2
   */
  multipleOf?: float,
  maximum?: float,
  exclusiveMaximum?: float,
  minimum?: float,
  exclusiveMinimum?: float,
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.3
   */
  maxLength?: int,
  minLength?: int,
  pattern?: string,
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.4
   */
  items?: Arrayable.t<definition>,
  additionalItems?: definition,
  maxItems?: int,
  minItems?: int,
  uniqueItems?: bool,
  contains?: t,
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.5
   */
  maxProperties?: int,
  minProperties?: int,
  required?: array<string>,
  properties?: Js.Dict.t<definition>,
  patternProperties?: Js.Dict.t<definition>,
  additionalProperties?: definition,
  dependencies?: Js.Dict.t<dependency>,
  propertyNames?: definition,
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.6
   */
  @as("if")
  if_?: definition,
  then?: definition,
  @as("else")
  else_?: definition,
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.7
   */
  allOf?: array<definition>,
  anyOf?: array<definition>,
  oneOf?: array<definition>,
  not?: definition,
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-7
   */
  format?: string,
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-8
   */
  contentMediaType?: string,
  contentEncoding?: string,
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-9
   */
  definitions?: Js.Dict.t<definition>,
  /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-10
   */
  title?: string,
  description?: string,
  default?: Js.Json.t,
  readOnly?: bool,
  writeOnly?: bool,
  examples?: array<Js.Json.t>,
}
and definition
and dependency

module Mutable = {
  type readOnly = t
  type t = {
    @as("$id")
    mutable id?: string,
    @as("$ref")
    mutable ref?: string,
    @as("$schema")
    mutable schema?: version,
    /**
   * @see https://datatracker.ietf.org/doc/html/draft-bhutton-json-schema-00#section-8.2.4
   * @see https://datatracker.ietf.org/doc/html/draft-bhutton-json-schema-validation-00#appendix-A
   */
    @as("$defs")
    mutable defs?: Js.Dict.t<definition>,
    /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.1
   */
    @as("type")
    mutable type_?: Arrayable.t<typeName>,
    mutable enum?: array<Js.Json.t>,
    mutable const?: Js.Json.t,
    /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.2
   */
    mutable multipleOf?: float,
    mutable maximum?: float,
    mutable exclusiveMaximum?: float,
    mutable minimum?: float,
    mutable exclusiveMinimum?: float,
    /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.3
   */
    mutable maxLength?: int,
    mutable minLength?: int,
    mutable pattern?: string,
    /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.4
   */
    mutable items?: Arrayable.t<definition>,
    mutable additionalItems?: definition,
    mutable maxItems?: int,
    mutable minItems?: int,
    mutable uniqueItems?: bool,
    mutable contains?: t,
    /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.5
   */
    mutable maxProperties?: int,
    mutable minProperties?: int,
    mutable required?: array<string>,
    mutable properties?: Js.Dict.t<definition>,
    mutable patternProperties?: Js.Dict.t<definition>,
    mutable additionalProperties?: definition,
    mutable dependencies?: Js.Dict.t<dependency>,
    mutable propertyNames?: definition,
    /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.6
   */
    @as("if")
    mutable if_?: definition,
    mutable then?: definition,
    @as("else")
    mutable else_?: definition,
    /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-6.7
   */
    mutable allOf?: array<definition>,
    mutable anyOf?: array<definition>,
    mutable oneOf?: array<definition>,
    mutable not?: definition,
    /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-7
   */
    mutable format?: string,
    /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-8
   */
    mutable contentMediaType?: string,
    mutable contentEncoding?: string,
    /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-9
   */
    mutable definitions?: Js.Dict.t<definition>,
    /**
   * @see https://tools.ietf.org/html/draft-handrews-json-schema-validation-01#section-10
   */
    mutable title?: string,
    mutable description?: string,
    mutable default?: Js.Json.t,
    mutable readOnly?: bool,
    mutable writeOnly?: bool,
    mutable examples?: array<Js.Json.t>,
  }

  external fromReadOnly: readOnly => t = "%identity"
  external toReadOnly: t => readOnly = "%identity"

  @val
  external mixin: (t, readOnly) => unit = "Object.assign"
}

module Definition = {
  type tagged = Schema(t) | Boolean(bool)

  external schema: t => definition = "%identity"
  external boolean: bool => definition = "%identity"

  let classify = definition => {
    if definition->Js.typeof === "boolean" {
      Boolean(definition->(magic: definition => bool))
    } else {
      Schema(definition->(magic: definition => t))
    }
  }
}

module Dependency = {
  @unboxed
  type tagged = Schema(t) | Required(array<string>)

  external required: array<string> => dependency = "%identity"
  external schema: t => dependency = "%identity"

  external classify: dependency => tagged = "%identity"
}
