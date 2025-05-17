@@warning("-30")

%%private(external magic: 'a => 'b = "%identity")

module WithReference = {
  type t<'a>
  type reference<'a> = {
    // The reference identifier. This MUST be in the form of a URI.
    @as("$ref")
    ref: string,
    // A short summary which by default SHOULD override that of the referenced component. If the referenced object-type does not allow a summary field, then this field has no effect.
    summary?: string,
    // A description which by default SHOULD override that of the referenced component. CommonMark syntax MAY be used for rich text representation. If the referenced object-type does not allow a description field, then this field has no effect.
    description?: string,
  }
  type tagged<'a> =
    | Object('a)
    | Reference(reference<'a>)

  external object: 'a => t<'a> = "%identity"
  external reference: reference<'a> => t<'a> = "%identity"

  let isReference = (_withReference: t<'a>): bool => {
    %raw(`"$ref" in _withReference`)
  }

  let classify = (withRef: t<'a>): tagged<'a> => {
    if withRef->isReference {
      Reference(withRef->(magic: t<'item> => reference<'item>))
    } else {
      Object(withRef->(magic: t<'item> => 'item))
    }
  }
}

/** 
Contact information for the exposed API.
 */
type contact = {
  // The identifying name of the contact person/organization.
  name?: string,
  // The URL pointing to the contact information. MUST be in the format of a URL.
  url?: string,
  // The email address of the contact person/organization. MUST be in the format of an email address.
  email?: string,
}

/** 
License information for the exposed API.
 */
type license = {
  // The license name used for the API.
  name: string,
  // An SPDX license expression for the API. The identifier field is mutually exclusive of the url field.
  identifier?: string,
  // A URL to the license used for the API. This MUST be in the form of a URL. The url field is mutually exclusive of the identifier field.
  url?: string,
}

/**
The object provides metadata about the API. The metadata MAY be used by the clients if needed, and MAY be presented in editing or documentation generation tools for convenience.
 */
type info = {
  // The title of the API.
  title: string,
  // A short summary of the API.
  summary?: string,
  // A description of the API. CommonMark syntax MAY be used for rich text representation.
  description?: string,
  // A URL to the Terms of Service for the API. This MUST be in the form of a URL.
  termsOfService?: string,
  // The contact information for the exposed API.
  contact?: contact,
  // The license information for the exposed API.
  license?: license,
  // The version of the OpenAPI document (which is distinct from the OpenAPI Specification version or the API implementation version).
  version: string,
}

/**
An object representing a Server Variable for server URL template substitution.
 */
type serverVariable = {
  // The default value to use for substitution, which SHALL be sent if an alternate value is not supplied. Note this behavior is different than the Schema Object's treatment of default values, because in those cases parameter values are optional. If the enum is defined, the value MUST exist in the enum's values.
  default: string,
  // An enumeration of string values to be used if the substitution options are from a limited set. The array MUST NOT be empty.
  enum?: array<string>,
  // An optional description for the server variable. CommonMark syntax MAY be used for rich text representation.
  description?: string,
}

/**
An object representing a Server.
 */
type server = {
  // A URL to the target host. This URL supports Server Variables and MAY be relative, to indicate that the host location is relative to the location where the OpenAPI document is being served. Variable substitutions will be made when a variable is named in {brackets}.
  url: string,
  // An optional string describing the host designated by the URL. CommonMark syntax MAY be used for rich text representation.
  description?: string,
  // A map between a variable name and its value. The value is used for substitution in the server's URL template.
  variables?: dict<serverVariable>,
}

type externalDocumentation = {
  // A description of the target documentation. CommonMark syntax MAY be used for rich text representation.
  description?: string,
  // The URL for the target documentation. This MUST be in the form of a URL.
  url: string,
}

/** 
There are four possible parameter locations specified by the in field:

path - Used together with Path Templating, where the parameter value is actually part of the operation's URL. This does not include the host or base path of the API. For example, in /items/{itemId}, the path parameter is itemId.
query - Parameters that are appended to the URL. For example, in /items?id=###, the query parameter is id.
header - Custom headers that are expected as part of the request. Note that RFC7230 states header names are case insensitive.
cookie - Used to pass a specific cookie value to the API.
 */
type parameterLocation =
  | @as("query") Query
  | @as("header") Header
  | @as("path") Path
  | @as("cookie") Cookie

/** 
 * The style of a parameter.
 * Describes how the parameter value will be serialized.
 * (serialization is not implemented yet)
 * Specification:
 * https://github.com/OAI/OpenAPI-Specification/blob/main/versions/3.1.0.md#style-values
 */
type parameterStyle = [
  | #matrix
  | #label
  | #form
  | #simple
  | #spaceDelimited
  | #pipeDelimited
  | #deepObject
]

type example = {
  // Short description for the example.
  summary?: string,
  // Long description for the example. CommonMark syntax MAY be used for rich text representation.
  description?: string,
  // Embedded literal example. The value field and externalValue field are mutually exclusive. To represent examples of media types that cannot naturally represented in JSON or YAML, use a string value to contain the example, escaping where necessary.
  value?: unknown,
  // A URI that points to the literal example. This provides the capability to reference examples that cannot easily be included in JSON or YAML documents. The value field and externalValue field are mutually exclusive. See the rules for resolving Relative References.
  externalValue?: string,
}

type schema = JSONSchema.t

/** 
Base fields for parameter object
 */
type rec baseParameter = {
  // A brief description of the parameter. This could contain examples of use. CommonMark syntax MAY be used for rich text representation.
  description?: string,
  // Determines whether this parameter is mandatory. If the parameter location is "path", this property is REQUIRED and its value MUST be true. Otherwise, the property MAY be included and its default value is false.
  required?: bool,
  // Specifies that a parameter is deprecated and SHOULD be transitioned out of usage. Default value is false.
  deprecated?: bool,
  // Sets the ability to pass empty-valued parameters. This is valid only for query parameters and allows sending a parameter with an empty value. Default value is false. If style is used, and if behavior is n/a (cannot be serialized), the value of allowEmptyValue SHALL be ignored. Use of this property is NOT RECOMMENDED, as it is likely to be removed in a later revision.
  allowEmptyValue?: bool,
  // Describes how the parameter value will be serialized depending on the type of the parameter value. Default values (based on value of in): for query - form; for path - simple; for header - simple; for cookie - form.
  style?: parameterStyle,
  // When this is true, parameter values of type array or object generate separate parameters for each value of the array or key-value pair of the map. For other types of parameters this property has no effect. When style is form, the default value is true. For all other styles, the default value is false.
  explode?: bool,
  // Determines whether the parameter value SHOULD allow reserved characters, as defined by RFC3986 :/?#[]@!$&'()*+,;= to be included without percent-encoding. This property only applies to parameters with an in value of query. The default value is false.
  allowReserved?: bool,
  // The schema defining the type used for the parameter.
  schema?: schema,
  // Examples of the parameter's potential value. Each example SHOULD contain a value in the correct format as specified in the parameter encoding. The examples field is mutually exclusive of the example field. Furthermore, if referencing a schema that contains an example, the examples value SHALL override the example provided by the schema.
  examples?: dict<WithReference.t<example>>,
  // Example of the parameter's potential value. The example SHOULD match the specified schema and encoding properties if present. The example field is mutually exclusive of the examples field. Furthermore, if referencing a schema that contains an example, the example value SHALL override the example provided by the schema. To represent examples of media types that cannot naturally be represented in JSON or YAML, a string value can contain the example with escaping where necessary.
  example?: option<Js.Json.t>,
  content?: dict<WithReference.t<mediaType>>,
}

/*
The Header Object follows the structure of the Parameter Object with the following changes:

name MUST NOT be specified, it is given in the corresponding headers map.
in MUST NOT be specified, it is implicitly in header.
All traits that are affected by the location MUST be applicable to a location of header (for example, style).
 */
and header = baseParameter

/*
A single encoding definition applied to a single schema property.
 */
and encoding = {
  // The Content-Type for encoding a specific property. Default value depends on the property type: for object - application/json; for array – the default is defined based on the inner type; for all other cases the default is application/octet-stream. The value can be a specific media type (e.g. application/json), a wildcard media type (e.g. image/*), or a comma-separated list of the two types.
  contentType?: string,
  // A map allowing additional information to be provided as headers, for example Content-Disposition. Content-Type is described separately and SHALL be ignored in this section. This property SHALL be ignored if the request body media type is not a multipart.
  headers?: dict<WithReference.t<header>>,
  // Describes how a specific property value will be serialized depending on its type. See Parameter Object for details on the style property. The behavior follows the same values as query parameters, including default values. This property SHALL be ignored if the request body media type is not application/x-www-form-urlencoded or multipart/form-data. If a value is explicitly defined, then the value of contentType (implicit or explicit) SHALL be ignored.
  style?: string,
  // When this is true, property values of type array or object generate separate parameters for each value of the array, or key-value-pair of the map. For other types of properties this property has no effect. When style is form, the default value is true. For all other styles, the default value is false. This property SHALL be ignored if the request body media type is not application/x-www-form-urlencoded or multipart/form-data. If a value is explicitly defined, then the value of contentType (implicit or explicit) SHALL be ignored.
  explode?: bool,
  // Determines whether the parameter value SHOULD allow reserved characters, as defined by RFC3986 :/?#[]@!$&'()*+,;= to be included without percent-encoding. The default value is false. This property SHALL be ignored if the request body media type is not application/x-www-form-urlencoded or multipart/form-data. If a value is explicitly defined, then the value of contentType (implicit or explicit) SHALL be ignored.
  allowReserved?: bool,
}

/*
Each Media Type Object provides schema and examples for the media type identified by its key.
 */
and mediaType = {
  // The schema defining the content of the request, response, or parameter.
  schema?: schema,
  // Example of the media type. The example object SHOULD be in the correct format as specified by the media type. The example field is mutually exclusive of the examples field. Furthermore, if referencing a schema which contains an example, the example value SHALL override the example provided by the schema.
  example?: unknown,
  // Examples of the media type. Each example object SHOULD match the media type and specified schema if present. The examples field is mutually exclusive of the example field. Furthermore, if referencing a schema which contains an example, the examples value SHALL override the example provided by the schema.
  examples?: dict<WithReference.t<example>>,
  // A map between a property name and its encoding information. The key, being the property name, MUST exist in the schema as a property. The encoding object SHALL only apply to requestBody objects when the media type is multipart or application/x-www-form-urlencoded.
  encoding?: dict<encoding>,
}

/**
Describes a single operation parameter.

A unique parameter is defined by a combination of a name and location.
 */
type parameter = {
  // The name of the parameter. Parameter names are case sensitive.
  // If in is "path", the name field MUST correspond to a template expression occurring within the path field in the Paths Object. See Path Templating for further information.
  // If in is "header" and the name field is "Accept", "Content-Type" or "Authorization", the parameter definition SHALL be ignored.
  // For all other cases, the name corresponds to the parameter name used by the in property.
  name: string,
  // The location of the parameter. Possible values are "query", "header", "path" or "cookie".
  @as("in")
  in_: parameterLocation,
  ...baseParameter,
}

/**
Describes a single request body.
 */
type requestBody = {
  // A brief description of the request body. This could contain examples of use. CommonMark syntax MAY be used for rich text representation.
  description?: string,
  // The content of the request body. The key is a media type or media type range and the value describes it. For requests that match multiple keys, only the most specific key is applicable. e.g. text/plain overrides text/*
  content: dict<mediaType>,
  // Determines if the request body is required in the request. Defaults to false.
  required?: bool,
}

/**
The Link object represents a possible design-time link for a response. The presence of a link does not guarantee the caller's ability to successfully invoke it, rather it provides a known relationship and traversal mechanism between responses and other operations.

Unlike dynamic links (i.e. links provided in the response payload), the OAS linking mechanism does not require link information in the runtime response.

For computing links, and providing instructions to execute them, a runtime expression is used for accessing values in an operation and using them as parameters while invoking the linked operation.
 */
type link = {
  // A relative or absolute URI reference to an OAS operation. This field is mutually exclusive of the operationId field, and MUST point to an Operation Object. Relative operationRef values MAY be used to locate an existing Operation Object in the OpenAPI definition. See the rules for resolving Relative References.
  operationRef?: string,
  // The name of an existing, resolvable OAS operation, as defined with a unique operationId. This field is mutually exclusive of the operationRef field.
  operationId?: string,
  // A map representing parameters to pass to an operation as specified with operationId or identified via operationRef. The key is the parameter name to be used, whereas the value can be a constant or an expression to be evaluated and passed to the linked operation. The parameter name can be qualified using the parameter location [{in}.]{name} for operations that use the same parameter name in different locations (e.g. path.id).
  parameters?: dict<unknown>, // TODO: The value should be typed as Any | {expression}
  // A literal value or {expression} to use as a request body when calling the target operation.
  requestBody?: unknown, // TODO: The value should be typed as Any | {expression}
  // A description of the link. CommonMark syntax MAY be used for rich text representation.
  description?: string,
  // A server object to be used by the target operation.
  server?: server,
}

/**
Describes a single response from an API Operation, including design-time, static links to operations based on the response.
 */
type response = {
  // A description of the response. CommonMark syntax MAY be used for rich text representation.
  description: string,
  // Maps a header name to its definition. RFC7230 states header names are case insensitive. If a response header is defined with the name "Content-Type", it SHALL be ignored.
  headers?: dict<WithReference.t<header>>,
  // A map containing descriptions of potential response payloads. The key is a media type or media type range and the value describes it. For responses that match multiple keys, only the most specific key is applicable. e.g. text/plain overrides text/*
  content?: dict<mediaType>,
  // A map of operations links that can be followed from the response. The key of the map is a short name for the link, following the naming constraints of the names for Component Objects.
  links?: dict<WithReference.t<link>>,
}

/* https://github.com/OAI/OpenAPI-Specification/blob/main/versions/3.1.0.md#security-requirement-object */
type securityRequirement = dict<array<string>>

/**
Describes the operations available on a single path. A Path Item MAY be empty, due to ACL constraints. The path itself is still exposed to the documentation viewer but they will not know which operations and parameters are available.
 */
type rec pathItem = {
  // Allows for a referenced definition of this path item. The referenced structure MUST be in the form of a Path Item Object. In case a Path Item Object field appears both in the defined object and the referenced object, the behavior is undefined. See the rules for resolving Relative References.
  @as("$ref")
  ref?: string,
  // An optional, string summary, intended to apply to all operations in this path.
  summary?: string,
  // An optional, string description, intended to apply to all operations in this path. CommonMark syntax MAY be used for rich text representation.
  description?: string,
  // A definition of a GET operation on this path.
  get?: operation,
  // A definition of a PUT operation on this path.
  put?: operation,
  // A definition of a POST operation on this path.
  post?: operation,
  // A definition of a DELETE operation on this path.
  delete?: operation,
  // A definition of a OPTIONS operation on this path.
  options?: operation,
  // A definition of a HEAD operation on this path.
  head?: operation,
  // A definition of a PATCH operation on this path.
  patch?: operation,
  // A definition of a TRACE operation on this path.
  trace?: operation,
  // An alternative server array to service all operations in this path.
  servers?: array<server>,
  // A list of parameters that are applicable for all the operations described under this path. These parameters can be overridden at the operation level, but cannot be removed there. The list MUST NOT include duplicated parameters. A unique parameter is defined by a combination of a name and location. The list can use the Reference Object to link to parameters that are defined at the OpenAPI Object's components/parameters.
  parameters?: array<WithReference.t<parameter>>,
}
/*
Describes a single API operation on a path.
 */
and operation = {
  // A list of tags for API documentation control. Tags can be used for logical grouping of operations by resources or any other qualifier.
  tags?: array<string>,
  // A short summary of what the operation does.
  summary?: string,
  // A verbose explanation of the operation behavior. CommonMark syntax MAY be used for rich text representation.
  description?: string,
  // Additional external documentation for this operation.
  externalDocs?: externalDocumentation,
  // Unique string used to identify the operation. The id MUST be unique among all operations described in the API. The operationId value is case-sensitive. Tools and libraries MAY use the operationId to uniquely identify an operation, therefore, it is RECOMMENDED to follow common programming naming conventions.
  operationId?: string,
  // A list of parameters that are applicable for this operation. If a parameter is already defined at the Path Item, the new definition will override it but can never remove it. The list MUST NOT include duplicated parameters. A unique parameter is defined by a combination of a name and location. The list can use the Reference Object to link to parameters that are defined at the OpenAPI Object's components/parameters.
  parameters?: array<WithReference.t<parameter>>,
  // The request body applicable for this operation. The requestBody is fully supported in HTTP methods where the HTTP 1.1 specification RFC7231 has explicitly defined semantics for request bodies. In other cases where the HTTP spec is vague (such as GET, HEAD and DELETE), requestBody is permitted but does not have well-defined semantics and SHOULD be avoided if possible.
  requestBody?: WithReference.t<requestBody>,
  // The list of possible responses as they are returned from executing this operation.
  responses?: dict<WithReference.t<response>>,
  // A map of possible out-of band callbacks related to the parent operation. The key is a unique identifier for the Callback Object. Each value in the map is a Callback Object that describes a request that may be initiated by the API provider and the expected responses.
  callbacks?: dict<WithReference.t<callback>>,
  // Declares this operation to be deprecated. Consumers SHOULD refrain from usage of the declared operation. Default value is false.
  deprecated?: bool,
  // A declaration of which security mechanisms can be used for this operation. The list of values includes alternative security requirement objects that can be used. Only one of the security requirement objects need to be satisfied to authorize a request. To make security optional, an empty security requirement ({}) can be included in the array. This definition overrides any declared top-level security. To remove a top-level security declaration, an empty array can be used.
  security?: array<securityRequirement>,
  // An alternative server array to service this operation. If an alternative server object is specified at the Path Item Object or Root level, it will be overridden by this value.
  servers?: array<server>,
}
and callback = dict<WithReference.t<pathItem>>

/**
Configuration details for a supported OAuth Flow
 */
type oauthFlow = {
  // The authorization URL to be used for this flow. This MUST be in the form of a URL. The OAuth2 standard requires the use of TLS.
  authorizationUrl: string,
  // The token URL to be used for this flow. This MUST be in the form of a URL. The OAuth2 standard requires the use of TLS.
  tokenUrl: string,
  // The URL to be used for obtaining refresh tokens. This MUST be in the form of a URL. The OAuth2 standard requires the use of TLS.
  refreshUrl?: string,
  // The available scopes for the OAuth2 security scheme. A map between the scope name and a short description for it. The map MAY be empty.
  scopes: dict<string>,
}

/**
Allows configuration of the supported OAuth Flows.
 */
type oauthFlows = {
  // Configuration for the OAuth Implicit flow
  implicit?: oauthFlow,
  // Configuration for the OAuth Resource Owner Password flow
  password?: oauthFlow,
  // Configuration for the OAuth Client Credentials flow. Previously called application in OpenAPI 2.0.
  clientCredentials?: oauthFlow,
  // Configuration for the OAuth Authorization Code flow. Previously called accessCode in OpenAPI 2.0.
  authorizationCode?: oauthFlow,
}

/**
Defines a security scheme that can be used by the operations.  
  
Supported schemes are HTTP authentication, an API key (either as a header, a cookie parameter or as a query parameter), mutual TLS (use of a client certificate), OAuth2's common flows (implicit, password, client credentials and authorization code) as defined in RFC6749, and OpenID Connect Discovery. Please note that as of 2020, the implicit flow is about to be deprecated by OAuth 2.0 Security Best Current Practice. Recommended for most use case is Authorization Code Grant flow with PKCE.
 */
type securityScheme = {
  // The type of the security scheme. Valid values are "apiKey", "http", "mutualTLS", "oauth2", "openIdConnect".
  @as("type")
  type_: string,
  // A description for security scheme. CommonMark syntax MAY be used for rich text representation.
  description?: string,
  // The name of the header, query or cookie parameter to be used.
  name: string,
  // The location of the API key. Valid values are "query", "header" or "cookie".
  @as("in")
  in_: string,
  // The name of the HTTP Authorization scheme to be used in the Authorization header as defined in RFC7235. The values used SHOULD be registered in the IANA Authentication Scheme registry.
  scheme: string,
  // A hint to the client to identify how the bearer token is formatted. Bearer tokens are usually generated by an authorization server, so this information is primarily for documentation purposes.
  bearerFormat?: string,
  // An object containing configuration information for the flow types supported.
  flows: oauthFlows,
  // OpenId Connect URL to discover OAuth2 configuration values. This MUST be in the form of a URL. The OpenID Connect standard requires the use of TLS.
  openIdConnectUrl: string,
} // TODO: Use variant type

/** 
Holds a set of reusable objects for different aspects of the OAS. All objects defined within the components object will have no effect on the API unless they are explicitly referenced from properties outside the components object.
*/
type components = {
  // An object to hold reusable Schema Objects.
  schemas?: dict<schema>,
  // An object to hold reusable Response Objects.
  responses?: dict<WithReference.t<response>>,
  // An object to hold reusable Parameter Objects.
  parameters?: dict<WithReference.t<parameter>>,
  // An object to hold reusable Example Objects.
  examples?: dict<WithReference.t<example>>,
  // An object to hold reusable Request Body Objects.
  requestBodies?: dict<WithReference.t<requestBody>>,
  // An object to hold reusable Header Objects.
  headers?: dict<WithReference.t<header>>,
  // An object to hold reusable Security Scheme Objects.
  securitySchemes?: dict<WithReference.t<securityScheme>>,
  // An object to hold reusable Link Objects.
  links?: dict<WithReference.t<link>>,
  // An object to hold reusable Callback Objects.
  callbacks?: dict<WithReference.t<callback>>,
  // An object to hold reusable Path Item Object.
  pathItems?: dict<WithReference.t<pathItem>>,
}

/**
Adds metadata to a single tag that is used by the Operation Object. It is not mandatory to have a Tag Object per tag defined in the Operation Object instances.
 */
type tag = {
  // The name of the tag.
  name: string,
  // A description for the tag. CommonMark syntax MAY be used for rich text representation.
  description?: string,
  // Additional external documentation for this tag.
  externalDocs?: externalDocumentation,
}

/**
Typed interfaces for OpenAPI 3.1.0  
see https://github.com/OAI/OpenAPI-Specification/blob/main/versions/3.1.0.md  
  
This is the root object of the OpenAPI document.
 */
type t = {
  // This string MUST be the version number of the OpenAPI Specification that the OpenAPI document uses. The openapi field SHOULD be used by tooling to interpret the OpenAPI document. This is not related to the API info.version string.
  openapi: string,
  // Provides metadata about the API. The metadata MAY be used by tooling as required.
  info: info,
  // The default value for the $schema keyword within Schema Objects contained within this OAS document. This MUST be in the form of a URI.
  jsonSchemaDialect?: string,
  // An array of Server Objects, which provide connectivity information to a target server. If the servers property is not provided, or is an empty array, the default value would be a Server Object with a url value of `/`.
  servers?: array<server>,
  // The available paths and operations for the API.
  paths?: dict<pathItem>,
  // The incoming webhooks that MAY be received as part of this API and that the API consumer MAY choose to implement. Closely related to the callbacks feature, this section describes requests initiated other than by an API call, for example by an out of band registration. The key name is a unique string to refer to each webhook, while the (optionally referenced) Path Item Object describes a request that may be initiated by the API provider and the expected responses. An example is available.
  webhooks?: dict<WithReference.t<pathItem>>,
  // An element to hold various schemas for the document.
  components?: components,
  // A declaration of which security mechanisms can be used across the API. The list of values includes alternative security requirement objects that can be used. Only one of the security requirement objects need to be satisfied to authorize a request. Individual operations can override this definition. To make security optional, an empty security requirement ({}) can be included in the array.
  security?: array<securityRequirement>,
  // A list of tags used by the document with additional metadata. The order of the tags can be used to reflect on their order by the parsing tools. Not all tags that are used by the Operation Object must be declared. The tags that are not declared MAY be organized randomly or based on the tools' logic. Each tag name in the list MUST be unique.
  tags?: array<tag>,
  // Additional external documentation.
  externalDocs?: externalDocumentation,
}

module Mutable = {
  type readOnly = t

  module WithReference = {
    type t<'a>
    type reference<'a> = {
      @as("$ref")
      mutable ref: string,
      mutable summary?: string,
      mutable description?: string,
    }
    type tagged<'a> =
      | Object('a)
      | Reference(reference<'a>)

    external object: 'a => t<'a> = "%identity"
    external reference: reference<'a> => t<'a> = "%identity"

    let isReference = (_withReference: t<'a>): bool => {
      %raw(`"$ref" in _withReference`)
    }

    let classify = (withRef: t<'a>): tagged<'a> => {
      if withRef->isReference {
        Reference(withRef->(magic: t<'item> => reference<'item>))
      } else {
        Object(withRef->(magic: t<'item> => 'item))
      }
    }
  }

  type contact = {
    mutable name?: string,
    mutable url?: string,
    mutable email?: string,
  }

  type license = {
    mutable name: string,
    mutable identifier?: string,
    mutable url?: string,
  }

  type info = {
    mutable title: string,
    mutable summary?: string,
    mutable description?: string,
    mutable termsOfService?: string,
    mutable contact?: contact,
    mutable license?: license,
    mutable version: string,
  }

  type serverVariable = {
    mutable default: string,
    mutable enum?: array<string>,
    mutable description?: string,
  }

  type server = {
    mutable url: string,
    mutable description?: string,
    mutable variables?: dict<serverVariable>,
  }

  type externalDocumentation = {
    mutable description?: string,
    mutable url: string,
  }

  type example = {
    mutable summary?: string,
    mutable description?: string,
    mutable value?: unknown,
    mutable externalValue?: string,
  }

  type rec baseParameter = {
    mutable description?: string,
    mutable required?: bool,
    mutable deprecated?: bool,
    mutable allowEmptyValue?: bool,
    mutable style?: parameterStyle,
    mutable explode?: bool,
    mutable allowReserved?: bool,
    mutable schema?: schema,
    mutable examples?: dict<WithReference.t<example>>,
    mutable example?: option<Js.Json.t>,
    mutable content?: dict<WithReference.t<mediaType>>,
  }

  and header = baseParameter

  and encoding = {
    mutable contentType?: string,
    mutable headers?: dict<WithReference.t<header>>,
    mutable style?: string,
    mutable explode?: bool,
    mutable allowReserved?: bool,
  }

  and mediaType = {
    mutable schema?: schema,
    mutable example?: unknown,
    mutable examples?: dict<WithReference.t<example>>,
    mutable encoding?: dict<encoding>,
  }

  type parameter = {
    mutable name: string,
    @as("in")
    mutable in_: parameterLocation,
    ...baseParameter,
  }

  type requestBody = {
    mutable description?: string,
    mutable content: dict<mediaType>,
    mutable required?: bool,
  }

  type link = {
    mutable operationRef?: string,
    mutable operationId?: string,
    mutable parameters?: dict<unknown>, // TODO: The value should be typed as Any | {expression}
    mutable requestBody?: unknown, // TODO: The value should be typed as Any | {expression}
    mutable description?: string,
    mutable server?: server,
  }

  type response = {
    mutable description: string,
    mutable headers?: dict<WithReference.t<header>>,
    mutable content?: dict<mediaType>,
    mutable links?: dict<WithReference.t<link>>,
  }

  type rec pathItem = {
    @as("$ref")
    mutable ref?: string,
    mutable summary?: string,
    mutable description?: string,
    mutable get?: operation,
    mutable put?: operation,
    mutable post?: operation,
    mutable delete?: operation,
    mutable options?: operation,
    mutable head?: operation,
    mutable patch?: operation,
    mutable trace?: operation,
    mutable servers?: array<server>,
    mutable parameters?: array<WithReference.t<parameter>>,
  }
  and operation = {
    mutable tags?: array<string>,
    mutable summary?: string,
    mutable description?: string,
    mutable externalDocs?: externalDocumentation,
    mutable operationId?: string,
    mutable parameters?: array<WithReference.t<parameter>>,
    mutable requestBody?: WithReference.t<requestBody>,
    mutable responses?: dict<WithReference.t<response>>,
    mutable callbacks?: dict<WithReference.t<callback>>,
    mutable deprecated?: bool,
    mutable security?: array<securityRequirement>,
    mutable servers?: array<server>,
  }
  and callback = dict<WithReference.t<pathItem>>

  type oauthFlow = {
    mutable authorizationUrl: string,
    mutable tokenUrl: string,
    mutable refreshUrl?: string,
    mutable scopes: dict<string>,
  }

  type oauthFlows = {
    mutable implicit?: oauthFlow,
    mutable password?: oauthFlow,
    mutable clientCredentials?: oauthFlow,
    mutable authorizationCode?: oauthFlow,
  }

  type securityScheme = {
    @as("type")
    mutable type_: string,
    mutable description?: string,
    mutable name: string,
    @as("in")
    mutable in_: string,
    mutable scheme: string,
    mutable bearerFormat?: string,
    mutable flows: oauthFlows,
    mutable openIdConnectUrl: string,
  } // TODO: Use variant type

  type components = {
    mutable schemas?: dict<schema>,
    mutable responses?: dict<WithReference.t<response>>,
    mutable parameters?: dict<WithReference.t<parameter>>,
    mutable examples?: dict<WithReference.t<example>>,
    mutable requestBodies?: dict<WithReference.t<requestBody>>,
    mutable headers?: dict<WithReference.t<header>>,
    mutable securitySchemes?: dict<WithReference.t<securityScheme>>,
    mutable links?: dict<WithReference.t<link>>,
    mutable callbacks?: dict<WithReference.t<callback>>,
    mutable pathItems?: dict<WithReference.t<pathItem>>,
  }

  type tag = {
    mutable name: string,
    mutable description?: string,
    mutable externalDocs?: externalDocumentation,
  }

  type t = {
    mutable openapi: string,
    mutable info: info,
    mutable jsonSchemaDialect?: string,
    mutable servers?: array<server>,
    mutable paths?: dict<pathItem>,
    mutable webhooks?: dict<WithReference.t<pathItem>>,
    mutable components?: components,
    mutable security?: array<securityRequirement>,
    mutable tags?: array<tag>,
    mutable externalDocs?: externalDocumentation,
  }

  external fromReadOnly: readOnly => t = "%identity"
  external toReadOnly: t => readOnly = "%identity"

  @val
  external mixin: (t, readOnly) => unit = "Object.assign"
}
