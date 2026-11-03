open Ppxlib
open Parsetree
open Ast_helper
open Util

(* Every branch here must keep the schema's value type equal to ~value_type -
   @s.with pins against it, so a branch that widened or narrowed the value would
   make that pin reject valid code. *)
let applySchemaAttribute ~loc ~value_type schema_expr
    ({attr_name = {Location.txt}} as attribute) =
  match txt with
  | "s.strict" -> [%expr S.strict [%e schema_expr]]
  | "s.strip" -> [%expr S.strip [%e schema_expr]]
  | "s.deepStrict" -> [%expr S.deepStrict [%e schema_expr]]
  | "s.deepStrip" -> [%expr S.deepStrip [%e schema_expr]]
  | "s.noValidation" -> [%expr S.noValidation [%e schema_expr] true]
  | "s.meta" ->
    let meta_value = getExpressionFromPayload attribute in
    [%expr S.meta [%e schema_expr] [%e meta_value]]
  | "s.with" ->
    let fn_expr = getExpressionFromPayload attribute in
    (* Constrain both the argument and the result, forcing the payload to
       `S.t<value> => S.t<value>` so a transform that changes the value type
       (S.to) is a compile error rather than a schema that silently disagrees
       with the type it was generated for. *)
    let loc = fn_expr.pexp_loc in
    let schema_type = [%type: [%t value_type] S.t] in
    Exp.constraint_ ~loc
      (Exp.apply ~loc fn_expr
         [(Nolabel, Exp.constraint_ ~loc schema_expr schema_type)])
      schema_type
  | txt when isSchemaAttributeName txt ->
    fail loc ("Unsupported schema attribute: \"@" ^ txt ^ "\"")
  | _ -> schema_expr

type nullability = Unset | Null | Nullable

let getNullability ~loc ptyp_attributes =
  match
    ( getAttributeByName ptyp_attributes "s.null",
      getAttributeByName ptyp_attributes "s.nullable" )
  with
  | Ok None, Ok None -> Unset
  | Ok (Some _), Ok None -> Null
  | Ok None, Ok (Some _) -> Nullable
  | Ok (Some _), Ok (Some _) ->
    fail loc
      "Attributes @s.null and @s.nullable are not supported at the same time"
  | _, Error s | Error s, _ -> fail loc s

(* Serde writes `None` as `null` and reads only `null` back as one, so that is
   what `option<_>` means wherever it stands. *)
let optionFactoryExpression ~loc = function
  | Unset | Null -> [%expr S.nullAsOption]
  | Nullable -> [%expr S.nullableAsOption]

(* What @s.default falls back over. Serde's `#[serde(default)]` fills in a
   missing value only and still rejects `null`, unless @s.null/@s.nullable say
   otherwise. *)
let defaultFactoryExpression ~loc = function
  | Unset -> [%expr S.option]
  | Null -> [%expr S.nullAsOption]
  | Nullable -> [%expr S.nullableAsOption]

let hasAttribute attributes name =
  match getAttributeByName attributes name with
  | Ok (Some _) -> true
  | _ -> false

let stringExpr s = Exp.constant (Pconst_string (s, Location.none, None))

let intExpr i = Exp.constant (Pconst_integer (string_of_int i, None))

(* For the S members metaquot can't parse because their names are OCaml
   keywords: `object`, `to`. *)
let sKeyword name = Exp.ident (mknoloc (Longident.Ldot (Lident "S", name)))

(* Serde's `Option` field: a missing key and `null` both read as `None`, ahead
   of whatever the item makes of them, and `None` writes as `null`. The null arm
   goes first because a union encodes through its first member that fits;
   S.option keeps the missing key ahead of an item that reads one itself. *)
let missingOrNullAsNone ~loc item_schema =
  let null_as_none =
    Exp.apply (sKeyword "to")
      [(Nolabel, [%expr S.literal Null.null]); (Nolabel, [%expr S.literal None])]
  in
  [%expr S.union [|[%e null_as_none]; S.option [%e item_schema]|]]

let objectSchema ~loc body =
  Exp.apply (sKeyword "object")
    [ ( Nolabel,
        uncurriedFun ~loc ~arity:1 [%expr fun (s : S.Object.s) -> [%e body]] );
    ]

let tupleSchema ~loc body =
  [%expr
    S.tuple
      [%e uncurriedFun ~loc ~arity:1 [%expr fun (s : S.Tuple.s) -> [%e body]]]]

(* Serde's default enum representation: `{"Variant": payload}`. The object has
   to hold that one key and nothing else, as serde_json demands. `build` gets
   the function that reads the key with a schema. *)
let externallyTagged ~loc ~key build =
  [%expr
    S.strict
      [%e
        objectSchema ~loc
          (build (fun payload_schema ->
               [%expr s.field [%e stringExpr key] [%e payload_schema]]))]]

type variantRepresentation =
  | External
  | Internal of expression (* @tag("...") *)
  | Untagged (* @unboxed *)

let getVariantRepresentation ~loc attributes =
  match
    (getAttributeByName attributes "tag", getAttributeByName attributes "unboxed")
  with
  | Ok (Some attribute), _ -> Internal (getExpressionFromPayload attribute)
  | Ok None, Ok (Some _) -> Untagged
  | Ok None, Ok None -> External
  | Error s, _ | _, Error s -> fail loc s

(* The key Serde writes for a constructor: its name, or the `@as` that renames
   it, the way `#[serde(rename = "...")]` does. *)
let getConstructorKey {pcd_name = {txt; loc}; pcd_attributes} =
  match getAttributeByName pcd_attributes "as" with
  | Ok None -> txt
  | Ok (Some attribute) -> (
    match (getExpressionFromPayload attribute).pexp_desc with
    | Pexp_constant (Pconst_string (key, _, _)) -> key
    | _ ->
      fail loc
        "The @as of a constructor with a payload must be a string - it names \
         the constructor's key in the JSON object")
  | Error s -> fail loc s

let rec generateConstrSchemaExpression {Location.txt = identifier; loc}
    type_args option_factory_expression =
  let open Longident in
  match (identifier, type_args) with
  | Lident "string", _ -> [%expr S.string]
  | Lident "int", _ -> [%expr S.int]
  | Lident "int64", _ -> fail loc "Can't generate schema for `int64` type"
  | Lident "float", _ -> [%expr S.float]
  | Lident "bigint", _ -> [%expr S.bigint]
  | Lident "bool", _ -> [%expr S.bool]
  | Lident "unit", _ -> [%expr S.nullAsUnit]
  | Lident "unknown", _ -> [%expr S.unknown]
  | Ldot (Lident "S", "never"), _ -> [%expr S.never]
  (* The format schemas and stdlib aliases are named identically as a type and
     as a schema, so `S.email` resolves to itself rather than to the
     `S.emailSchema` the generic `Ldot` case below would build. *)
  | Ldot (Lident "S", ("integer" | "date" | "json" | "jsonString" | "blob" | "formData"
     | "file" | "env" | "urlSearchParams" | "queryString"
     | "isoDateTime" | "utcDateTime" | "port" | "email" | "uuid" | "uuidv4"
     | "uuidv6" | "uuidv7" | "cuid" | "cuid2" | "ulid" | "ksuid" | "xid"
     | "nanoid" | "e164" | "mac" | "hex" | "base64" | "base64url" | "uri"
     | "httpUrl" | "url" | "isoDate" | "isoTime" | "duration" | "hostname"
     | "idnHostname" | "ipv4" | "ipv6" | "cidrv4" | "cidrv6"
     | "uriReference" | "uriTemplate" | "iri" | "iriReference" | "idnEmail"
     | "jsonPointer" | "relativeJsonPointer")) as schema_ident, _ ->
    Exp.ident (mknoloc schema_ident)
  | Ldot (Lident "S", "nonEmpty"), [item_type] ->
    [%expr S.nonEmpty [%e generateCoreTypeSchemaExpression item_type]]
  | Ldot (Ldot (Lident "Js", "Json"), "t"), _ | Ldot (Lident "JSON", "t"), _ ->
    [%expr S.json]
  | Lident "array", [item_type] ->
    [%expr S.array [%e generateCoreTypeSchemaExpression item_type]]
  | Lident "list", [item_type] ->
    [%expr S.list [%e generateCoreTypeSchemaExpression item_type]]
  | Lident "option", [item_type] ->
    [%expr
      [%e option_factory_expression]
        [%e generateCoreTypeSchemaExpression item_type]]
  | Lident "null", [item_type] ->
    [%expr S.null [%e generateCoreTypeSchemaExpression item_type]]
  | Ldot (Ldot (Lident "Js", "Nullable"), "t"), [item_type]
  | Ldot (Lident "Nullable", "t"), [item_type]
  | Ldot (Lident "Js", "nullable"), [item_type] ->
    [%expr S.nullable [%e generateCoreTypeSchemaExpression item_type]]
  | Lident "dict", [item_type]
  | Ldot (Ldot (Lident "Js", "Dict"), "t"), [item_type]
  | Ldot (Lident "Dict", "t"), [item_type] ->
    [%expr S.dict [%e generateCoreTypeSchemaExpression item_type]]
  | Lident s, [] -> makeIdentExpr (generateSchemaName s)
  | Lident s, [arg] ->
    Exp.apply (makeIdentExpr (generateSchemaName s))
      [(Nolabel, generateCoreTypeSchemaExpression arg)]
  | Lident _, _ -> fail loc "Parametrized types with more than one type parameter are not supported yet"
  | Ldot (left, right), [] ->
    Exp.ident (mknoloc (Ldot (left, generateSchemaName right)))
  | Ldot (left, right), [arg] ->
    Exp.apply
      (Exp.ident (mknoloc (Ldot (left, generateSchemaName right))))
      [(Nolabel, generateCoreTypeSchemaExpression arg)]
  | Ldot _, _ -> fail loc "Parametrized types with more than one type parameter are not supported yet"
  | Lapply (_, _), _ -> fail loc "Unsupported lapply syntax"

and polyvariantUnionItems row_fields =
  (* Returns the flattened list of S.union members for a set of poly-variant
     rows. An inherited row (`Rinherit`) may itself expand into several members
     (when it is an inline polyvariant), so we concat-map rather than map. *)
  row_fields
  |> List.map (fun {prf_desc; prf_loc = loc} ->
         (* The bool field of Rtag is the ampersand-conjunction flag,
            which ReScript polymorphic variants don't expose. *)
         match prf_desc with
         | Rtag ({txt = name}, _, []) ->
           [[%expr S.literal [%e Exp.variant name None]]]
         | Rtag ({txt = name}, _, [payload_core_type]) ->
           (* `#tag(t1, t2)` carries one tuple payload at the type level, which
              is what Serde writes for a tuple variant: `{"tag": [t1, t2]}`. *)
           [ externallyTagged ~loc ~key:name (fun readKey ->
                 Exp.variant name
                   (Some
                      (readKey
                         (generateCoreTypeSchemaExpression payload_core_type))))
           ]
         | Rtag _ ->
           fail loc
             "Polymorphic variant ampersand types (`Tag of t1 & t2) are not \
              supported"
         | Rinherit {ptyp_desc = Ptyp_variant (inherited_rows, _, _)} ->
           (* Inline inherited polyvariant: `[ [#a | #b] | #c ]`. Splice its
              rows directly into this union so we keep a single flat union. *)
           polyvariantUnionItems inherited_rows
         | Rinherit inherited_core_type ->
           (* Named inherited polyvariant: `[ base | #c ]`. Reuse the inherited
              type's schema (e.g. `baseSchema`) as a nested union member. The
              inherited schema's value type is narrower than the enclosing
              variant and `S.t` is invariant, so cast it to unify within the
              union. `S.castToAny` is a typed `%identity` (runtime no-op); the
              nested schema handles its own tags for both parsing and
              reversing. *)
           [ [%expr
               S.castToAny [%e generateCoreTypeSchemaExpression inherited_core_type]]
           ])
  |> List.concat

and generatePolyvariantSchemaExpression row_fields =
  match polyvariantUnionItems row_fields with
  | [item] -> item
  | union_items -> [%expr S.union [%e Exp.array union_items]]

and generateFieldSchemaExpression field =
  let {ptyp_desc; ptyp_loc = loc; ptyp_attributes} = field.core_type in
  let has = hasAttribute ptyp_attributes in
  let has_default = has "s.default" || has "s.defaultWith" in
  let option_item =
    match ptyp_desc with
    | Ptyp_constr ({txt = Longident.Lident "option"}, [item_type]) -> Some item_type
    | _ -> None
  in
  if field.is_optional then
    let schema_expression = generateCoreTypeSchemaExpression field.core_type in
    let optional_schema =
      if has_default then [%expr S.option [%e schema_expression]]
      else
        match (getNullability ~loc ptyp_attributes, option_item) with
        (* On `option<_>` the attribute already went into the item's schema,
           so the field itself gets the Serde reading below. *)
        | Null, None -> [%expr S.nullAsOption [%e schema_expression]]
        | Nullable, None -> [%expr S.nullableAsOption [%e schema_expression]]
        | _ -> missingOrNullAsNone ~loc schema_expression
    in
    [%expr Obj.magic [%e optional_schema]]
  else
    match option_item with
    | Some item_type
      when not (has "s.matches" || has "s.null" || has "s.nullable" || has_default)
      ->
      (* Built over the item rather than around the option's schema, so the
         attributes written on `option<_>` (@s.with, @s.meta) wrap the field's
         whole reading. *)
      generateCoreTypeSchemaExpression
        ~base:(missingOrNullAsNone ~loc (generateCoreTypeSchemaExpression item_type))
        field.core_type
    | _ -> generateCoreTypeSchemaExpression field.core_type

and generateVariantSchemaExpression ~representation constr_decls =
  let payloadCoreTypeToMatchesExpression core_type =
    [%expr s.matches [%e generateCoreTypeSchemaExpression core_type]]
  in
  (* The constructor over its runtime representation: what an @unboxed variant
     reads, and an @tag one. Serde's internal tagging agrees on an inline
     record; any other payload stays under `_0`, where Serde flattens a struct
     and refuses the rest. *)
  let runtimeShaped ~loc ~name pcd_args =
    let body =
      match pcd_args with
      | Pcstr_tuple [payload_core_type] ->
        Exp.construct (lid name)
          (Some (payloadCoreTypeToMatchesExpression payload_core_type))
      | Pcstr_tuple payload_core_types ->
        Exp.construct (lid name)
          (Some
             (Exp.tuple
                (payload_core_types |> List.map payloadCoreTypeToMatchesExpression)))
      | Pcstr_record label_declarations ->
        Exp.construct (lid name)
          (Some
             (Exp.record
                (label_declarations
                |> List.map (fun label_declaration ->
                       let field = parseLabelDeclaration label_declaration in
                       ( lid field.name,
                         [%expr s.matches [%e generateFieldSchemaExpression field]]
                       )))
                None))
    in
    [%expr
      S.schema
        [%e uncurriedFun ~loc ~arity:1 [%expr fun (s : S.Schema.s) -> [%e body]]]]
  in
  let externallyTaggedPayload ~loc ~name ~key pcd_args =
    externallyTagged ~loc ~key (fun readKey ->
        match pcd_args with
        | Pcstr_tuple [payload_core_type] ->
          Exp.construct (lid name)
            (Some (readKey (generateCoreTypeSchemaExpression payload_core_type)))
        | Pcstr_tuple payload_core_types ->
          let items =
            payload_core_types
            |> List.mapi (fun idx payload_core_type ->
                   [%expr
                     s.item [%e intExpr idx]
                       [%e generateCoreTypeSchemaExpression payload_core_type]])
          in
          readKey
            (tupleSchema ~loc (Exp.construct (lid name) (Some (Exp.tuple items))))
        | Pcstr_record label_declarations ->
          let field_expressions =
            label_declarations
            |> List.map (fun label_declaration ->
                   let field = parseLabelDeclaration label_declaration in
                   ( lid field.name,
                     [%expr
                       s.field
                         [%e stringExpr field.runtime_name]
                         [%e generateFieldSchemaExpression field]] ))
          in
          readKey
            (objectSchema ~loc
               (Exp.construct (lid name) (Some (Exp.record field_expressions None)))))
  in
  let spread_schemas = ref [] in
  let union_items =
    constr_decls
    |> List.filter_map (fun ({pcd_name = {txt = name; loc}; pcd_args} as decl) ->
           if name = "..." then (
             match pcd_args with
             | Pcstr_tuple [spread_type] ->
               let spread_schema =
                 generateCoreTypeSchemaExpression spread_type
               in
               spread_schemas := spread_schema :: !spread_schemas;
               None
             | _ -> fail loc "Unsupported variant spread syntax")
           else
             let constructor = Exp.construct (lid name) None in
             Some
               (match (representation, pcd_args) with
               | (External | Untagged), Pcstr_tuple [] ->
                 [%expr S.literal [%e constructor]]
               (* Serde's internally tagged unit variant: `{"tag": "Variant"}`.
                  The constructor is its own tag value, `@as` included. *)
               | Internal tag, Pcstr_tuple [] ->
                 objectSchema ~loc
                   [%expr
                     s.tag [%e tag] [%e constructor];
                     [%e constructor]]
               | External, _ ->
                 externallyTaggedPayload ~loc ~name ~key:(getConstructorKey decl)
                   pcd_args
               | (Internal _ | Untagged), _ -> runtimeShaped ~loc ~name pcd_args))
  in
  let spread_schemas = List.rev !spread_schemas in
  if spread_schemas = [] then
    match union_items with
    | [item] -> item
    | _ -> [%expr S.union [%e Exp.array union_items]]
  else
    (* For variant spreads, extract anyOf items from each spread schema's
       AnyOf tag and concatenate with the local items. S.t<'value> is a
       tagged variant (see S.resi), so we can pattern-match the schema
       directly without S.tagged. *)
    let spread_items_exprs =
      spread_schemas
      |> List.map (fun spread_schema ->
             [%expr
               Obj.magic (
                 if (S.untag [%e spread_schema]).tag == S.AnyOf then
                   Obj.magic ((S.untag [%e spread_schema]).anyOf)
                 else
                   [| Obj.magic [%e spread_schema] |]
               )])
    in
    let local_items = Exp.array union_items in
    let all_items =
      List.fold_left
        (fun acc spread_expr ->
          [%expr Stdlib.Array.concat [%e acc] [%e spread_expr]])
        local_items spread_items_exprs
    in
    [%expr S.union (Obj.magic [%e all_items])]

and generateObjectSchema fields =
  let field_expressions =
    fields
    |> List.map (fun field ->
           ( lid field.name,
             [%expr s.matches [%e generateFieldSchemaExpression field]] ))
  in
  let body =
    Exp.extension
      ( mkloc "obj" Location.none,
        PStr [Str.eval (Exp.record field_expressions None)] )
  in
  [%expr
    S.schema
      [%e
        uncurriedFun ~loc:Location.none ~arity:1
          [%expr fun (s : S.Schema.s) -> [%e body]]]]

and generateRecordSchema type_name fields =
  let field_expressions =
    fields
    |> List.map (fun field ->
           ( lid field.name,
             [%expr s.matches [%e generateFieldSchemaExpression field]] ))
  in
  let record_expr = Exp.record field_expressions None in
  let body =
    match field_expressions with
    | [] ->
      Exp.constraint_ record_expr (Typ.constr (lid type_name) [])
    | _ -> record_expr
  in
  [%expr
    S.schema
      [%e
        uncurriedFun ~loc:Location.none ~arity:1
          [%expr fun (s : S.Schema.s) -> [%e body]]]]

and generateRecordSchemaWithSpreads spread_types regular_fields =
  let field_obj_expressions =
    regular_fields
    |> List.map (fun field ->
           ( lid field.runtime_name,
             [%expr s.matches [%e generateFieldSchemaExpression field]] ))
  in
  let fields_obj =
    Exp.extension
      ( mkloc "obj" Location.none,
        PStr [Str.eval (Exp.record field_obj_expressions None)] )
  in
  let spread_schema_exprs =
    spread_types |> List.map generateCoreTypeSchemaExpression
  in
  let raw_str s =
    Exp.extension
      ( mkloc "raw" Location.none,
        PStr
          [Str.eval (Exp.constant (Pconst_string (s, Location.none, None)))] )
  in
  let spread_property_args =
    spread_schema_exprs
    |> List.map (fun spread_schema ->
           [%expr Obj.magic ((S.untag [%e spread_schema]).properties)])
  in
  (* Use the regular-fields object as the assign target so spread-property
     keys get folded into it without mutating the spread schemas' own
     properties dicts. ReScript's type system already forbids overlapping
     keys between spread types and explicit fields, so the Object.assign
     overwrite direction (sources overwrite target) is unobservable here. *)
  let target_arg, s_pat =
    if regular_fields = [] then
      ( [%expr Obj.magic [%e raw_str "{}"]],
        [%pat? (_s : S.Schema.s)] )
    else ([%expr Obj.magic [%e fields_obj]], [%pat? (s : S.Schema.s)])
  in
  let assign_call =
    [%expr
      Stdlib.Object.assignMany
        [%e target_arg]
        [%e Exp.array spread_property_args]]
  in
  [%expr
    S.schema
      [%e
        uncurriedFun ~loc:Location.none ~arity:1
          (Exp.fun_ Nolabel None s_pat [%expr Obj.magic [%e assign_call]])]]

(* `base` stands in for the schema the type itself would get, under the same
   attributes - generateFieldSchemaExpression builds a field's reading of an
   option through it. *)
and generateCoreTypeSchemaExpression ?base core_type =
  let {ptyp_desc; ptyp_loc; ptyp_attributes} = core_type in
  let customSchemaExpression = getAttributeByName ptyp_attributes "s.matches" in
  let nullability = getNullability ~loc:ptyp_loc ptyp_attributes in
  let schema_expression =
    match (base, customSchemaExpression) with
    | Some base, _ -> base
    | None, Ok None -> (
      match ptyp_desc with
      | Ptyp_any -> fail ptyp_loc "Can't generate schema for `any` type"
      | Ptyp_arrow (_, _, _) ->
        fail ptyp_loc "Can't generate schema for function type"
      | Ptyp_package _ -> fail ptyp_loc "Can't generate schema for module type"
      | Ptyp_tuple tuple_types ->
        let body =
          Exp.tuple
            (tuple_types
            |> List.map (fun tuple_type ->
                   [%expr
                     s.matches [%e generateCoreTypeSchemaExpression tuple_type]]))
        in
        [%expr
          S.schema
            [%e
              uncurriedFun ~loc:ptyp_loc ~arity:1
                [%expr
                  fun (s : S.Schema.s) : [%t core_type] -> [%e body]]]]
      | Ptyp_var s -> makeIdentExpr (generateTypeVarSchemaName s)
      | Ptyp_constr (constr, type_args) ->
        generateConstrSchemaExpression constr type_args
          (optionFactoryExpression ~loc:ptyp_loc nullability)
      | Ptyp_variant (row_fields, _, _) ->
        generatePolyvariantSchemaExpression row_fields
      | Ptyp_object (object_fields, Closed) ->
        object_fields |> List.map parseObjectField |> generateObjectSchema
      | _ -> fail ptyp_loc "Unsupported type")
    | None, Ok (Some attribute) -> getExpressionFromPayload attribute
    | None, Error s -> fail ptyp_loc s
  in
  let default_factory_expression =
    defaultFactoryExpression ~loc:ptyp_loc nullability
  in
  let handle_attribute schema_expr ({attr_name = {Location.txt}} as attribute) =
    match txt with
    | "s.matches" | "s.null" | "s.nullable" -> schema_expr (* handled above *)
    | "s.default" ->
      let default_value = getExpressionFromPayload attribute in
      [%expr
        S.Option.getOr
          ([%e default_factory_expression] [%e schema_expr])
          [%e default_value]]
    | "s.defaultWith" ->
      let default_fn = getExpressionFromPayload attribute in
      [%expr
        S.Option.getOrWith
          ([%e default_factory_expression] [%e schema_expr])
          [%e default_fn]]
    | _ ->
      applySchemaAttribute ~loc:ptyp_loc
        ~value_type:(stripSchemaAttributes core_type)
        schema_expr attribute
  in
  List.fold_left handle_attribute schema_expression ptyp_attributes

let generateTypeDeclarationSchemaExpression type_declaration =
  (* let {ptype_name = {txt = type_name}} = type_declaration in *)
  match type_declaration with
  | {ptype_loc; ptype_kind = Ptype_abstract; ptype_manifest = None} ->
    fail ptype_loc "Can't generate schema for abstract type"
  | {ptype_manifest = Some manifest; _} ->
    manifest |> generateCoreTypeSchemaExpression
  | {ptype_kind = Ptype_variant decls; ptype_attributes; ptype_loc} ->
    generateVariantSchemaExpression
      ~representation:(getVariantRepresentation ~loc:ptype_loc ptype_attributes)
      decls
  | {ptype_name = {txt = type_name}; ptype_kind = Ptype_record label_declarations; _} ->
    let spread_types, regular_lds =
      List.partition
        (fun {pld_name = {txt}} -> txt = "...")
        label_declarations
    in
    if spread_types = [] then
      generateRecordSchema type_name
        (regular_lds |> List.map parseLabelDeclaration)
    else
      let spread_core_types =
        spread_types |> List.map (fun {pld_type} -> pld_type)
      in
      let regular_fields = regular_lds |> List.map parseLabelDeclaration in
      generateRecordSchemaWithSpreads spread_core_types regular_fields
  | {ptype_loc; _} -> fail ptype_loc "Unsupported type declaration"

let generateSchemaValueBinding type_name ptype_params schema_expr =
  let schema_name_pat = Pat.var (mknoloc (generateSchemaName type_name)) in
  match ptype_params with
  | [] ->
    Vb.mk schema_name_pat
      (Exp.constraint_ schema_expr
         [%type: [%t Typ.constr (lid type_name) []] S.t])
  | [(ct, _)] -> (
    match ct.ptyp_desc with
    | Ptyp_var s ->
      let param_pat =
        Pat.constraint_
          (Pat.var (mknoloc (generateTypeVarSchemaName s)))
          [%type: [%t Typ.var s] S.t]
      in
      let constrained =
        Exp.constraint_ schema_expr
          [%type: [%t Typ.constr (lid type_name) [Typ.var s]] S.t]
      in
      let loc = ct.ptyp_loc in
      Vb.mk schema_name_pat
        (uncurriedFun ~loc ~arity:1
           [%expr fun [%p param_pat] -> [%e constrained]])
    | _ ->
      fail ct.ptyp_loc "Expected a type variable as type parameter")
  | _ ->
    fail (fst (List.hd ptype_params)).ptyp_loc
      "Parametrized types with more than one type parameter are not supported yet"

(* Applies type-level @s.* attributes inside the body, so a recursive wrapper
   registers the transformed schema - the one self-references resolve to -
   rather than the bare one. *)
let generateDeclarationSchemaExpression type_declaration =
  let {ptype_attributes; ptype_name = {txt = type_name}; ptype_loc; ptype_params}
      =
    type_declaration
  in
  List.fold_left
    (applySchemaAttribute ~loc:ptype_loc
       ~value_type:(Typ.constr (lid type_name) (List.map fst ptype_params)))
    (generateTypeDeclarationSchemaExpression type_declaration)
    ptype_attributes

let hasSchemaAttribute {ptype_attributes; ptype_loc} =
  match getAttributeByName ptype_attributes "schema" with
  | Ok None -> false
  | Ok (Some _) -> true
  | Error err -> fail ptype_loc err

let mapTypeDeclaration type_declaration =
  if hasSchemaAttribute type_declaration then
    let {ptype_name = {txt = type_name}; ptype_params} = type_declaration in
    [ generateSchemaValueBinding type_name ptype_params
        (generateDeclarationSchemaExpression type_declaration) ]
  else []

(* The placeholder is bound under the exact name a self-reference compiles to,
   so recursion resolves by shadowing - including hand-written references in
   @s.matches payloads. *)
let wrapRecursive {ptype_name = {txt = type_name}; ptype_loc} body =
  let param_pat =
    Pat.constraint_
      (Pat.var (mknoloc (generateSchemaName type_name)))
      [%type: [%t Typ.constr (lid type_name) []] S.t]
  in
  [%expr
    S.recursive
      [%e Exp.constant (Pconst_string (type_name, Location.none, None))]
      [%e
        uncurriedFun ~loc:ptype_loc ~arity:1
          (Exp.fun_ Nolabel None param_pat body)]]

(* Which of `members` the generated expression mentions. Scans the emitted code
   rather than the source type, so hand-written @s.matches references count as
   dependencies and a field fully replaced by @s.matches contributes none. *)
let referencedMembers members expr =
  let found = ref [] in
  let scanner =
    object
      inherit Ast_traverse.iter as super

      method! expression e =
        (match e.pexp_desc with
        | Pexp_ident {txt = Longident.Lident ident} ->
          members
          |> List.iter (fun member ->
                 if generateSchemaName member = ident && not (List.mem member !found)
                 then found := member :: !found)
        | _ -> ());
        super#expression e
    end
  in
  scanner#expression expr;
  !found

(* Only the outermost S.recursive call carries $defs, so each entry point of a
   mutual group re-expands the whole group inside its own callback - nested
   calls register into the shared $defs. One expansion per entry point is the
   cost. *)
let mapRecursiveTypeDeclarations decls =
  match decls |> List.filter hasSchemaAttribute with
  | [] -> []
  | annotated ->
    let members = annotated |> List.map (fun d -> d.ptype_name.txt) in
    (* Bodies are generated once and reused by every copy in a mutual expansion. *)
    let generated =
      annotated
      |> List.map (fun d ->
             (d.ptype_name.txt, (d, generateDeclarationSchemaExpression d)))
    in
    let declOf name = fst (List.assoc name generated) in
    let bodyOf name = snd (List.assoc name generated) in
    let directDepsByName =
      members
      |> List.map (fun name -> (name, referencedMembers members (bodyOf name)))
    in
    let directDeps name = List.assoc name directDepsByName in
    (* Transitive deps of `name`, not crossing `blocked` members - a reference
       to a member already bound in scope forces no expansion behind it. *)
    let reachableAvoiding blocked name =
      let rec collect visited = function
        | [] -> visited
        | n :: rest ->
          if List.mem n visited || List.mem n blocked then collect visited rest
          else collect (n :: visited) (directDeps n @ rest)
      in
      collect [] (directDeps name)
    in
    let reachableByName =
      members |> List.map (fun name -> (name, reachableAvoiding [] name))
    in
    let reachable name = List.assoc name reachableByName in
    let sameGroup a b =
      a = b || (List.mem b (reachable a) && List.mem a (reachable b))
    in
    let groups =
      List.fold_left
        (fun acc name ->
          if acc |> List.exists (List.mem name) then acc
          else acc @ [members |> List.filter (sameGroup name)])
        [] members
    in
    (* Emit groups dependencies-first: a member that merely *uses* another one
       binds against its already-emitted top-level schema. *)
    let rec topological emitted remaining =
      match remaining with
      | [] -> []
      | _ -> (
        let ready, blocked =
          remaining
          |> List.partition (fun group ->
                 group
                 |> List.for_all (fun name ->
                        reachable name
                        |> List.for_all (fun dep ->
                               List.mem dep group || List.mem dep emitted)))
        in
        match ready with
        (* Unreachable - groups are collapsed cycles, so the rest is a DAG.
           Fail loudly rather than emit unresolvable bindings. *)
        | [] ->
          fail (declOf (List.hd (List.hd remaining))).ptype_loc
            "sury-ppx internal error: failed to order recursive type groups. \
             Please report the issue to https://github.com/DZakh/sury/issues"
        | _ -> ready @ topological (emitted @ List.concat ready) blocked)
    in
    (* A dep that other pending deps need goes later: later deps become *outer*
       `let`s, in scope for earlier ones, so an expansion reuses the sibling
       binding instead of duplicating it (and its $defs entry). Cyclic siblings
       stay put; nested expansion resolves them. *)
    let rec orderDeps ~blocked deps =
      match deps with
      | [] -> []
      | _ -> (
        let inner, rest =
          deps
          |> List.partition (fun d ->
                 deps
                 |> List.for_all (fun other ->
                        other = d
                        || not (List.mem d (reachableAvoiding blocked other))))
        in
        match inner with
        | [] -> deps
        | _ -> inner @ orderDeps ~blocked rest)
    in
    let rec expand ~group ~in_scope name =
      let in_scope = name :: in_scope in
      (* Only direct deps get a binding - an indirect one is bound by the
         expansion that references it; here it would be an unused `let`. *)
      let deps =
        directDeps name
        |> List.filter (fun dep ->
               List.mem dep group && not (List.mem dep in_scope))
        |> orderDeps ~blocked:in_scope
      in
      let rec bind body = function
        | [] -> body
        | dep :: outer ->
          bind
            (Exp.let_ Nonrecursive
               [ Vb.mk
                   (Pat.var (mknoloc (generateSchemaName dep)))
                   (expand ~group ~in_scope:(outer @ in_scope) dep) ]
               body)
            outer
      in
      wrapRecursive (declOf name) (bind (bodyOf name) deps)
    in
    topological [] groups
    |> List.map (fun group ->
           let is_recursive =
             match group with
             | [name] -> List.mem name (reachable name)
             | _ -> true
           in
           group
           |> List.map (fun name ->
                  let {ptype_loc; ptype_params} = declOf name in
                  let schema_expr =
                    if not is_recursive then bodyOf name
                    else if ptype_params <> [] then
                      fail ptype_loc
                        "Recursive parametrized types are not supported yet"
                    else expand ~group ~in_scope:[] name
                  in
                  Str.value Nonrecursive
                    [generateSchemaValueBinding name ptype_params schema_expr]))
    |> List.concat

let mapStructureItem mapper ({pstr_desc} as structure_item) =
  match pstr_desc with
  | Pstr_type (Recursive, decls) ->
    mapper#structure_item structure_item :: mapRecursiveTypeDeclarations decls
  | Pstr_type (Nonrecursive, decls) -> (
    let value_bindings = decls |> List.map mapTypeDeclaration |> List.concat in
    [mapper#structure_item structure_item]
    @
    match value_bindings with
    | [] -> []
    | _ -> [Str.value Nonrecursive value_bindings])
  | _ -> [mapper#structure_item structure_item]
