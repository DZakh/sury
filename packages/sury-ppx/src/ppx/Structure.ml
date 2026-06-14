open Ppxlib
open Parsetree
open Ast_helper
open Util

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
  | Lident "unit", _ -> [%expr S.unit]
  | Lident "unknown", _ -> [%expr S.unknown]
  | Ldot (Lident "S", "never"), _ -> [%expr S.never]
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
  | Lident s, _ -> makeIdentExpr (generateSchemaName s)
  | Ldot (left, right), _ ->
    Exp.ident (mknoloc (Ldot (left, generateSchemaName right)))
  | Lapply (_, _), _ -> fail loc "Unsupported lapply syntax"

and generatePolyvariantSchemaExpression row_fields =
  let union_items =
    row_fields
    |> List.map (fun {prf_desc} ->
           let name =
             match prf_desc with
             | Rtag ({txt}, _, _) -> txt
             | _ -> failwith "Unsupported polymorphic variant constructor"
           in
           [%expr S.literal [%e Exp.variant name None]])
  in
  match union_items with
  | [item] -> item
  | _ -> [%expr S.union [%e Exp.array union_items]]

and generateFieldSchemaExpression field =
  let schema_expression = generateCoreTypeSchemaExpression field.core_type in
  if field.is_optional then [%expr Obj.magic (S.option [%e schema_expression])]
  else schema_expression

and generateVariantSchemaExpression constr_decls =
  let payloadCoreTypeToMatchesExpression core_type =
    [%expr s.matches [%e generateCoreTypeSchemaExpression core_type]]
  in
  let spread_schemas = ref [] in
  let union_items =
    constr_decls
    |> List.filter_map
         (fun {pcd_name = {txt = name; loc}; pcd_args} ->
           if name = "..." then (
             match pcd_args with
             | Pcstr_tuple [spread_type] ->
               let spread_schema =
                 generateCoreTypeSchemaExpression spread_type
               in
               spread_schemas := spread_schema :: !spread_schemas;
               None
             | _ -> fail loc "Unsupported variant spread syntax"
           ) else
             Some
               (match pcd_args with
               | Pcstr_tuple [] ->
                 [%expr S.literal [%e Exp.construct (lid name) None]]
               | Pcstr_tuple payload_core_types ->
                 [%expr
                   S.schema
                     (Obj.magic (fun (s : S.Schema.s) ->
                          [%e
                            Exp.construct (lid name)
                              (Some
                                 (match payload_core_types with
                                 | [payload_core_type] ->
                                   payloadCoreTypeToMatchesExpression
                                     payload_core_type
                                 | payload_core_types ->
                                   Exp.tuple
                                     (payload_core_types
                                     |> List.map
                                          payloadCoreTypeToMatchesExpression
                                     )))]))]
               | Pcstr_record label_declarations ->
                 let fields =
                   label_declarations |> List.map parseLabelDeclaration
                 in
                 let field_expressions =
                   fields
                   |> List.map (fun field ->
                          let schema_expression =
                            generateFieldSchemaExpression field
                          in
                          ( lid field.name,
                            [%expr s.matches [%e schema_expression]] ))
                 in
                 [%expr
                   S.schema
                     (Obj.magic (fun (s : S.Schema.s) ->
                          [%e
                            Exp.construct (lid name)
                              (Some (Exp.record field_expressions None))]))]))
  in
  let spread_schemas = List.rev !spread_schemas in
  if spread_schemas = [] then
    match union_items with
    | [item] -> item
    | _ -> [%expr S.union [%e Exp.array union_items]]
  else
    (* For variant spreads, extract anyOf items from spread schemas and
       concatenate with the local items *)
    let spread_items_exprs =
      spread_schemas
      |> List.map (fun spread_schema ->
             [%expr
               match S.tagged (Obj.magic [%e spread_schema]) with
               | Union {anyOf} -> anyOf
               | _ -> [| Obj.magic [%e spread_schema] |]])
    in
    let local_items = Exp.array union_items in
    let all_items =
      List.fold_left
        (fun acc spread_expr ->
          [%expr Js.Array2.concat [%e acc] [%e spread_expr]])
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
  (* Use Obj.magic to cast to uncurried function in case of uncurried mode *)
  [%expr
    S.schema
      (Obj.magic (fun (s : S.Schema.s) ->
           [%e
             Exp.extension
               ( mkloc "obj" Location.none,
                 PStr [Str.eval (Exp.record field_expressions None)] )]))]

and generateRecordSchema fields =
  let field_expressions =
    fields
    |> List.map (fun field ->
           ( lid field.name,
             [%expr s.matches [%e generateFieldSchemaExpression field]] ))
  in
  (* Use Obj.magic to cast to uncurried function in case of uncurried mode *)
  [%expr
    S.schema
      (Obj.magic (fun (s : S.Schema.s) ->
           [%e Exp.record field_expressions None]))]

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
  (* Chain Object.assign 2-arg calls:
     Object.assign(Object.assign({}, sp1.properties), sp2.properties, ..., fields_obj). *)
  let spread_property_args =
    spread_schema_exprs
    |> List.map (fun spread_schema ->
           [%expr Obj.magic ((S.untag [%e spread_schema]).properties)])
  in
  let regular_args_list =
    if regular_fields = [] then [] else [[%expr Obj.magic [%e fields_obj]]]
  in
  let sources = spread_property_args @ regular_args_list in
  let base = [%expr Obj.magic [%e raw_str "{}"]] in
  (* Build nested 2-arg Object.assign calls: assign(assign(assign({}, sp1), sp2), fields). *)
  let merged =
    List.fold_left
      (fun acc src ->
        Exp.apply
          [%expr (Obj.magic [%e raw_str "Object.assign"])]
          [(Nolabel, acc); (Nolabel, src)])
      base sources
  in
  [%expr
    S.schema (Obj.magic (fun (s : S.Schema.s) -> Obj.magic [%e merged]))]

and generateCoreTypeSchemaExpression core_type =
  let {ptyp_desc; ptyp_loc; ptyp_attributes} = core_type in
  let customSchemaExpression = getAttributeByName ptyp_attributes "s.matches" in
  let option_factory_expression =
    match
      ( getAttributeByName ptyp_attributes "s.null",
        getAttributeByName ptyp_attributes "s.nullable" )
    with
    | Ok None, Ok None -> [%expr S.option]
    | Ok (Some _), Ok None -> [%expr S.nullAsOption]
    | Ok None, Ok (Some _) -> [%expr S.nullableAsOption]
    | Ok (Some _), Ok (Some _) ->
      fail ptyp_loc
        "Attributes @s.null and @s.nullable are not supported at the same time"
    | _, Error s | Error s, _ -> fail ptyp_loc s
  in
  let schema_expression =
    match customSchemaExpression with
    | Ok None -> (
      match ptyp_desc with
      | Ptyp_any -> fail ptyp_loc "Can't generate schema for `any` type"
      | Ptyp_arrow (_, _, _) ->
        fail ptyp_loc "Can't generate schema for function type"
      | Ptyp_package _ -> fail ptyp_loc "Can't generate schema for module type"
      | Ptyp_tuple tuple_types ->
        [%expr
          S.schema
            (Obj.magic (fun (s : S.Schema.s) : [%t core_type] ->
                 [%e
                   Exp.tuple
                     (tuple_types
                     |> List.map (fun tuple_type ->
                            [%expr
                              s.matches
                                [%e generateCoreTypeSchemaExpression tuple_type]])
                     )]))]
      | Ptyp_var s -> makeIdentExpr (generateSchemaName s)
      | Ptyp_constr (constr, type_args) ->
        generateConstrSchemaExpression constr type_args
          option_factory_expression
      | Ptyp_variant (row_fields, _, _) ->
        generatePolyvariantSchemaExpression row_fields
      | Ptyp_object (object_fields, Closed) ->
        object_fields |> List.map parseObjectField |> generateObjectSchema
      | _ -> fail ptyp_loc "Unsupported type")
    | Ok (Some attribute) -> getExpressionFromPayload attribute
    | Error s -> fail ptyp_loc s
  in
  let handle_attribute schema_expr ({attr_name = {Location.txt}} as attribute) =
    match txt with
    | "s.matches" | "s.null" | "s.nullable" -> schema_expr (* handled above *)
    | "s.default" ->
      let default_value = getExpressionFromPayload attribute in
      [%expr
        S.Option.getOr
          ([%e option_factory_expression] [%e schema_expr])
          [%e default_value]]
    | "s.defaultWith" ->
      let default_fn = getExpressionFromPayload attribute in
      [%expr
        S.Option.getOrWith
          ([%e option_factory_expression] [%e schema_expr])
          [%e default_fn]]
    | "s.strict" -> [%expr S.strict [%e schema_expr]]
    | "s.strip" -> [%expr S.strip [%e schema_expr]]
    | "s.deepStrict" -> [%expr S.deepStrict [%e schema_expr]]
    | "s.deepStrip" -> [%expr S.deepStrip [%e schema_expr]]
    | "s.noValidation" -> [%expr S.noValidation [%e schema_expr]]
    | "s.meta" ->
      let meta_value = getExpressionFromPayload attribute in
      [%expr S.meta [%e schema_expr] [%e meta_value]]
    | txt when txt <> "" && String.length txt >= 2 && String.sub txt 0 2 = "s."
      ->
      fail ptyp_loc ("Unsupported schema attribute: \"@" ^ txt ^ "\"")
    | _ -> schema_expr
  in
  List.fold_left handle_attribute schema_expression ptyp_attributes

let generateTypeDeclarationSchemaExpression type_declaration =
  (* let {ptype_name = {txt = type_name}} = type_declaration in *)
  match type_declaration with
  | {ptype_loc; ptype_kind = Ptype_abstract; ptype_manifest = None} ->
    fail ptype_loc "Can't generate schema for abstract type"
  | {ptype_manifest = Some manifest; _} ->
    manifest |> generateCoreTypeSchemaExpression
  | {ptype_kind = Ptype_variant decls; _} ->
    generateVariantSchemaExpression decls
  | {ptype_kind = Ptype_record label_declarations; _} ->
    let spread_types, regular_lds =
      List.partition
        (fun {pld_name = {txt}} -> txt = "...")
        label_declarations
    in
    if spread_types = [] then
      regular_lds |> List.map parseLabelDeclaration |> generateRecordSchema
    else
      let spread_core_types =
        spread_types |> List.map (fun {pld_type} -> pld_type)
      in
      let regular_fields =
        regular_lds |> List.map parseLabelDeclaration
      in
      generateRecordSchemaWithSpreads spread_core_types regular_fields
  | {ptype_loc; _} -> fail ptype_loc "Unsupported type declaration"

let generateSchemaValueBinding type_name schema_expr =
  let schema_name_pat = Pat.var (mknoloc (generateSchemaName type_name)) in
  Vb.mk schema_name_pat
    (Exp.constraint_ schema_expr [%type: [%t Typ.constr (lid type_name) []] S.t])

let mapTypeDeclaration type_declaration =
  let {ptype_attributes; ptype_name = {txt = type_name}; ptype_loc} =
    type_declaration
  in
  match getAttributeByName ptype_attributes "schema" with
  | Ok None -> []
  | Error err -> fail ptype_loc err
  | Ok _ ->
    [
      generateSchemaValueBinding type_name
        (generateTypeDeclarationSchemaExpression type_declaration);
    ]

let mapStructureItem mapper ({pstr_desc} as structure_item) =
  match pstr_desc with
  | Pstr_type (rec_flag, decls) -> (
    let value_bindings = decls |> List.map mapTypeDeclaration |> List.concat in
    [mapper#structure_item structure_item]
    @
    match List.length value_bindings > 0 with
    | true -> [Str.value rec_flag value_bindings]
    | false -> [])
  | _ -> [mapper#structure_item structure_item]
