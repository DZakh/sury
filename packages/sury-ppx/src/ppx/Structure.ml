open Ppxlib
open Parsetree
open Ast_helper
open Util

let applySchemaAttribute ~loc schema_expr
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
  | txt when txt <> "" && String.length txt >= 2 && String.sub txt 0 2 = "s." ->
    fail loc ("Unsupported schema attribute: \"@" ^ txt ^ "\"")
  | _ -> schema_expr

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
  let payloadCoreTypeToMatchesExpression core_type =
    [%expr s.matches [%e generateCoreTypeSchemaExpression core_type]]
  in
  row_fields
  |> List.map (fun {prf_desc; prf_loc} ->
         (* The bool field of Rtag is the ampersand-conjunction flag,
            which ReScript polymorphic variants don't expose. *)
         match prf_desc with
         | Rtag ({txt = name}, _, []) ->
           [[%expr S.literal [%e Exp.variant name None]]]
         | Rtag ({txt = name}, _, [{ptyp_desc = Ptyp_tuple tuple_types}]) ->
           (* ReScript represents `#tag(t1, t2)` as a single tuple payload at
              the type level. Unfold it so the construction site uses flat
              args, mirroring how generateVariantSchemaExpression handles
              Pcstr_tuple multi-arg variants. *)
           let body =
             Exp.variant name
               (Some
                  (Exp.tuple
                     (tuple_types
                     |> List.map payloadCoreTypeToMatchesExpression)))
           in
           [ [%expr
               S.schema
                 [%e
                   uncurriedFun ~loc:prf_loc ~arity:1
                     [%expr fun (s : S.Schema.s) -> [%e body]]]]
           ]
         | Rtag ({txt = name}, _, [payload_core_type]) ->
           let body =
             Exp.variant name
               (Some (payloadCoreTypeToMatchesExpression payload_core_type))
           in
           [ [%expr
               S.schema
                 [%e
                   uncurriedFun ~loc:prf_loc ~arity:1
                     [%expr fun (s : S.Schema.s) -> [%e body]]]]
           ]
         | Rtag _ ->
           fail prf_loc
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
    |> List.filter_map (fun {pcd_name = {txt = name; loc}; pcd_args} ->
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
             Some
               (match pcd_args with
               | Pcstr_tuple [] ->
                 [%expr S.literal [%e Exp.construct (lid name) None]]
               | Pcstr_tuple payload_core_types ->
                 let body =
                   Exp.construct (lid name)
                     (Some
                        (match payload_core_types with
                        | [payload_core_type] ->
                          payloadCoreTypeToMatchesExpression payload_core_type
                        | payload_core_types ->
                          Exp.tuple
                            (payload_core_types
                            |> List.map payloadCoreTypeToMatchesExpression)))
                 in
                 [%expr
                   S.schema
                     [%e
                       uncurriedFun ~loc ~arity:1
                         [%expr fun (s : S.Schema.s) -> [%e body]]]]
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
                 let body =
                   Exp.construct (lid name)
                     (Some (Exp.record field_expressions None))
                 in
                 [%expr
                   S.schema
                     [%e
                       uncurriedFun ~loc ~arity:1
                         [%expr fun (s : S.Schema.s) -> [%e body]]]]))
  in
  let spread_schemas = List.rev !spread_schemas in
  if spread_schemas = [] then
    match union_items with
    | [item] -> item
    | _ -> [%expr S.union [%e Exp.array union_items]]
  else
    (* For variant spreads, extract anyOf items from each spread schema's
       Union tag and concatenate with the local items. S.t<'value> is a
       tagged variant (see S.resi), so we can pattern-match the schema
       directly without S.tagged. *)
    let spread_items_exprs =
      spread_schemas
      |> List.map (fun spread_schema ->
             [%expr
               Obj.magic (
                 if (S.untag [%e spread_schema]).tag == S.Union then
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
    | _ -> applySchemaAttribute ~loc:ptyp_loc schema_expr attribute
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

let mapTypeDeclaration type_declaration =
  let {ptype_attributes; ptype_name = {txt = type_name}; ptype_loc; ptype_params}
      =
    type_declaration
  in
  match getAttributeByName ptype_attributes "schema" with
  | Ok None -> []
  | Error err -> fail ptype_loc err
  | Ok _ ->
    let schema_expr =
      generateTypeDeclarationSchemaExpression type_declaration
    in
    let schema_expr =
      List.fold_left
        (applySchemaAttribute ~loc:ptype_loc)
        schema_expr ptype_attributes
    in
    [generateSchemaValueBinding type_name ptype_params schema_expr]

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
