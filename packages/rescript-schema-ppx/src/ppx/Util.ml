open Ppxlib
open Parsetree
open Ast_helper

let loc = !default_loc
let fail loc message =
  Location.raise_errorf ~loc "[@sury/ppx] %s" message
let longident_parse = Longident.parse [@@ocaml.warning "-3"]
let mkloc txt loc = {Location.txt; loc}
let mknoloc txt = mkloc txt Location.none
let lid ?(loc = Location.none) s = mkloc (Longident.parse s) loc
let makeIdentExpr ?attrs s = Exp.ident ?attrs (mknoloc (longident_parse s))

let getAttributeByName attributes name =
  let filtered =
    attributes |> List.filter (fun {attr_name = {Location.txt}} -> txt = name)
  in
  match filtered with
  | [] -> Ok None
  | [attribute] -> Ok (Some attribute)
  | _ -> Error ("Too many occurrences of \"" ^ name ^ "\" attribute")

let getExpressionFromPayload {attr_name = {loc}; attr_payload = payload} =
  match payload with
  | PStr [{pstr_desc = Pstr_eval (expr, _)}] -> expr
  | _ -> fail loc "Expected expression as attribute payload"

let generateSchemaName type_name =
  match type_name with
  | "t" -> "schema"
  | _ -> type_name ^ "Schema"

type field = {
  name: string;
  runtime_name: string;
  core_type: core_type;
  is_optional: bool;
}

let getMaybeFieldAlias loc attributes =
  match getAttributeByName attributes "as" with
  | Ok (Some attribute) -> (
    match (getExpressionFromPayload attribute).pexp_desc with
    | Pexp_constant (Pconst_string (str, _, _)) -> Some str
    | _ -> fail loc "The @as attribute payload is not a string")
  | Ok None -> None
  | Error s -> fail loc s

let parseLabelDeclaration {pld_name = {txt}; pld_loc; pld_type; pld_attributes}
    =
  let maybe_field_alias = getMaybeFieldAlias pld_loc pld_attributes in
  let runtime_name =
    match maybe_field_alias with
    | Some field_alias -> field_alias
    | None -> txt
  in
  let is_optional =
    ["ns.optional"; "res.optional"]
    |> List.map (fun attr -> getAttributeByName pld_attributes attr)
    |> List.exists (function
         | Ok (Some _) -> true
         | _ -> false)
  in
  {name = txt; runtime_name; core_type = pld_type; is_optional}

let parseObjectField {pof_desc; pof_loc; pof_attributes} =
  let name, core_type =
    match pof_desc with
    | Oinherit _ -> fail pof_loc "Unsupported Oinherit object field"
    | Otag ({txt}, core_type) -> (txt, core_type)
  in
  let is_optional =
    ["ns.optional"; "res.optional"]
    |> List.map (fun attr -> getAttributeByName pof_attributes attr)
    |> List.exists (function
         | Ok (Some _) -> true
         | _ -> false)
  in
  {name; runtime_name = name; core_type; is_optional}
