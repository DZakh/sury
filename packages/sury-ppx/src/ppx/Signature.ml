open Ppxlib
open Parsetree
open Ast_helper
open Util

let generateSchemaSignatureItem ~type_declaration =
  let {ptype_name = {txt = type_name}} = type_declaration in
  let schema_name = generateSchemaName type_name in
  [%type: [%t Typ.constr (lid type_name) []] S.t]
  |> Val.mk (mknoloc schema_name)
  |> Sig.value

let mapSignatureItem mapper ({psig_desc} as signature_item) =
  match psig_desc with
  | Psig_type (_, decls) ->
    let generated_sig_items =
      decls
      |> List.map (fun type_declaration ->
             match
               getAttributeByName type_declaration.ptype_attributes "schema"
             with
             | Error err -> fail type_declaration.ptype_loc err
             | Ok None -> []
             | Ok (Some _) -> [generateSchemaSignatureItem ~type_declaration])
      |> List.concat
    in
    mapper#signature_item signature_item :: generated_sig_items
  | _ -> [mapper#signature_item signature_item]
