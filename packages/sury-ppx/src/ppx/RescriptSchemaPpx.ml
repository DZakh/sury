open Ppxlib

class mapper =
  object (self)
    inherit Ast_traverse.map

    method! signature sign =
      sign |> List.map (Signature.mapSignatureItem self) |> List.concat

    method! structure strt =
      strt |> List.map (Structure.mapStructureItem self) |> List.concat
  end

let signatureMapper = (new mapper)#signature
let structureMapper = (new mapper)#structure;;

Ppxlib.Driver.register_transformation ~impl:structureMapper
  ~intf:signatureMapper "rescript-schema"
