open Yojson.Safe

let get_value element fields =
  match List.assoc_opt element fields with
  | Some value -> Ok value
  | None -> Error (Printf.sprintf "Missing field: %s" element)