open Types
open Helpers
open Yojson.Safe

let ( let* ) r f =
  match r with
  | Ok v -> f v
  | Error e -> Error e

let get_value element fields =
  match List.assoc_opt element fields with
  | Some value -> Ok value
  | None -> Error (Printf.sprintf "Missing field: %s" element)

let as_string = function
  | `String s -> Ok s
  | _ -> Error "Expected a string"

let as_list = function
  | `List l -> Ok l
  | _ -> Error "Expected a list"

let as_string_list json =
  let* values = as_list json in
  let rec collect acc = function
    | [] -> Ok (List.rev acc)
    | x :: xs ->
        let* s = as_string x in
        collect (s :: acc) xs
  in
  collect [] values

let char_list_of_strings strings =
  let rec collect acc = function
    | [] -> Ok (List.rev acc)
    | s :: rest ->
        let* c = char_of_string s in
        collect (c :: acc) rest
  in
  collect [] strings

let check_parsing jsonfile_path =
  try
    let json = Yojson.Safe.from_file jsonfile_path in
    Ok json
  with exn ->
    Error (Printf.sprintf "Error parsing JSON file: %s" (Printexc.to_string exn))

let get_machine_settings jsonfile_path =
  let* json = check_parsing jsonfile_path in
  let fields = Yojson.Safe.Util.to_assoc json in

  let* machine_name = get_value "name" fields |> Result.bind as_string in
  let* alphabet_strings = get_value "alphabet" fields |> Result.bind as_string_list in
  let* alphabet = char_list_of_strings alphabet_strings in
  let* blank_string = get_value "blank" fields |> Result.bind as_string in
  let* blank = char_of_string blank_string in
  let* states = get_value "states" fields |> Result.bind as_string_list in
  let* initial_state = get_value "initial" fields |> Result.bind as_string in
  let* final_states = get_value "finals" fields |> Result.bind as_string_list in

  if not (List.mem blank alphabet) then
    Error "Invalid machine: blank symbol must be part of alphabet"
  else if not (List.mem initial_state states) then
    Error "Invalid machine: initial state must be part of states"
  else if not (List.for_all (fun st -> List.mem st states) final_states) then
    Error "Invalid machine: final states must be part of states"
  else
    Ok {
      machine_name;
      initial_state;
      states;
      alphabet;
      blank;
      transitions = [];
      final_states;
    }

let get_machine_Settings = get_machine_settings