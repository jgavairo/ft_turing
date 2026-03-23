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

let action_of_string = function
  | "LEFT" -> Ok Left
  | "RIGHT" -> Ok Right
  | s -> Error (Printf.sprintf "Invalid action: %s" s)

let as_list = function
  | `List l -> Ok l
  | _ -> Error "Expected a list"

let as_assoc = function
  | `Assoc fields -> Ok fields
  | _ -> Error "Root JSON must be an object"

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

let rec map_result f acc = function
  | [] -> Ok (List.rev acc)
  | x :: xs ->
      let* y = f x in
      map_result f (y :: acc) xs

let parse_transition current_state transition_json =
  let* fields = as_assoc transition_json in

  let* read_json = get_value "read" fields in
  let* read_string = as_string read_json in
  let* read = char_of_string read_string in

  let* write_json = get_value "write" fields in
  let* write_string = as_string write_json in
  let* replace_by = char_of_string write_string in

  let* to_state_json = get_value "to_state" fields in
  let* to_state = as_string to_state_json in

  let* action_json = get_value "action" fields in
  let* action_string = as_string action_json in
  let* action = action_of_string action_string in

  Ok {
    current_state;
    read;
    replace_by;
    to_state;
    action;
  }

let parse_transition_list current_state rules_json =
  let* rules = as_list rules_json in
  map_result (parse_transition current_state) [] rules

let parse_transitions transitions_json =
  let* by_state = as_assoc transitions_json in
  let rec collect acc = function
    | [] -> Ok (List.rev acc |> List.flatten)
    | (state_name, rules_json) :: rest ->
        let* parsed_rules = parse_transition_list state_name rules_json in
        collect (parsed_rules :: acc) rest
  in
  collect [] by_state

let check_parsing jsonfile_path =
  try
    let json = Yojson.Safe.from_file jsonfile_path in
    Ok json
  with exn ->
    Error (Printf.sprintf "Error parsing JSON file: %s" (Printexc.to_string exn))

let get_machine_settings jsonfile_path =
  let* json = check_parsing jsonfile_path in
  let* fields = as_assoc json in

  let* name_json = get_value "name" fields in
  let* machine_name = as_string name_json in

  let* alphabet_json = get_value "alphabet" fields in
  let* alphabet_strings = as_string_list alphabet_json in
  let* alphabet = char_list_of_strings alphabet_strings in

  let* blank_json = get_value "blank" fields in
  let* blank_string = as_string blank_json in
  let* blank = char_of_string blank_string in

  let* states_json = get_value "states" fields in
  let* states = as_string_list states_json in

  let* initial_json = get_value "initial" fields in
  let* initial_state = as_string initial_json in

  let* finals_json = get_value "finals" fields in
  let* final_states = as_string_list finals_json in

  let* transitions_json = get_value "transitions" fields in
  let* transitions = parse_transitions transitions_json in

  if not (List.mem blank alphabet) then
    Error "Invalid machine: blank symbol must be part of alphabet"
  else if not (List.mem initial_state states) then
    Error "Invalid machine: initial state must be part of states"
  else if not (List.for_all (fun st -> List.mem st states) final_states) then
    Error "Invalid machine: final states must be part of states"
  else if not (List.for_all (fun t -> List.mem t.current_state states) transitions) then
    Error "Invalid machine: transition source state must be part of states"
  else if not (List.for_all (fun t -> List.mem t.to_state states) transitions) then
    Error "Invalid machine: transition to_state must be part of states"
  else if not (List.for_all (fun t -> List.mem t.read alphabet) transitions) then
    Error "Invalid machine: transition read symbol must be part of alphabet"
  else if not (List.for_all (fun t -> List.mem t.replace_by alphabet) transitions) then
    Error "Invalid machine: transition write symbol must be part of alphabet"
  else
    Ok {
      machine_name;
      initial_state;
      states;
      alphabet;
      blank;
      transitions;
      final_states;
    }

let get_machine_Settings = get_machine_settings