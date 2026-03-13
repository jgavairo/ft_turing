open Types
open Helpers
open Parser

let has_json_extension path =
      Filename.check_suffix path ".json"

let can_open_file path = 
  try
    let ic = open_in_bin path in
    close_in ic;
    true
  with _ -> false

let () =
  if Array.length Sys.argv <> 3 then
    Printf.printf "usage: ft_turing jsonfile input\n"
  else
    let jsonfile_path = Sys.argv.(1) in
    let input = Sys.argv.(2) in

    if not (has_json_extension jsonfile_path) then
      Printf.printf "Error: The file must have a .json extension.\n"
    else if not (can_open_file jsonfile_path) then
      Printf.printf "Error: Cannot open file %s\n" jsonfile_path
    else
      Printf.printf "jsonfile=%s input=%s\n" jsonfile_path input

  (* let machine_name = "unary_sub" in
  let path = "machines/" ^ machine_name ^ ".json" in
  let json = Yojson.Safe.from_file path in
  let name = get_value "name" (Yojson.Safe.Util.to_assoc json) |> Result.get_ok |> Yojson.Safe.Util.to_string in
  let alphabet = get_value "alphabet" (Yojson.Safe.Util.to_assoc json) |> Result.get_ok |> Yojson.Safe.Util.to_list |> List.map Yojson.Safe.Util.to_string in
  let blank = get_value "blank" (Yojson.Safe.Util.to_assoc json) |> Result.get_ok |> Yojson.Safe.Util.to_string in
  let states = get_value "states" (Yojson.Safe.Util.to_assoc json) |> Result.get_ok |> Yojson.Safe.Util.to_list |> List.map Yojson.Safe.Util.to_string in
  let initial_state = get_value "initial" (Yojson.Safe.Util.to_assoc json) |> Result.get_ok |> Yojson.Safe.Util.to_string in
  let final_states = get_value "finals" (Yojson.Safe.Util.to_assoc json) |> Result.get_ok |> Yojson.Safe.Util.to_list |> List.map Yojson.Safe.Util.to_string in
  Printf.printf "Machine name: %s\n" name;
  Printf.printf "Alphabet: %s\n" (String.concat ", " alphabet);
  Printf.printf "Blank symbol: %s\n" blank;
  Printf.printf "States: %s\n" (String.concat ", " states);
  Printf.printf "Initial state: %s\n" initial_state;
  Printf.printf "Final states: %s\n" (String.concat ", " final_states); *)

