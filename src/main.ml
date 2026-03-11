open Types
open Helpers
open Parser

let () =
  let machine_name = "unary_sub" in
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
  Printf.printf "Final states: %s\n" (String.concat ", " final_states);