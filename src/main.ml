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
      match get_machine_settings jsonfile_path with
      | Error msg ->
          Printf.printf "Error: %s\n" msg
      | Ok machine ->
          let alphabet = List.map (String.make 1) machine.alphabet in
          let action_to_string = function
            | Left -> "LEFT"
            | Right -> "RIGHT"
          in
          Printf.printf "jsonfile=%s input=%s\n" jsonfile_path input;
          Printf.printf "Machine name: %s\n" machine.machine_name;
          Printf.printf "Alphabet: %s\n" (String.concat ", " alphabet);
          Printf.printf "Blank symbol: %c\n" machine.blank;
          Printf.printf "States: %s\n" (String.concat ", " machine.states);
          Printf.printf "Initial state: %s\n" machine.initial_state;
          Printf.printf "Final states: %s\n" (String.concat ", " machine.final_states);
          Printf.printf "Transitions (%d):\n" (List.length machine.transitions);
          List.iter
            (fun t ->
              Printf.printf "(%s, %c) -> (%s, %c, %s)\n"
                t.current_state
                t.read
                t.to_state
                t.replace_by
                (action_to_string t.action))
            machine.transitions

