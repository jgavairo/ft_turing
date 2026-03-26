open Types
open Helpers
open Parser
open Simulator

let has_json_extension path =
      Filename.check_suffix path ".json"

let print_help () =
  Printf.printf "usage: ft_turing [-h] jsonfile input\n";
  Printf.printf "positional arguments:\n";
  Printf.printf "jsonfile json description of the machine\n";
  Printf.printf "input input of the machine\n";
  Printf.printf "optional arguments:\n";
  Printf.printf "-h, --help show this help message and exit\n"

let can_open_file path = 
  try
    let ic = open_in_bin path in
    close_in ic;
    true
  with _ -> false

let () =
  if Array.length Sys.argv = 2 && (Sys.argv.(1) = "-h" || Sys.argv.(1) = "--help") then
    print_help ()
  else if Array.length Sys.argv <> 3 then
    Printf.printf "usage: ft_turing [-h] jsonfile input\n"
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
          begin
            match init_config machine input with
            | Error msg ->
                Printf.printf "Error: %s\n" msg
            | Ok config ->
                let sep = String.make 80 '*' in
                Printf.printf "%s\n" sep;
                Printf.printf "*%s*\n" (String.make 78 ' ');
                Printf.printf "*%s%s%s*\n" 
                  (String.make ((78 - String.length machine.machine_name) / 2) ' ')
                  machine.machine_name
                  (String.make ((78 - String.length machine.machine_name + 1) / 2) ' ');
                Printf.printf "*%s*\n" (String.make 78 ' ');
                Printf.printf "%s\n" sep;
                Printf.printf "\n";
                Printf.printf "Alphabet: %s\n" (String.concat ", " alphabet);
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
                  machine.transitions;
                Printf.printf "\n";
                Printf.printf "%s\n" sep;
                Printf.printf "Execution with input: \"%s\"\n" input;
                Printf.printf "%s\n" sep;
                Printf.printf "\n";
                run machine config
          end

