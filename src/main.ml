open Types
open Helpers

let () =
  let t1 = {
    current_state = "scanright";
    read = '1';
    replace_by = '1';
    to_state = "scanright";
    action = Right;
  } in

  let t2 = {
    current_state = "scanright";
    read = '=';
    replace_by = '.';
    to_state = "eraseone";
    action = Left;
  } in

  let m = {
    machine_name = "demo";
    initial_state = "scanright";
    states = ["scanright"; "eraseone"; "HALT"];
    alphabet = ['1'; '='; '.'];
    blank = '.';
    transitions = [t1; t2];
    final_states = ["HALT"];
  } in

  (* test char_of_string *)
  begin match char_of_string "a" with
  | Ok c -> Printf.printf "char_of_string(\"a\") = %c\n" c
  | Error msg -> Printf.printf "error: %s\n" msg
  end;

  (* test is_final_state *)
  Printf.printf "is_final_state HALT = %b\n" (is_final_state m "HALT");
  Printf.printf "is_final_state scanright = %b\n" (is_final_state m "scanright");

  (* test find_transition_for_this_char *)
  begin match find_transition_for_this_char m "scanright" '=' with
  | Some tr ->
      Printf.printf "transition found: %s --%c--> %s\n"
        tr.current_state tr.read tr.to_state
  | None ->
      Printf.printf "no transition found\n"
  end