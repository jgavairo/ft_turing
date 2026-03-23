open Types
open Helpers

type config = {
  state : string;
  left : char list;
  head : char;
  right : char list;
}

let chars_to_string chars =
  let buffer = Buffer.create (List.length chars) in
  List.iter (Buffer.add_char buffer) chars;
  Buffer.contents buffer

let explode_string s =
  let rec aux i acc =
    if i < 0 then acc
    else aux (i - 1) (s.[i] :: acc)
  in
  aux (String.length s - 1) []

let is_valid_input_char machine c =
  c <> machine.blank && List.mem c machine.alphabet

let validate_input machine input_chars =
  let rec loop = function
    | [] -> Ok ()
    | c :: rest ->
        if is_valid_input_char machine c then
          loop rest
        else
          Error (Printf.sprintf "Invalid input character: %c" c)
  in
  loop input_chars

let validate_input_format machine input =
  if machine.machine_name <> "unary_sub" then
    Ok ()
  else
    let len = String.length input in
    let count_char ch =
      let rec aux i acc =
        if i >= len then acc
        else if input.[i] = ch then aux (i + 1) (acc + 1)
        else aux (i + 1) acc
      in
      aux 0 0
    in
    let minus_count = count_char '-' in
    let equal_count = count_char '=' in
    if minus_count <> 1 || equal_count <> 1 then
      Error "Invalid input: unary_sub expects exactly one '-' and one '='"
    else if input.[len - 1] <> '=' then
      Error "Invalid input: unary_sub input must end with '='"
    else
      let minus_idx = String.index input '-' in
      let equal_idx = String.index input '=' in
      if minus_idx = 0 || equal_idx <= minus_idx + 1 then
        Error "Invalid input: unary_sub expects format 1+-1+="
      else
        let left_count = minus_idx in
        let right_count = equal_idx - minus_idx - 1 in
        if left_count < right_count then
          Error "Invalid input: unary_sub requires left unary number >= right unary number"
        else
          Ok ()

let print_config machine config =
  let left_visible = List.rev config.left in
  let right_padding = List.init 10 (fun _ -> machine.blank) in
  let right_visible = config.right @ right_padding in
  Printf.printf "[%s<%c>%s]\n"
    (chars_to_string left_visible)
    config.head
    (chars_to_string right_visible)

let init_config machine input =
  let input_chars = explode_string input in
  match validate_input machine input_chars with
  | Error _ as e -> e
  | Ok () ->
      begin
        match validate_input_format machine input with
        | Error _ as e -> e
        | Ok () ->
            begin
              match input_chars with
              | [] ->
                  Ok {
                    state = machine.initial_state;
                    left = [];
                    head = machine.blank;
                    right = [];
                  }
              | h :: t ->
                  Ok {
                    state = machine.initial_state;
                    left = [];
                    head = h;
                    right = t;
                  }
            end
      end

let step machine config =
  match find_transition_for_this_char machine config.state config.head with
  | None -> None
  | Some tr ->
      let written = tr.replace_by in
      let next_config =
        match tr.action with
        | Right ->
            begin
              match config.right with
              | [] ->
                  {
                    state = tr.to_state;
                    left = written :: config.left;
                    head = machine.blank;
                    right = [];
                  }
              | h :: t ->
                  {
                    state = tr.to_state;
                    left = written :: config.left;
                    head = h;
                    right = t;
                  }
            end
        | Left ->
            begin
              match config.left with
              | [] ->
                  {
                    state = tr.to_state;
                    left = [];
                    head = machine.blank;
                    right = written :: config.right;
                  }
              | h :: t ->
                  {
                    state = tr.to_state;
                    left = t;
                    head = h;
                    right = written :: config.right;
                  }
            end
      in
      Some (next_config, tr)

let action_to_string = function
  | Left -> "LEFT"
  | Right -> "RIGHT"

let run machine initial_config =
  let max_steps = 100000 in
  let rec loop steps config =
    if steps >= max_steps then
      begin
        print_config machine config;
        Printf.printf "STOP: exceeded max steps (%d), possible infinite loop\n" max_steps
      end
    else
    if is_final_state machine config.state then
      begin
        print_config machine config;
        Printf.printf "HALT: reached final state %s\n" config.state
      end
    else
      match step machine config with
      | None ->
          begin
            print_config machine config;
            Printf.printf "BLOCKED: no transition for state=%s read=%c\n"
              config.state config.head
          end
      | Some (next_config, tr) ->
          print_config machine config;
          Printf.printf "(%s, %c) -> (%s, %c, %s)\n"
            tr.current_state
            tr.read
            tr.to_state
            tr.replace_by
            (action_to_string tr.action);
          loop (steps + 1) next_config
  in
  loop 0 initial_config
