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

let count_char_in_string input ch =
  let len = String.length input in
  let rec aux i acc =
    if i >= len then acc
    else if input.[i] = ch then aux (i + 1) (acc + 1)
    else aux (i + 1) acc
  in
  aux 0 0

let all_chars_match input predicate =
  let len = String.length input in
  let rec aux i =
    if i >= len then true
    else if predicate input.[i] then aux (i + 1)
    else false
  in
  aux 0

let validate_unary_sub_format input =
  let len = String.length input in
  let minus_count = count_char_in_string input '-' in
  let equal_count = count_char_in_string input '=' in
  if minus_count <> 1 || equal_count <> 1 then
    Error "Invalid input: unary_sub expects exactly one '-' and one '='"
  else if len = 0 || input.[len - 1] <> '=' then
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

let validate_unary_add_format input =
  let len = String.length input in
  let plus_count = count_char_in_string input '+' in
  let equal_count = count_char_in_string input '=' in
  if plus_count <> 1 || equal_count <> 1 then
    Error "Invalid input: unary_add expects exactly one '+' and one '='"
  else if len = 0 || input.[len - 1] <> '=' then
    Error "Invalid input: unary_add input must end with '='"
  else
    let plus_idx = String.index input '+' in
    let equal_idx = String.index input '=' in
    if plus_idx = 0 || equal_idx <= plus_idx + 1 then
      Error "Invalid input: unary_add expects format 1++1+="
    else
      Ok ()

let validate_palindrome_format input =
  if all_chars_match input (fun c -> c = '0' || c = '1') then
    Ok ()
  else
    Error "Invalid input: palindrome expects only '0' and '1'"

let validate_zero_n_one_n_format input =
  let len = String.length input in
  let rec scan_zeros i =
    if i < len && input.[i] = '0' then scan_zeros (i + 1)
    else i
  in
  let rec scan_ones i =
    if i < len && input.[i] = '1' then scan_ones (i + 1)
    else i
  in
  let zero_count = scan_zeros 0 in
  let one_end = scan_ones zero_count in
  let one_count = one_end - zero_count in
  if one_end <> len then
    Error "Invalid input: zero_n_one_n expects format 0*1*"
  else if zero_count <> one_count then
    Error "Invalid input: zero_n_one_n expects |0| = |1|"
  else
    Ok ()

let validate_zero_2n_format input =
  let len = String.length input in
  if not (all_chars_match input (fun c -> c = '0')) then
    Error "Invalid input: zero_2n expects only '0'"
  else if len mod 2 <> 0 then
    Error "Invalid input: zero_2n expects an even number of '0'"
  else
    Ok ()

let validate_encoded_unary_add_runner_format input =
  let pipe_count = count_char_in_string input '|' in
  if pipe_count <> 1 then
    Error "Invalid input: encoded_unary_add_runner expects exactly one '|' separator"
  else
    let pipe_idx = String.index input '|' in
    if pipe_idx = String.length input - 1 then
      Error "Invalid input: encoded_unary_add_runner expects unary_add input after '|'"
    else
      let expr_len = String.length input - pipe_idx - 1 in
      let expr = String.sub input (pipe_idx + 1) expr_len in
      validate_unary_add_format expr

let validate_input_format machine input =
  match machine.machine_name with
  | "unary_sub" -> validate_unary_sub_format input
  | "unary_add" -> validate_unary_add_format input
  | "palindrome" -> validate_palindrome_format input
  | "zero_n_one_n" -> validate_zero_n_one_n_format input
  | "zero_2n" -> validate_zero_2n_format input
  | "encoded_unary_add_runner" -> validate_encoded_unary_add_runner_format input
  | _ -> Ok ()

let print_config machine config =
  let left_visible = List.rev config.left in
  let right_padding = List.init 10 (fun _ -> machine.blank) in
  let right_visible = config.right @ right_padding in
  Printf.printf "[%s<%c>%s] "
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
