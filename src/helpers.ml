open Types

let char_of_string s =
  if String.length s = 1 then
    Ok(String.get s 0)
  else
    Error("String must be of length 1")


let is_final_state machine current_state =
  if List.exists (fun s -> s = current_state) machine.final_states then
    true
  else
    false


let find_transition_for_this_char machine current_state current_char =
  List.find_opt (fun t -> t.current_state = current_state && t.read = current_char) machine.transitions