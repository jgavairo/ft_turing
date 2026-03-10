type action = Left | Right

type transition = {
  current_state: string;
  read: char;
  replace_by : char;
  to_state: string;
  action: action;
}

type machine = {
machine_name: string;
initial_state: string;
states: string list;
alphabet: char list;
blank: char;
transitions: transition list;
final_states: string list;
}