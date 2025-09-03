open Turing

type step = {
  mutable input: string;
  mutable state: string;
  mutable head: int;
  mutable initial_step_sent: bool;
}

let step_to_json step =
  let json = `Assoc [
    ("input", `String step.input);
    ("state", `String step.state);
    ("head", `Int step.head);
  ] in
  Yojson.Safe.to_string json

let error_to_json msg =
  let json = `Assoc [
    ("error", `String msg);
  ] in
  Yojson.Safe.to_string json

let start_debug_mode (module Machine : Machine) (input : string) =
  let updated_input, state, head = Machine.initial_step input in
  let current_step: step = {
    input = updated_input;
    state = state;
    head = head;
    initial_step_sent = false;
  } in

  Dream.run
  @@ Dream.logger
  @@ Dream.router [
    Dream.get "/next-step" (fun _ ->
      let updated_input, state, head = Machine.compute_next_step current_step.input current_step.state current_step.head in
      current_step.input <- updated_input;
      current_step.state <- state;
      current_step.head <- head;
      Dream.json (step_to_json current_step));
  ]