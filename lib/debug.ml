open Turing

type step = {
  mutable input: string;
  mutable state: string;
  mutable head: int;
  mutable initial_step_sent: bool;
}

type breakpoints = {
  mutable lines: (string * string) list;
}

let ok_status = 
  let json = `Assoc [("status", `String "ok")] in
  Yojson.Safe.to_string json

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
  let breakpoints: breakpoints = {
    lines = [];
  } in

  Dream.run
  @@ Dream.logger
  @@ Dream.router [
    Dream.get "/step" (fun _ ->
      let updated_input, state, head = Machine.compute_next_step current_step.input current_step.state current_step.head in
      current_step.input <- updated_input;
      current_step.state <- state;
      current_step.head <- head;
      Dream.json (step_to_json current_step));

    Dream.get "/run" (fun _ ->
      let rec run_until_breakpoint () =
          if List.exists (fun (s) -> s = current_step.state) Machine.finals
            || List.exists (fun (s, r) -> 
            s = current_step.state
            && r.[0] = current_step.input.[current_step.head]
          ) breakpoints.lines
          then ()
          else
            let updated_input, state, head = Machine.compute_next_step current_step.input current_step.state current_step.head in
            current_step.input <- updated_input;
            current_step.state <- state;
            current_step.head <- head;
            run_until_breakpoint ()
      in
      run_until_breakpoint ();
      Dream.json (step_to_json current_step)
    );

    Dream.post "/breakpoint" (fun req ->
      (* %lwt is a macro for handling Lwt promises *)
      let%lwt body = Dream.body req in
      match Yojson.Safe.from_string body with
      | exception Yojson.Json_error msg ->
          Dream.json (error_to_json msg)
      | json ->
          let open Yojson.Safe.Util in
          let state = json |> member "state" |> to_string in
          let read = json |> member "read" |> to_string in
          breakpoints.lines <- (state, read) :: breakpoints.lines;
          Dream.json (ok_status)
    );

    Dream.delete "/breakpoint" (fun req ->
      let%lwt body = Dream.body req in
      match Yojson.Safe.from_string body with
      | exception Yojson.Json_error msg ->
          Dream.json (error_to_json msg)
      | json ->
          let open Yojson.Safe.Util in
          let state = json |> member "state" |> to_string in
          let read = json |> member "read" |> to_string in
          breakpoints.lines <- List.filter (fun (s, r) -> not (s = state && r = read)) breakpoints.lines;
          Dream.json (ok_status)
    );
  ]