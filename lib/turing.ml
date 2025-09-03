module StateHashtbl = Hashtbl.Make (struct
  type t = string

  let equal = String.equal
  let hash = Hashtbl.hash
end)

module type INPUT = sig
  type transition = string * string * string * string

  val name : string
  val alphabet : string list
  val blank : string
  val states : string list
  val initial : string
  val finals : string list
  val transitions : transition array StateHashtbl.t
end

module type Machine = sig
  type transition = string * string * string * string
  type state = string
  type step = string * state * int

  val name : string
  val alphabet : string list
  val blank : string
  val states : state list
  val initial : state
  val finals : state list
  val transitions : transition array StateHashtbl.t
  val print_machine : unit -> unit
  val initial_step: string -> step
  val execute : string -> unit
  val compute : string -> state -> int -> unit
  val compute_next_step : string -> state -> int -> step
  val process : string -> state -> int -> bool -> step
  val compile : string -> unit
end

module Make : functor (I : INPUT) -> Machine =
functor
  (I : INPUT)
  ->
  struct
    type transition = string * string * string * string
    type state = string
    type step = string * state * int

    let name = I.name
    let alphabet = I.alphabet
    let blank = I.blank
    let states = I.states
    let initial = I.initial
    let finals = I.finals
    let transitions = I.transitions

    let initial_step str_input =
      if
        String.for_all
          (fun c ->
            List.exists (fun str -> c <> blank.[0] && str.[0] = c) alphabet)
          str_input
      then
        let blank_str = String.make 10 blank.[0] in
        if List.exists (fun str -> str = "_start_mem") states then
          (("#" ^ str_input ^ blank_str), initial, 0)
        else ((str_input ^ blank_str), initial, 0)
      else
        raise
          (Invalid_argument
             ("Input for " ^ name
            ^ " is incorrect. One of the character is not in the alphabet of \
               the machine or is the blank character."))

    let process str_input state head debug =
      let updated_input =
        if head = String.length str_input then str_input ^ blank
        else if head = -1 then blank ^ str_input
        else str_input
      in
      let updated_head = if head = -1 then 0 else head in
      let formated_str =
        "["
        ^ String.fold_left
            (fun acc element ->
              acc
              ^
              if String.length acc = updated_head then
                "\027[1;96m<\027[0;37m" ^ String.make 1 element
                ^ "\027[1;96m>\027[0;37m"
              else String.make 1 element)
            "" updated_input
        ^ "]"
      in
      if not debug then print_string formated_str;
      let read_head = String.make 1 updated_input.[updated_head] in
      if not debug then print_string ("(" ^ state ^ ", " ^ read_head ^ ") -> ");
      let state_transition = StateHashtbl.find transitions state in
      let _, new_state, write, action =
        match
          Array.find_map
            (fun (read, to_state, write, action) ->
              if read = read_head then Some (read, to_state, write, action)
              else None)
            state_transition
        with
        | Some t -> t
        | None -> ("", "", "", "")
      in
      let new_head =
        match action with "LEFT" -> head - 1 | "RIGHT" -> head + 1 | _ -> -1
      in
      let new_str =
        String.fold_left
          (fun acc element ->
            acc @ if List.length acc = head then [ write.[0] ] else [ element ])
          [] updated_input
      in
      let new_str = String.of_seq (List.to_seq new_str) in
      let formated_state =
        if new_state = "HALT" then "\027[0;32mHALT\027[0;37m"
        else if new_state = "ERROR" then "\027[0;31mERROR\027[0;37m"
        else new_state
      in
      if not debug then print_endline ("(" ^ formated_state ^ ", " ^ write ^ ", " ^ action ^ ")");
      (new_str, new_state, new_head)

    let rec compute str_input state head =
      if List.exists (fun e -> e = state) finals then
        print_endline ("Output: " ^ str_input)
      else
        let new_input, new_state, new_head = process str_input state head false in
        compute new_input new_state new_head

    let compute_next_step str_input state head =
      let new_input, new_state, new_head = process str_input state head true in
      (new_input, new_state, new_head)

    let execute str_input =
      if
        String.for_all
          (fun c ->
            List.exists (fun str -> c <> blank.[0] && str.[0] = c) alphabet)
          str_input
      then
        let blank_str = String.make 10 blank.[0] in
        if List.exists (fun str -> str = "_start_mem") states then
          compute ("#" ^ str_input ^ blank_str) initial 0
        else compute (str_input ^ blank_str) initial 0
      else
        raise
          (Invalid_argument
             ("Input for " ^ name
            ^ " is incorrect. One of the character is not in the alphabet of \
               the machine or is the blank character."))

    let print_machine () =
      print_endline
        "\027[1;35m================================================================================";
      print_newline ();
      print_endline ("\027[0;37m \t                        " ^ name);
      print_newline ();
      print_endline
        "\027[1;35m================================================================================\027[0;37m";
      let print_elem str = print_string (str ^ "; ") in
      print_string "Alphabet: [ ";
      List.iter print_elem alphabet;
      print_endline "]";
      print_string "States  : [ ";
      List.iter print_elem states;
      print_endline "]";
      print_endline ("Initial : " ^ initial);
      print_string "Finals  : [ ";
      List.iter print_elem finals;
      print_endline "]";
      StateHashtbl.iter
        (fun key arr ->
          Array.iter
            (fun (read, to_state, write, action) ->
              print_endline
                ("(" ^ key ^ ", " ^ read ^ ") -> (" ^ to_state ^ ", " ^ write
               ^ ", " ^ action ^ ")"))
            arr)
        transitions;
      print_endline
        "\027[1;35m================================================================================\027[0;37m"
    let compile str_input =
      if
        String.for_all
          (fun c -> List.exists (fun str -> str.[0] = c) alphabet)
          str_input
      then (
        let utm_allowed_alphabet = "0123456789abcdef+=-" in
        let machine_alphabet =
          List.fold_left (fun acc str -> acc ^ str) "" alphabet
        in
        let nb_of_states = List.length states - List.length finals in
        if
          not
            (String.for_all
               (fun c -> String.exists (fun a -> a = c) utm_allowed_alphabet)
               machine_alphabet)
        then
          invalid_arg
            "Error: alphabet is not compatible with the utm allowed alphabet: \
             0123456789abcdef+=-"
        else if nb_of_states > String.length utm_allowed_alphabet then
          invalid_arg "Error: too many states. Maximum states allowed is 19"
        else
          let tail_states =
            List.filter
              (fun str ->
                not (List.exists (fun s -> s = str) finals || str = initial))
              states
          in
          let ht_state = StateHashtbl.create nb_of_states in
          StateHashtbl.add ht_state initial
            (String.make 1 utm_allowed_alphabet.[0]);
          List.iteri
            (fun i str ->
              StateHashtbl.add ht_state str
                (String.make 1 utm_allowed_alphabet.[i + 1]))
            tail_states;
          let build_transition arr =
            let raw_transitions =
              Array.fold_left
                (fun acc (read, to_state, write, action) ->
                  acc ^ read
                  ^ (try StateHashtbl.find ht_state to_state
                     with e ->
                       if List.exists (fun str -> str = to_state) finals then
                         "H"
                       else raise e)
                  ^ write
                  ^ if action = "RIGHT" then "0," else "1,")
                "" arr
            in
            String.sub raw_transitions 0 (String.length raw_transitions - 1)
          in
          let first_state =
            StateHashtbl.find ht_state initial
            ^ "("
            ^ build_transition (StateHashtbl.find transitions initial)
            ^ ")"
          in
          print_string
            (machine_alphabet ^ "_"
            ^ List.fold_left
                (fun acc str ->
                  acc
                  ^ StateHashtbl.find ht_state str
                  ^ "("
                  ^ build_transition (StateHashtbl.find transitions str)
                  ^ ")")
                first_state tail_states
            ^ ">" ^ str_input))
      else
        raise
          (Invalid_argument
             ("Input for " ^ name
            ^ " is incorrect. One of the character is not in the alphabet of \
               the machine."))
  end

let from_input json_path =
  if not (String.ends_with ~suffix:".json" json_path) then
    invalid_arg ("Error: " ^ json_path ^ " is not a JSON file.")
  else
    let open Core in
    let open Yojson.Basic.Util in
    let json = Yojson.Basic.from_file json_path in
    let name : string = json |> member "name" |> to_string in
    let alphabet : string list =
      json |> member "alphabet" |> to_list |> filter_string
    in
    let blank : string = json |> member "blank" |> to_string in
    let states : string list =
      json |> member "states" |> to_list |> filter_string
    in
    let initial : string = json |> member "initial" |> to_string in
    let finals : string list =
      json |> member "finals" |> to_list |> filter_string
    in
    let transitions = json |> member "transitions" |> to_assoc in
    let ht = StateHashtbl.create (List.length states) in
    transitions
    |> List.iter ~f:(fun (state, transitions) ->
           let transition_array =
             if not (List.exists states ~f:(fun str -> String.equal str state))
             then
               raise
                 (Invalid_argument
                    ("Error with JSON file: " ^ state ^ " is not in states."))
             else
               transitions |> to_list
               |> List.map ~f:(fun t ->
                      let read = t |> member "read" |> to_string in
                      let to_state = t |> member "to_state" |> to_string in
                      let write = t |> member "write" |> to_string in
                      let action = t |> member "action" |> to_string in
                      if
                        not
                          (List.exists alphabet ~f:(fun str ->
                               String.equal str read))
                      then
                        raise
                          (Invalid_argument
                             ("Error with JSON file: in " ^ state ^ ", read: '"
                            ^ read ^ "' is not in alphabet."))
                      else if
                        not
                          (List.exists alphabet ~f:(fun str ->
                               String.equal str write))
                      then
                        raise
                          (Invalid_argument
                             ("Error with JSON file: in " ^ state ^ ", write: '"
                            ^ write ^ "' is not in alphabet."))
                      else if
                        not
                          (List.exists states ~f:(fun str ->
                               String.equal str to_state))
                      then
                        raise
                          (Invalid_argument
                             ("Error with JSON file: in " ^ state
                            ^ ", to_state: '" ^ to_state ^ "' is not in states."
                             ))
                      else if
                        not
                          (String.equal action "LEFT"
                          || String.equal action "RIGHT")
                      then
                        raise
                          (Invalid_argument
                             ("Error with JSON file: in " ^ state
                            ^ ", action: '" ^ action
                            ^ "' is not a valid action."))
                      else (read, to_state, write, action))
               |> Array.of_list
           in
           StateHashtbl.add ht state transition_array);
    if List.is_empty alphabet then
      raise (Invalid_argument "Error with JSON file: alphabet is empty.")
    else if not (List.for_all alphabet ~f:(fun str -> String.length str = 1))
    then
      raise
        (Invalid_argument
           "Error with JSON file: one letter of the alphabet is larger than \
            one character.")
    else if String.is_empty blank then
      raise (Invalid_argument "Error with JSON file: blank is empty.")
    else if List.is_empty states then
      raise (Invalid_argument "Error with JSON file: states is empty.")
    else if String.is_empty initial then
      raise (Invalid_argument "Error with JSON file: initial is empty.")
    else if List.is_empty finals then
      raise (Invalid_argument "Error with JSON file: finals is empty.")
    else if not (List.exists states ~f:(fun str -> String.equal str initial))
    then
      raise
        (Invalid_argument
           "Error with JSON file: initial is not in states array.")
    else if
      not
        (List.for_all finals ~f:(fun str ->
             List.exists states ~f:(fun s -> String.equal s str)))
    then
      raise
        (Invalid_argument
           "Error with JSON file: some of the finals states are not in the \
            states list")
    else
      let module Input : INPUT = struct
        type transition = string * string * string * string

        let name = name
        let alphabet = alphabet
        let blank = blank
        let states = states
        let initial = initial
        let finals = finals
        let transitions = ht
      end in
      (module Input : INPUT)
