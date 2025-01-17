module StateHashtbl = Hashtbl.Make(struct
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
        val transitions : (transition array) StateHashtbl.t
end

module type Machine = sig

        type transition = string * string * string * string
        type state = string

        val name : string
        val alphabet : string list
        val blank : string
        val states : state list
        val initial : state
        val finals : state list
        val transitions : (transition array) StateHashtbl.t

        val print_machine : unit -> unit
        val execute : string -> unit
        val compute : string -> state -> int -> unit
        val process : string -> state -> int -> string * state * int
        val compile : string -> unit

end

module Make : functor (I: INPUT) -> Machine = functor (I: INPUT) -> struct
        
        type transition = string * string * string * string
        type state = string

        let name = I.name
        let alphabet = I.alphabet
        let blank = I.blank
        let states = I.states
        let initial = I.initial
        let finals = I.finals
        let transitions = I.transitions
        let process str_input state head =
                let formated_str = String.fold_left (fun acc element -> acc @ if List.length acc = head then ['<';element;'>'] else [element]) [] str_input in
                let formated_str = "[" ^ String.of_seq (List.to_seq formated_str) ^ "] " in
                print_string formated_str;
                let read_head = String.make 1 str_input.[head] in
                print_string ("(" ^ state ^ ", " ^ read_head ^ ") -> ");
                let state_transition = StateHashtbl.find transitions state in
                let (_, new_state, write, action) = 
                        match Array.find_map
                                (fun (read, to_state, write, action) -> if read = read_head then Some (read, to_state, write, action) else None) 
                                state_transition with
                                | Some t -> t
                                | None -> ("", "", "", "")
                in
                let new_head = match action with
                        | "LEFT" -> head - 1
                        | "RIGHT" -> head + 1
                        | _ -> -1
                in
                let new_str = String.fold_left (fun acc element -> acc @ if List.length acc = head then [write.[0]] else [element]) [] str_input in
                let new_str = String.of_seq (List.to_seq new_str) in
                print_endline ("(" ^ new_state ^ ", " ^ write ^ ", " ^ action ^ ")");
                (new_str, new_state, new_head)
        let rec compute str_input state head = 
                if List.exists (fun e -> e = state) finals then begin print_endline ("Output: " ^ str_input) end
                else
                        let (new_input, new_state, new_head) = process str_input state head in
                        compute new_input new_state new_head
        let execute str_input = compute str_input initial 0
        let print_machine () =
                print_endline ("********************************************************************************");
                print_endline ("*                                                                              *");
                print_endline ("*                               " ^ name ^ "                          *");
                print_endline ("*                                                                              *");
                print_endline ("********************************************************************************");
                let print_elem str = print_string (str ^ "; ") in
                print_string "Alphabet: [ "; List.iter print_elem alphabet; print_endline "]";
                print_string "States  : [ "; List.iter print_elem states; print_endline "]";
                print_endline ("Initial : " ^ initial);
                print_string "Finals  : [ "; List.iter print_elem finals; print_endline "]";
                StateHashtbl.iter 
                        (fun key arr -> Array.iter 
                                (fun (read, to_state, write, action) -> print_endline ("(" ^ key ^ ", " ^ read ^ ") -> (" ^ to_state ^ ", " ^ write ^ ", " ^ action ^ ")")) arr)
                        transitions;
                print_endline ("********************************************************************************")

        let compile input = print_endline ("future_machine_description;" ^ input)
end

let from_input json_path =
        let open Core in
        let open Yojson.Basic.Util in
        let json = Yojson.Basic.from_file json_path in
        let name = json |> member "name" |> to_string in
        let alphabet = json |> member "alphabet" |> to_list |> filter_string in
        let blank = json |> member "blank" |> to_string in
        let states = json |> member "states" |> to_list |> filter_string in
        let initial = json |> member "initial" |> to_string in
        let finals = json |> member "finals" |> to_list |> filter_string in
        let transitions = json |> member "transitions" |> to_assoc in
        let ht = StateHashtbl.create (List.length states) in
        transitions |> List.iter ~f:(fun (state, transitions) ->
                let transition_array =
                        transitions |> to_list
                        |> List.map ~f:(fun t ->
                            let read = t |> member "read" |> to_string in
                            let to_state = t |> member "to_state" |> to_string in
                            let write = t |> member "write" |> to_string in
                            let action = t |> member "action" |> to_string in
                            (read, to_state, write, action))
                        |> Array.of_list
                      in
                      StateHashtbl.add ht state transition_array);
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
