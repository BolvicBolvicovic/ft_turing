module StateHashtbl = Hashtbl.Make(struct
        type t = string
        let equal = String.equal
        let hash = Hashtbl.hash
end)

module type INPUT = sig
        type transition = string * string * string * string
        val alphabet : string list
        val blank : string
        val states : string list
        val initial : string
        val finals : string list
        val transitions : (transition array) StateHashtbl.t
end

module type Machine = sig

        type action =
                | LEFT
                | RIGHT
        type transition = string * string * string * string
        type state = string

        val alphabet : string list
        val blank : string
        val states : state list
        val initial : state
        val finals : state list
        val transitions : (transition array) StateHashtbl.t
        val execute : string -> unit
        val compute : string -> state -> int -> unit
        val process : string -> state -> int -> string * state * int  

end

module type MAKE = functor (I: INPUT) -> Machine

module Make : MAKE = functor (I: INPUT) -> struct
        
        type action =
                | LEFT
                | RIGHT
        type transition = string * string * string * string
        type state = string

        let alphabet = I.alphabet
        let blank = I.blank
        let states = I.states
        let initial = I.initial
        let finals = I.finals
        let transitions = I.transitions
        let process str_input state head =
                let formated_str = String.fold_left (fun acc element -> acc @ if List.length acc - 1 = head then ['<';element;'>'] else [element]) [] str_input in
                let formated_str = "[" ^ String.of_seq (List.to_seq formated_str) ^ "]" in
                print_endline formated_str;
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
                let new_str = String.fold_left (fun acc element -> acc @ if List.length acc - 1 = head then [write.[0]] else [element]) [] str_input in
                let new_str = String.of_seq (List.to_seq new_str) in
                print_endline ("(" ^ new_state ^ ", " ^ write ^ ", " ^ action ^ ")");
                (new_str, new_state, new_head)
        let rec compute str_input state head = 
                if List.exists (fun e -> e = state) finals then () 
                else
                        let (new_input, new_state, new_head) = process str_input state head in
                        compute new_input new_state new_head
        let execute str_input = compute str_input initial 0
end
