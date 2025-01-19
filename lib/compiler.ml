type foundation =
        | Inputs of string
        | Assign_op
        | Write of string
        | Register of string
        | Int of string
        | Callee of string
        | Action of string
        | Eq_res of string
        | Null

type operator =
        | Subroutine of (foundation * foundation * foundation)  (* Callee * Register  * Null or Input or Int *)
        | Eq of (operator * foundation * operator * operator)   (* Callee * Register  * Input or Int * Routine * Routine *)
        | Normal of (operator * operator)                       (* Subroutine or Null * Routine *)
        | Routine of (foundation * foundation)                  (* Action * Callee *)
        | Null

type line = {
        inputs : foundation;
        write  : foundation;
        next   : operator;
}

type func = {
        name : string;
        starts_at : int;
        definition : (line * int) list;
}

type transition = string * (string * string) array list
        
type tm_json = {
        file_name : string;
        alphabet : string array;
        blank : string;
        states : string array;
        transitions : transition list;
}


let to_list file_name = 
        let trimmed_file = open_in file_name 
                |> In_channel.input_lines 
                |> List.mapi (fun i str -> (str, (i + 1)))
                |> List.map (fun (str, i) -> (List.hd (String.split_on_char '#' str), i)) 
                |> List.filter (fun (str, _) -> "" <> str)
        in
        let (str_alphabet, i) = List.hd trimmed_file in
        let trimmed_file = 
                if String.starts_with ~prefix:"alphabet[" str_alphabet && String.ends_with ~suffix:"]" str_alphabet then trimmed_file
                else invalid_arg ("alphabet is missing or incorrectly formated at start of the file.\nAt line " ^ string_of_int i ^ ": '" ^ str_alphabet ^ "' should be of the form alphabet[01abc]")
        in
        let trimmed_file = if List.exists (fun (str, _) -> String.starts_with ~prefix:"_start:" str) trimmed_file then trimmed_file
                else invalid_arg ("_start function missing or not at begining of its line.")
        in
        let file_name = file_name 
                |> String.split_on_char '/'
                |> List.rev |> List.hd
                |> String.split_on_char '.'
                |> List.hd
        in
        (file_name, List.hd trimmed_file, List.tl trimmed_file)

let to_funcs (file_name, alphabet, trimmed_file) =
        let func_names = List.filter (fun (line, _) -> String.ends_with ~suffix:":" line) trimmed_file in
        let split_list_on_func_names list =
                let rec tail acc current = function
                        | [] -> List.rev (if List.is_empty current then acc else (List.rev current :: acc))
                        | h :: t ->
                                if String.ends_with ~suffix:":" (fst h) then 
                                        if List.is_empty current then tail acc [] t else tail (List.rev current :: acc) [] t
                                else
                                        tail acc (h :: current) t
                in tail [] [] list
        in
        let parsed_func = try
                let parsed_func = trimmed_file 
                        |> split_list_on_func_names 
                        |> List.fold_left2 (fun acc name definition -> (name, definition) :: acc) [] func_names
                in List.rev parsed_func
        with e ->
                print_endline "func_names:";
                func_names |> List.iter (fun (line, i) -> print_endline ("At line " ^ string_of_int i ^ ": " ^ line));
                print_endline "parsed_func:";
                split_list_on_func_names trimmed_file
                        |> List.iter
                                (fun list -> print_endline "def"; List.iter (fun (line, i) -> print_endline ("At line " ^ string_of_int i ^ ": " ^ line)) list);
                print_endline "Number of function name and function definition do not match.";
                print_endline "Or two function name follow each other";
                raise e
        in
        let tokenized_func = List.map (fun (name, definition) -> 
                        let tokenized_def = List.map (fun (line, i) ->
                                let trimmed_line = line
                                        |> String.trim 
                                        |> String.split_on_char ' ' 
                                        |> List.filter (fun str -> str <> "") in
                                let new_line = trimmed_line
                                        |> List.mapi (fun j word -> match j with
                                                | 0 -> if String.starts_with ~prefix:"[" word && String.ends_with ~suffix:"]" word then
                                                                Inputs (List.hd (String.split_on_char ']' (List.hd (String.split_on_char '[' word))))
                                                        else 
                                                                raise (Invalid_argument ("Error at line " ^ string_of_int i ^ ": '" ^ word ^ "' should be of form [x] and placed first"))
                                                | 1 -> if word = "<-" then
                                                                Assign_op
                                                        else
                                                                raise (Invalid_argument ("Error at line " ^ string_of_int i ^ ": '" ^ word ^ "' should be '<-' which is always placed second"))
                                                | 2 -> if word = "self" || (String.length word = 1 && String.contains (List.hd trimmed_line) word.[0]) then
                                                                Write word
                                                        else
                                                                raise (Invalid_argument ("Error at line " ^ string_of_int i ^ ": '" ^ word ^ "' should be either 'self' or part of '" ^ List.hd trimmed_line ^ "'"))
                                                | 3 -> (
                                                        match word with
                                                        | "then" -> Null
                                                        | "eq"  | "mov" | "inc" | "dec" -> Callee word
                                                        | _      -> raise (Invalid_argument ("Error at line " ^ string_of_int i ^ ": '" ^ word ^ "' should be either 'then' or a register operation."))
                                                )
                                                | 4 -> (
                                                        match word with
                                                        | "RIGHT" | "LEFT" | "STILL" -> Action word
                                                        | "eax" | "ebx" | "ecx" | "edx" -> Register word
                                                        | _      -> raise (Invalid_argument ("Error at line " ^ string_of_int i ^ ": '" ^ word ^ "' should be either an Action or a Register."))
                                                )
                                                | 5 -> if (String.starts_with ~prefix:"int(" word && String.ends_with ~suffix:")" word) then
                                                                Int (List.hd (List.rev (String.split_on_char '(' (List.hd (String.split_on_char ')' word)))))
                                                        else if word = "self" || (String.length word = 1 && String.contains (List.hd trimmed_line) word.[0]) then
                                                                Inputs word
                                                        else if word = "and" || word = "then" then
                                                                Null
                                                        else
                                                                raise (Invalid_argument ("Error at line " ^ string_of_int i ^ ": '" ^ word ^ "' should be a valid input or 'and'."))
                                                | 6 -> (
                                                        match word with
                                                        | ":true" | ":false" -> Eq_res word
                                                        | "RIGHT" | "LEFT" | "STILL" -> Action word
                                                        | "then" -> Null
                                                        | foo -> Callee foo
                                                )
                                                | 7 -> (
                                                        match word with
                                                        | "RIGHT" | "LEFT" | "STILL" -> Action word
                                                        | "then" | "and" -> Null
                                                        | _      -> raise (Invalid_argument ("Error at line " ^ string_of_int i ^ ": '" ^ word ^ "' should be either an Action or a 'then' or 'and'."))
                                                )
                                                | 8 -> (
                                                        match word with
                                                        | "RIGHT" | "LEFT" | "STILL" -> Action word
                                                        | "and" -> Null
                                                        | foo -> Callee foo
                                                )
                                                | 9 -> (
                                                        match word with
                                                        | "and" -> Null
                                                        | foo -> Callee foo
                                                )
                                                | 10 -> Callee word
                                                | 11 -> Null 
                                                | 12 -> if word = "RIGHT" || word = "LEFT" || word = "STILL" then Action word 
                                                        else raise (Invalid_argument ("Error at line " ^ string_of_int i ^ ": '" ^ word ^ "' should not exists."))
                                                | 13 -> Null
                                                | 14 -> Callee word

                                                | _      -> raise (Invalid_argument ("Error at line " ^ string_of_int i ^ ": '" ^ word ^ "' should not exists."))
                                                                
                                        ) in (new_line, i) 
                        ) definition in
                        (name, tokenized_def)
                ) parsed_func
        in 
        let parsed_operator = List.map (fun ((name, j), definition) ->
                let parsed_def = List.map (fun (line, i) ->
                                let arr = Array.of_list line in
                                let operators = match Array.length arr with 
                                | 7 -> {
                                        inputs = arr.(0);
                                        write  = arr.(2);
                                        next   = Normal (Null, Routine (arr.(4), arr.(6)))
                                }
                                | 9 -> {
                                        inputs = arr.(0);
                                        write  = arr.(2);
                                        next   = Normal (Subroutine (arr.(3), arr.(4), Null), Routine (arr.(6), arr.(8)));
                                }
                                | 10 -> {
                                        inputs = arr.(0);
                                        write  = arr.(2);
                                        next   = Normal (Subroutine (arr.(3), arr.(4), arr.(5)), Routine (arr.(7), arr.(9)))
                                }
                                | 15 -> {
                                        inputs = arr.(0);
                                        write  = arr.(2);
                                        next   = Eq (Subroutine (arr.(3), arr.(4), arr.(5)), arr.(6), Routine (arr.(8), arr.(10)), Routine (arr.(12), arr.(14)))
                                }
                                | _      -> raise (Invalid_argument ("Error at line " ^ string_of_int i ^ ": '" ^ string_of_int (List.length definition) ^ "' should not exists."))
                                in (operators, i)
                ) definition in
                { name = String.sub (String.trim name) 0 (String.length (String.trim name)); starts_at = j; definition = parsed_def }
        ) tokenized_func
        in
        (file_name, alphabet, parsed_operator)




let to_transitions (line, i) =
        let inputs = match line.inputs with
                | Inputs str -> String.fold_left (fun acc c -> String.make 1 c :: acc ) [] str
                | _ -> raise (Invalid_argument ("Error at line " ^ string_of_int i ^ ": Incorrect input"))
        in
        let state = match line.next with 
                | Normal (Null, Routine (_, Callee state)) -> state
                | _ -> raise (Invalid_argument ("Error at line " ^ string_of_int i ^ ": Incorrect state"))
        in
        let write = match line.write with
                | Write w -> w
                | _ -> raise (Invalid_argument ("Error at line " ^ string_of_int i ^ ": Incorrect write"))
        in
        let action = match line.next with 
                | Normal (Null, Routine (Action action, _)) -> action
                | _ -> raise (Invalid_argument ("Error at line " ^ string_of_int i ^ ": Incorrect action"))
        in
        inputs |> List.map (fun input -> [|("read", input); ("to_state", state); ("write", write); ("action", action);|])

let parse_func as_funcs = List.map (fun func -> (func.name, List.concat (List.map to_transitions func.definition))) as_funcs

let to_tm_json (file_name, (alphabet, _), as_tree) = 
        let parsed_alphabet = String.fold_left (fun acc c -> String.make 1 c :: acc ) [] alphabet |> List.rev in
        let parsed_state_names = List.map (fun func -> func.name) as_tree in
        let parsed_transitions = parse_func as_tree in
        {
                file_name = file_name;
                alphabet = Array.of_list parsed_alphabet;
                blank = List.hd parsed_alphabet;
                states = Array.of_list parsed_state_names;
                transitions = parsed_transitions;
        }

let to_file tm_json =
        let string_of_array (arr: string array) = 
                let new_string = Array.fold_left (fun acc str -> acc ^ "'" ^ str ^ "', ") "" arr in
                String.sub new_string 0 (String.length new_string - 2)
        in
        let string_of_transition (name, definition) =
"'" ^ name ^ "' : {
                " ^ (let new_list = List.map (fun arr -> "{ 'read' : '" ^ snd arr.(0) ^ "', 'to_state' : '" ^ snd arr.(1) ^ "', 'write' : '" ^ snd arr.(2) ^ "', 'action' : '" ^ snd arr.(3) ^ "' }, ") definition in
                        let new_string = List.fold_left (fun acc str -> acc ^ str) "" new_list in
                        print_endline ("For " ^ name ^ ": new_string: <" ^ new_string ^ ">");
                        String.sub new_string 0 (String.length new_string - 2)
                ) ^ "
        }, "
in

"{
        'name'          : '" ^ tm_json.file_name ^ "',
        'alphabet'      : [" ^ string_of_array tm_json.alphabet ^ "],
        'blank'         : '" ^ tm_json.blank ^ "',
        'states'        : [" ^ string_of_array tm_json.states ^ "],
        'initial'       : '_start',
        'finals'        : ['HALT'],
        'transitions'   : {
                " ^ (let new_string =
                        List.map string_of_transition tm_json.transitions |> List.fold_left (fun acc str -> acc ^ str) "" in
                        String.sub new_string 0 (String.length new_string - 2)
                )                        
                ^ "
        }
}"

let compile file_name = file_name 
        |> to_list 
        |> to_funcs
        |> to_tm_json 
        |> to_file
        |> print_endline
