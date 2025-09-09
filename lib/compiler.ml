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
  | Subroutine of (foundation * foundation * foundation)
    (* Callee * Register  * Int *)
  | Eq of (operator * foundation * operator * operator)
    (* Subroutine * Eq_res * Routine * Routine *)
  | Normal of (operator * operator) (* Subroutine or Null * Routine *)
  | Routine of (foundation * foundation) (* Action * Callee *)
  | Null

type line = { inputs : foundation; write : foundation; next : operator }
type func = { name : string; starts_at : int; definition : (line * int) list }
type transition = string * (string * string) array list

type tm_json = {
  file_name : string;
  alphabet : string array;
  blank : string;
  states : string array;
  transitions : transition list;
}

let to_list file_name =
  let trimmed_file =
    open_in file_name |> In_channel.input_lines
    |> List.mapi (fun i str -> (str, i + 1))
    |> List.map (fun (str, i) -> (List.hd (String.split_on_char '#' str), i))
    |> List.filter (fun (str, _) -> "" <> str)
  in
  let str_alphabet, i = List.hd trimmed_file in
  let trimmed_file =
    if
      String.starts_with ~prefix:"alphabet[" str_alphabet
      && String.ends_with ~suffix:"]" str_alphabet
    then trimmed_file
    else
      invalid_arg
        ("alphabet is missing or incorrectly formated at start of the file.\n\
          At line " ^ string_of_int i ^ ": '" ^ str_alphabet
       ^ "' should be of the form alphabet[01abc]")
  in
  let trimmed_file =
    if
      List.exists
        (fun (str, _) ->
          String.starts_with ~prefix:"_start:" str
          || String.starts_with ~prefix:"_start_mem:" str)
        trimmed_file
    then trimmed_file
    else
      invalid_arg
        "_start or _start_mem function missing or not at begining of its line."
  in
  let file_name =
    file_name |> String.split_on_char '/' |> List.rev |> List.hd
    |> String.split_on_char '.' |> List.hd
  in
  let alphabet, _ = List.hd trimmed_file in
  let parsed_alphabet =
    String.sub alphabet
      (String.index alphabet '[' + 1)
      (String.rindex alphabet ']' - String.index alphabet '[' - 1)
  in
  if String.length parsed_alphabet = 0 then
    raise
      (Invalid_argument ("Error in file " ^ file_name ^ ".s: alphabet is empty."))
  else
    ( file_name,
      parsed_alphabet,
      List.tl trimmed_file |> List.filter (fun (str, _) -> str <> "") )

let to_funcs (file_name, alphabet, trimmed_file) =
  let mem_opt =
    if
      List.exists
        (fun (str, _) -> String.starts_with ~prefix:"_start_mem:" str)
        trimmed_file
      && not
           (List.exists
              (fun (str, _) -> String.starts_with ~prefix:"_start:" str)
              trimmed_file)
    then [| true |]
    else [| false |]
  in
  let func_names =
    List.filter
      (fun (line, _) -> String.ends_with ~suffix:":" line)
      trimmed_file
  in
  let split_list_on_func_names list =
    let rec aux acc current = function
      | [] ->
          List.rev
            (if List.is_empty current then acc else List.rev current :: acc)
      | h :: t ->
          if String.ends_with ~suffix:":" (fst h) then
            if List.is_empty current then aux acc [] t
            else aux (List.rev current :: acc) [] t
          else aux acc (h :: current) t
    in
    aux [] [] list
  in
  let parsed_func =
    try
      let parsed_func =
        trimmed_file |> split_list_on_func_names
        |> List.fold_left2
             (fun acc name definition -> (name, definition) :: acc)
             [] func_names
      in
      List.rev parsed_func
    with e ->
      print_endline
        "Number of function name and function definition do not match.";
      print_endline "Or two function name follow each other";
      raise e
  in
  let tokenized_func =
    List.map
      (fun (name, definition) ->
        let tokenized_def =
          List.map
            (fun (line, i) ->
              let trimmed_line =
                line |> String.trim |> String.split_on_char ' '
                |> List.filter (fun str -> str <> "")
              in
              let new_line =
                trimmed_line
                |> List.mapi (fun j word ->
                       match j with
                       | 0 ->
                           if
                             String.starts_with ~prefix:"[" word
                             && String.ends_with ~suffix:"]" word
                           then
                             Inputs (String.sub word 1 (String.length word - 2))
                           else
                             raise
                               (Invalid_argument
                                  ("Error at line " ^ string_of_int i ^ ": '"
                                 ^ word
                                 ^ "' should be of form [x] and placed first"))
                       | 1 ->
                           if word = "<-" then Assign_op
                           else
                             raise
                               (Invalid_argument
                                  ("Error at line " ^ string_of_int i ^ ": '"
                                 ^ word
                                 ^ "' should be '<-' which is always placed \
                                    second"))
                       | 2 ->
                           if
                             word = "self"
                             || String.length word = 1
                                && String.contains alphabet word.[0]
                           then Write word
                           else
                             raise
                               (Invalid_argument
                                  ("Error at line " ^ string_of_int i ^ ": '"
                                 ^ word
                                 ^ "' should be either 'self' or part of \
                                    alphabet '" ^ alphabet ^ "'"))
                       | 3 -> (
                           match word with
                           | "then" -> Null
                           | "eq" | "mov" | "inc" | "dec" -> Callee word
                           | _ ->
                               raise
                                 (Invalid_argument
                                    ("Error at line " ^ string_of_int i ^ ": '"
                                   ^ word
                                   ^ "' should be either 'then' or a register \
                                      operation.")))
                       | 4 -> (
                           match word with
                           | "RIGHT" | "LEFT" -> Action word
                           | "eax" | "ebx" | "ecx" | "edx" -> Register word
                           | _ ->
                               raise
                                 (Invalid_argument
                                    ("Error at line " ^ string_of_int i ^ ": '"
                                   ^ word
                                   ^ "' should be either an Action or a \
                                      Register.")))
                       | 5 ->
                           if
                             String.starts_with ~prefix:"int(" word
                             && String.ends_with ~suffix:")" word
                           then
                             Int
                               (String.sub word
                                  (String.index word '(' + 1)
                                  (String.rindex word ')'
                                 - String.index word '(' - 1))
                           else if
                             word = "self"
                             || String.length word = 1
                                && String.contains alphabet word.[0]
                           then Inputs word
                           else if word = "and" || word = "then" then Null
                           else
                             raise
                               (Invalid_argument
                                  ("Error at line " ^ string_of_int i ^ ": '"
                                 ^ word ^ "' should be a valid input or 'and'."
                                  ))
                       | 6 -> (
                           match word with
                           | ":true" | ":false" -> Eq_res word
                           | "RIGHT" | "LEFT" -> Action word
                           | "then" -> Null
                           | foo -> Callee foo)
                       | 7 -> (
                           match word with
                           | "RIGHT" | "LEFT" -> Action word
                           | "then" | "and" -> Null
                           | _ ->
                               raise
                                 (Invalid_argument
                                    ("Error at line " ^ string_of_int i ^ ": '"
                                   ^ word
                                   ^ "' should be either an Action or a 'then' \
                                      or 'and'.")))
                       | 8 -> (
                           match word with
                           | "RIGHT" | "LEFT" -> Action word
                           | "and" -> Null
                           | foo -> Callee foo)
                       | 9 -> (
                           match word with "and" -> Null | foo -> Callee foo)
                       | 10 -> Callee word
                       | 11 -> Null
                       | 12 ->
                           if word = "RIGHT" || word = "LEFT" then Action word
                           else
                             raise
                               (Invalid_argument
                                  ("Error at line " ^ string_of_int i ^ ": '"
                                 ^ word ^ "' should not exists."))
                       | 13 -> Null
                       | 14 -> Callee word
                       | _ ->
                           raise
                             (Invalid_argument
                                ("Error at line " ^ string_of_int i ^ ": '"
                               ^ word ^ "' should not exists.")))
              in
              (new_line, i))
            definition
        in
        (name, tokenized_def))
      parsed_func
  in
  let parsed_operator =
    List.map
      (fun ((name, j), definition) ->
        let parsed_def =
          List.map
            (fun (line, i) ->
              let arr = Array.of_list line in
              let operators =
                match Array.length arr with
                | 7 ->
                    {
                      inputs = arr.(0);
                      write = arr.(2);
                      next = Normal (Null, Routine (arr.(4), arr.(6)));
                    }
                | 9 ->
                    {
                      inputs = arr.(0);
                      write =
                        (if arr.(2) = Write "self" then
                           raise
                             (Invalid_argument
                                ("Error at line " ^ string_of_int i
                               ^ ": 'self' cannot be used when a memory \
                                  instruction is also used."))
                         else arr.(2));
                      next =
                        Normal
                          ( Subroutine (arr.(3), arr.(4), Null),
                            Routine (arr.(6), arr.(8)) );
                    }
                | 10 ->
                    {
                      inputs = arr.(0);
                      write =
                        (if arr.(2) = Write "self" then
                           raise
                             (Invalid_argument
                                ("Error at line " ^ string_of_int i
                               ^ ": 'self' cannot be used when a memory \
                                  instruction is also used."))
                         else arr.(2));
                      next =
                        Normal
                          ( Subroutine (arr.(3), arr.(4), arr.(5)),
                            Routine (arr.(7), arr.(9)) );
                    }
                | 15 ->
                    {
                      inputs = arr.(0);
                      write =
                        (if arr.(2) = Write "self" then
                           raise
                             (Invalid_argument
                                ("Error at line " ^ string_of_int i
                               ^ ": 'self' cannot be used when a memory \
                                  instruction is also used."))
                         else arr.(2));
                      next =
                        Eq
                          ( Subroutine (arr.(3), arr.(4), arr.(5)),
                            arr.(6),
                            Routine (arr.(8), arr.(10)),
                            Routine (arr.(12), arr.(14)) );
                    }
                | _ ->
                    raise
                      (Invalid_argument
                         ("Error at line " ^ string_of_int i ^ ": '"
                         ^ string_of_int (List.length definition)
                         ^ "' should not exists."))
              in
              (operators, i))
            definition
        in
        {
          name =
            String.sub (String.trim name) 0
              (String.length (String.trim name) - 1);
          starts_at = j;
          definition = parsed_def;
        })
      tokenized_func
  in
  let update_alpha alpha =
    let mem_alphabet = "#0123456789abcdefABCD" in
    alpha
    ^ String.fold_left
        (fun acc c ->
          if String.contains alpha c then acc else acc ^ String.make 1 c)
        "" mem_alphabet
  in
  let updated_alphabet =
    if mem_opt.(0) then update_alpha alphabet else alphabet
  in
  let init_mem ope =
    let sub_alpha =
      String.sub updated_alphabet 1 (String.length updated_alphabet - 1)
    in
    let blank = String.make 1 alphabet.[0] in
    List.append
      [
        {
          name = "_start";
          starts_at = -1;
          definition =
            [
              ( {
                  inputs = Inputs "#";
                  write = Write "self";
                  next =
                    Normal
                      (Null, Routine (Action "RIGHT", Callee "reach_mem_start"));
                },
                -1 );
            ];
        };
        {
          name = "reach_mem_start";
          starts_at = -1;
          definition =
            [
              ( {
                  inputs = Inputs sub_alpha;
                  write = Write "self";
                  next =
                    Normal
                      (Null, Routine (Action "RIGHT", Callee "reach_mem_start"));
                },
                -1 );
              ( {
                  inputs = Inputs blank;
                  write = Write "self";
                  next =
                    Normal
                      (Null, Routine (Action "RIGHT", Callee "start_of_eax"));
                },
                -1 );
            ];
        };
        {
          name = "start_of_eax";
          starts_at = -1;
          definition =
            [
              ( {
                  inputs = Inputs blank;
                  write = Write "A";
                  next =
                    Normal
                      (Null, Routine (Action "RIGHT", Callee "start_of_ebx0"));
                },
                -1 );
            ];
        };
        {
          name = "start_of_ebx0";
          starts_at = -1;
          definition =
            [
              ( {
                  inputs = Inputs blank;
                  write = Write "0";
                  next =
                    Normal
                      (Null, Routine (Action "RIGHT", Callee "start_of_ebx1"));
                },
                -1 );
            ];
        };
        {
          name = "start_of_ebx1";
          starts_at = -1;
          definition =
            [
              ( {
                  inputs = Inputs blank;
                  write = Write "0";
                  next =
                    Normal
                      (Null, Routine (Action "RIGHT", Callee "start_of_ebx2"));
                },
                -1 );
            ];
        };
        {
          name = "start_of_ebx2";
          starts_at = -1;
          definition =
            [
              ( {
                  inputs = Inputs blank;
                  write = Write "0";
                  next =
                    Normal
                      (Null, Routine (Action "RIGHT", Callee "start_of_ebx3"));
                },
                -1 );
            ];
        };
        {
          name = "start_of_ebx3";
          starts_at = -1;
          definition =
            [
              ( {
                  inputs = Inputs blank;
                  write = Write "0";
                  next =
                    Normal
                      (Null, Routine (Action "RIGHT", Callee "start_of_ebx4"));
                },
                -1 );
            ];
        };
        {
          name = "start_of_ebx4";
          starts_at = -1;
          definition =
            [
              ( {
                  inputs = Inputs blank;
                  write = Write "B";
                  next =
                    Normal
                      (Null, Routine (Action "RIGHT", Callee "start_of_ecx0"));
                },
                -1 );
            ];
        };
        {
          name = "start_of_ecx0";
          starts_at = -1;
          definition =
            [
              ( {
                  inputs = Inputs blank;
                  write = Write "0";
                  next =
                    Normal
                      (Null, Routine (Action "RIGHT", Callee "start_of_ecx1"));
                },
                -1 );
            ];
        };
        {
          name = "start_of_ecx1";
          starts_at = -1;
          definition =
            [
              ( {
                  inputs = Inputs blank;
                  write = Write "0";
                  next =
                    Normal
                      (Null, Routine (Action "RIGHT", Callee "start_of_ecx2"));
                },
                -1 );
            ];
        };
        {
          name = "start_of_ecx2";
          starts_at = -1;
          definition =
            [
              ( {
                  inputs = Inputs blank;
                  write = Write "0";
                  next =
                    Normal
                      (Null, Routine (Action "RIGHT", Callee "start_of_ecx3"));
                },
                -1 );
            ];
        };
        {
          name = "start_of_ecx3";
          starts_at = -1;
          definition =
            [
              ( {
                  inputs = Inputs blank;
                  write = Write "0";
                  next =
                    Normal
                      (Null, Routine (Action "RIGHT", Callee "start_of_ecx4"));
                },
                -1 );
            ];
        };
        {
          name = "start_of_ecx4";
          starts_at = -1;
          definition =
            [
              ( {
                  inputs = Inputs blank;
                  write = Write "C";
                  next =
                    Normal
                      (Null, Routine (Action "RIGHT", Callee "start_of_edx0"));
                },
                -1 );
            ];
        };
        {
          name = "start_of_edx0";
          starts_at = -1;
          definition =
            [
              ( {
                  inputs = Inputs blank;
                  write = Write "0";
                  next =
                    Normal
                      (Null, Routine (Action "RIGHT", Callee "start_of_edx1"));
                },
                -1 );
            ];
        };
        {
          name = "start_of_edx1";
          starts_at = -1;
          definition =
            [
              ( {
                  inputs = Inputs blank;
                  write = Write "0";
                  next =
                    Normal
                      (Null, Routine (Action "RIGHT", Callee "start_of_edx2"));
                },
                -1 );
            ];
        };
        {
          name = "start_of_edx2";
          starts_at = -1;
          definition =
            [
              ( {
                  inputs = Inputs blank;
                  write = Write "0";
                  next =
                    Normal
                      (Null, Routine (Action "RIGHT", Callee "start_of_edx3"));
                },
                -1 );
            ];
        };
        {
          name = "start_of_edx3";
          starts_at = -1;
          definition =
            [
              ( {
                  inputs = Inputs blank;
                  write = Write "0";
                  next =
                    Normal
                      (Null, Routine (Action "RIGHT", Callee "start_of_edx4"));
                },
                -1 );
            ];
        };
        {
          name = "start_of_edx4";
          starts_at = -1;
          definition =
            [
              ( {
                  inputs = Inputs blank;
                  write = Write "D";
                  next =
                    Normal (Null, Routine (Action "RIGHT", Callee "end_of_mem0"));
                },
                -1 );
            ];
        };
        {
          name = "end_of_mem0";
          starts_at = -1;
          definition =
            [
              ( {
                  inputs = Inputs blank;
                  write = Write "0";
                  next =
                    Normal (Null, Routine (Action "RIGHT", Callee "end_of_mem1"));
                },
                -1 );
            ];
        };
        {
          name = "end_of_mem1";
          starts_at = -1;
          definition =
            [
              ( {
                  inputs = Inputs blank;
                  write = Write "0";
                  next =
                    Normal (Null, Routine (Action "RIGHT", Callee "end_of_mem2"));
                },
                -1 );
            ];
        };
        {
          name = "end_of_mem2";
          starts_at = -1;
          definition =
            [
              ( {
                  inputs = Inputs blank;
                  write = Write "0";
                  next =
                    Normal (Null, Routine (Action "RIGHT", Callee "end_of_mem3"));
                },
                -1 );
            ];
        };
        {
          name = "end_of_mem3";
          starts_at = -1;
          definition =
            [
              ( {
                  inputs = Inputs blank;
                  write = Write "0";
                  next =
                    Normal
                      (Null, Routine (Action "LEFT", Callee "back_prog_start"));
                },
                -1 );
            ];
        };
        {
          name = "back_prog_start";
          starts_at = -1;
          definition =
            [
              ( {
                  inputs =
                    Inputs
                      (String.fold_left
                         (fun acc c ->
                           if c = '#' then acc else acc ^ String.make 1 c)
                         "" updated_alphabet);
                  write = Write "self";
                  next =
                    Normal
                      (Null, Routine (Action "LEFT", Callee "back_prog_start"));
                },
                -1 );
              ( {
                  inputs = Inputs "#";
                  write = Write "self";
                  next =
                    Normal (Null, Routine (Action "RIGHT", Callee "_start_mem"));
                },
                -1 );
            ];
        };
      ]
      ope
  in
  (* TODO: build the memmove paterns -> issue with Eq to be solved *)
  let build_funcs func =
    let func_def_begin =
      List.map
        (fun (line, i) ->
          match line.next with
          | Normal (Null, _) -> (line, i)
          | Normal
              ( Subroutine (Callee c0, Register r, _),
                Routine (Action a, Callee c1) ) ->
              let w = match line.write with Write str -> str | _ -> "" in
              let int_value =
                match line.next with
                | Normal (Subroutine (_, _, Int v), _) -> v
                | _ -> ""
              in
              ( {
                  inputs = line.inputs;
                  write = Write "#";
                  next =
                    Normal
                      ( Null,
                        Routine
                          ( Action "RIGHT",
                            Callee
                              (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0 ^ "_" ^ r
                             ^ int_value) ) );
                },
                i )
          | Eq
              ( Subroutine (Callee c0, Register r, Int nb),
                Eq_res bool,
                Routine (Action a1, Callee c1),
                Routine (Action a2, Callee c2) ) ->
              let w = match line.write with Write str -> str | _ -> "" in
              ( {
                  inputs = line.inputs;
                  write = Write "#";
                  next =
                    Normal
                      ( Null,
                        Routine
                          ( Action "RIGHT",
                            Callee
                              (w ^ "_" ^ a1 ^ "_" ^ c1 ^ "_or_" ^ a2 ^ "_" ^ c2
                             ^ "_if_" ^ bool ^ ":" ^ c0 ^ "_" ^ r ^ "_" ^ nb) )
                      );
                },
                i )
          | _ ->
              raise
                (Invalid_argument
                   ("Error at line " ^ string_of_int i ^ ": Incorrect input")))
        func.definition
    in
    let func_begin =
      {
        name = func.name;
        starts_at = func.starts_at;
        definition = func_def_begin;
      }
    in
    let alphabet_without_tag =
      String.fold_left
        (fun acc c -> if c = '#' then acc else acc ^ String.make 1 c)
        "" updated_alphabet
    in
    let funcs_subroutines =
      List.map
        (fun (line, i) ->
          let w = match line.write with Write str -> str | _ -> "" in
          match line.next with
          | Normal
              ( Subroutine (Callee c0, Register r, _),
                Routine (Action a, Callee c1) ) ->
              let int_value =
                match line.next with
                | Normal (Subroutine (_, _, Int v), _) ->
                    if String.for_all (String.contains "0123456789abcdef") v
                    then v
                    else
                      raise
                        (Invalid_argument
                           ("Error at line " ^ string_of_int i ^ ": '" ^ v
                          ^ "' is not a valid hexa. Hexa needs to be of the \
                             form 0000. Example: 04a6"))
                | _ -> ""
              in
              let register =
                match r with
                | "eax" -> 'A'
                | "ebx" -> 'B'
                | "ecx" -> 'C'
                | "edx" -> 'D'
                | _ ->
                    raise
                      (Invalid_argument
                         ("Error at line " ^ string_of_int i ^ ": '" ^ r
                        ^ "' is not a valid register."))
              in
              let alphabet_without_register =
                String.fold_left
                  (fun acc c ->
                    if c = register then acc else acc ^ String.make 1 c)
                  "" updated_alphabet
              in
              let func_sub_heart =
                match c0 with
                | "inc" ->
                    [
                      {
                        name =
                          w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0 ^ "_" ^ r
                          ^ int_value ^ "_begin";
                        starts_at = -1;
                        definition =
                          [
                            ( {
                                inputs = Inputs "0123456789abcdef";
                                write = Write "self";
                                next =
                                  Normal
                                    ( Null,
                                      Routine
                                        ( Action "RIGHT",
                                          Callee
                                            (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0
                                           ^ "_" ^ r ^ int_value ^ "_1") ) );
                              },
                              -1 );
                          ];
                      };
                      {
                        name =
                          w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0 ^ "_" ^ r
                          ^ int_value ^ "_1";
                        starts_at = -1;
                        definition =
                          [
                            ( {
                                inputs = Inputs "0123456789abcdef";
                                write = Write "self";
                                next =
                                  Normal
                                    ( Null,
                                      Routine
                                        ( Action "RIGHT",
                                          Callee
                                            (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0
                                           ^ "_" ^ r ^ int_value ^ "_2") ) );
                              },
                              -1 );
                          ];
                      };
                      {
                        name =
                          w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0 ^ "_" ^ r
                          ^ int_value ^ "_2";
                        starts_at = -1;
                        definition =
                          [
                            ( {
                                inputs = Inputs "0123456789abcdef";
                                write = Write "self";
                                next =
                                  Normal
                                    ( Null,
                                      Routine
                                        ( Action "RIGHT",
                                          Callee
                                            (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0
                                           ^ "_" ^ r ^ int_value ^ "_inc") ) );
                              },
                              -1 );
                          ];
                      };
                      {
                        name =
                          w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0 ^ "_" ^ r
                          ^ int_value ^ "_inc";
                        starts_at = -1;
                        definition =
                          [
                            ( {
                                inputs = Inputs "0";
                                write = Write "1";
                                next =
                                  Normal
                                    ( Null,
                                      Routine
                                        ( Action "LEFT",
                                          Callee
                                            (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0
                                           ^ "_" ^ r ^ int_value ^ "_return") )
                                    );
                              },
                              -1 );
                            ( {
                                inputs = Inputs "1";
                                write = Write "2";
                                next =
                                  Normal
                                    ( Null,
                                      Routine
                                        ( Action "LEFT",
                                          Callee
                                            (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0
                                           ^ "_" ^ r ^ int_value ^ "_return") )
                                    );
                              },
                              -1 );
                            ( {
                                inputs = Inputs "2";
                                write = Write "3";
                                next =
                                  Normal
                                    ( Null,
                                      Routine
                                        ( Action "LEFT",
                                          Callee
                                            (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0
                                           ^ "_" ^ r ^ int_value ^ "_return") )
                                    );
                              },
                              -1 );
                            ( {
                                inputs = Inputs "3";
                                write = Write "4";
                                next =
                                  Normal
                                    ( Null,
                                      Routine
                                        ( Action "LEFT",
                                          Callee
                                            (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0
                                           ^ "_" ^ r ^ int_value ^ "_return") )
                                    );
                              },
                              -1 );
                            ( {
                                inputs = Inputs "4";
                                write = Write "5";
                                next =
                                  Normal
                                    ( Null,
                                      Routine
                                        ( Action "LEFT",
                                          Callee
                                            (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0
                                           ^ "_" ^ r ^ int_value ^ "_return") )
                                    );
                              },
                              -1 );
                            ( {
                                inputs = Inputs "5";
                                write = Write "6";
                                next =
                                  Normal
                                    ( Null,
                                      Routine
                                        ( Action "LEFT",
                                          Callee
                                            (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0
                                           ^ "_" ^ r ^ int_value ^ "_return") )
                                    );
                              },
                              -1 );
                            ( {
                                inputs = Inputs "6";
                                write = Write "7";
                                next =
                                  Normal
                                    ( Null,
                                      Routine
                                        ( Action "LEFT",
                                          Callee
                                            (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0
                                           ^ "_" ^ r ^ int_value ^ "_return") )
                                    );
                              },
                              -1 );
                            ( {
                                inputs = Inputs "7";
                                write = Write "8";
                                next =
                                  Normal
                                    ( Null,
                                      Routine
                                        ( Action "LEFT",
                                          Callee
                                            (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0
                                           ^ "_" ^ r ^ int_value ^ "_return") )
                                    );
                              },
                              -1 );
                            ( {
                                inputs = Inputs "8";
                                write = Write "9";
                                next =
                                  Normal
                                    ( Null,
                                      Routine
                                        ( Action "LEFT",
                                          Callee
                                            (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0
                                           ^ "_" ^ r ^ int_value ^ "_return") )
                                    );
                              },
                              -1 );
                            ( {
                                inputs = Inputs "9";
                                write = Write "a";
                                next =
                                  Normal
                                    ( Null,
                                      Routine
                                        ( Action "LEFT",
                                          Callee
                                            (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0
                                           ^ "_" ^ r ^ int_value ^ "_return") )
                                    );
                              },
                              -1 );
                            ( {
                                inputs = Inputs "a";
                                write = Write "b";
                                next =
                                  Normal
                                    ( Null,
                                      Routine
                                        ( Action "LEFT",
                                          Callee
                                            (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0
                                           ^ "_" ^ r ^ int_value ^ "_return") )
                                    );
                              },
                              -1 );
                            ( {
                                inputs = Inputs "b";
                                write = Write "c";
                                next =
                                  Normal
                                    ( Null,
                                      Routine
                                        ( Action "LEFT",
                                          Callee
                                            (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0
                                           ^ "_" ^ r ^ int_value ^ "_return") )
                                    );
                              },
                              -1 );
                            ( {
                                inputs = Inputs "c";
                                write = Write "d";
                                next =
                                  Normal
                                    ( Null,
                                      Routine
                                        ( Action "LEFT",
                                          Callee
                                            (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0
                                           ^ "_" ^ r ^ int_value ^ "_return") )
                                    );
                              },
                              -1 );
                            ( {
                                inputs = Inputs "d";
                                write = Write "e";
                                next =
                                  Normal
                                    ( Null,
                                      Routine
                                        ( Action "LEFT",
                                          Callee
                                            (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0
                                           ^ "_" ^ r ^ int_value ^ "_return") )
                                    );
                              },
                              -1 );
                            ( {
                                inputs = Inputs "e";
                                write = Write "f";
                                next =
                                  Normal
                                    ( Null,
                                      Routine
                                        ( Action "LEFT",
                                          Callee
                                            (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0
                                           ^ "_" ^ r ^ int_value ^ "_return") )
                                    );
                              },
                              -1 );
                            ( {
                                inputs = Inputs "f";
                                write = Write "0";
                                next =
                                  Normal
                                    ( Null,
                                      Routine
                                        ( Action "LEFT",
                                          Callee
                                            (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0
                                           ^ "_" ^ r ^ int_value ^ "_inc") ) );
                              },
                              -1 );
                          ];
                      };
                    ]
                | "dec" ->
                    [
                      {
                        name =
                          w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0 ^ "_" ^ r
                          ^ int_value ^ "_begin";
                        starts_at = -1;
                        definition =
                          [
                            ( {
                                inputs = Inputs "0123456789abcdef";
                                write = Write "self";
                                next =
                                  Normal
                                    ( Null,
                                      Routine
                                        ( Action "RIGHT",
                                          Callee
                                            (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0
                                           ^ "_" ^ r ^ int_value ^ "_1") ) );
                              },
                              -1 );
                          ];
                      };
                      {
                        name =
                          w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0 ^ "_" ^ r
                          ^ int_value ^ "_1";
                        starts_at = -1;
                        definition =
                          [
                            ( {
                                inputs = Inputs "0123456789abcdef";
                                write = Write "self";
                                next =
                                  Normal
                                    ( Null,
                                      Routine
                                        ( Action "RIGHT",
                                          Callee
                                            (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0
                                           ^ "_" ^ r ^ int_value ^ "_2") ) );
                              },
                              -1 );
                          ];
                      };
                      {
                        name =
                          w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0 ^ "_" ^ r
                          ^ int_value ^ "_2";
                        starts_at = -1;
                        definition =
                          [
                            ( {
                                inputs = Inputs "0123456789abcdef";
                                write = Write "self";
                                next =
                                  Normal
                                    ( Null,
                                      Routine
                                        ( Action "RIGHT",
                                          Callee
                                            (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0
                                           ^ "_" ^ r ^ int_value ^ "_inc") ) );
                              },
                              -1 );
                          ];
                      };
                      {
                        name =
                          w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0 ^ "_" ^ r
                          ^ int_value ^ "_inc";
                        starts_at = -1;
                        definition =
                          [
                            ( {
                                inputs = Inputs "0";
                                write = Write "f";
                                next =
                                  Normal
                                    ( Null,
                                      Routine
                                        ( Action "LEFT",
                                          Callee
                                            (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0
                                           ^ "_" ^ r ^ int_value ^ "_dec") ) );
                              },
                              -1 );
                            ( {
                                inputs = Inputs "1";
                                write = Write "0";
                                next =
                                  Normal
                                    ( Null,
                                      Routine
                                        ( Action "LEFT",
                                          Callee
                                            (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0
                                           ^ "_" ^ r ^ int_value ^ "_return") )
                                    );
                              },
                              -1 );
                            ( {
                                inputs = Inputs "2";
                                write = Write "1";
                                next =
                                  Normal
                                    ( Null,
                                      Routine
                                        ( Action "LEFT",
                                          Callee
                                            (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0
                                           ^ "_" ^ r ^ int_value ^ "_return") )
                                    );
                              },
                              -1 );
                            ( {
                                inputs = Inputs "3";
                                write = Write "2";
                                next =
                                  Normal
                                    ( Null,
                                      Routine
                                        ( Action "LEFT",
                                          Callee
                                            (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0
                                           ^ "_" ^ r ^ int_value ^ "_return") )
                                    );
                              },
                              -1 );
                            ( {
                                inputs = Inputs "4";
                                write = Write "3";
                                next =
                                  Normal
                                    ( Null,
                                      Routine
                                        ( Action "LEFT",
                                          Callee
                                            (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0
                                           ^ "_" ^ r ^ int_value ^ "_return") )
                                    );
                              },
                              -1 );
                            ( {
                                inputs = Inputs "5";
                                write = Write "4";
                                next =
                                  Normal
                                    ( Null,
                                      Routine
                                        ( Action "LEFT",
                                          Callee
                                            (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0
                                           ^ "_" ^ r ^ int_value ^ "_return") )
                                    );
                              },
                              -1 );
                            ( {
                                inputs = Inputs "6";
                                write = Write "5";
                                next =
                                  Normal
                                    ( Null,
                                      Routine
                                        ( Action "LEFT",
                                          Callee
                                            (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0
                                           ^ "_" ^ r ^ int_value ^ "_return") )
                                    );
                              },
                              -1 );
                            ( {
                                inputs = Inputs "7";
                                write = Write "6";
                                next =
                                  Normal
                                    ( Null,
                                      Routine
                                        ( Action "LEFT",
                                          Callee
                                            (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0
                                           ^ "_" ^ r ^ int_value ^ "_return") )
                                    );
                              },
                              -1 );
                            ( {
                                inputs = Inputs "8";
                                write = Write "7";
                                next =
                                  Normal
                                    ( Null,
                                      Routine
                                        ( Action "LEFT",
                                          Callee
                                            (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0
                                           ^ "_" ^ r ^ int_value ^ "_return") )
                                    );
                              },
                              -1 );
                            ( {
                                inputs = Inputs "9";
                                write = Write "8";
                                next =
                                  Normal
                                    ( Null,
                                      Routine
                                        ( Action "LEFT",
                                          Callee
                                            (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0
                                           ^ "_" ^ r ^ int_value ^ "_return") )
                                    );
                              },
                              -1 );
                            ( {
                                inputs = Inputs "a";
                                write = Write "9";
                                next =
                                  Normal
                                    ( Null,
                                      Routine
                                        ( Action "LEFT",
                                          Callee
                                            (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0
                                           ^ "_" ^ r ^ int_value ^ "_return") )
                                    );
                              },
                              -1 );
                            ( {
                                inputs = Inputs "b";
                                write = Write "a";
                                next =
                                  Normal
                                    ( Null,
                                      Routine
                                        ( Action "LEFT",
                                          Callee
                                            (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0
                                           ^ "_" ^ r ^ int_value ^ "_return") )
                                    );
                              },
                              -1 );
                            ( {
                                inputs = Inputs "c";
                                write = Write "b";
                                next =
                                  Normal
                                    ( Null,
                                      Routine
                                        ( Action "LEFT",
                                          Callee
                                            (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0
                                           ^ "_" ^ r ^ int_value ^ "_return") )
                                    );
                              },
                              -1 );
                            ( {
                                inputs = Inputs "d";
                                write = Write "c";
                                next =
                                  Normal
                                    ( Null,
                                      Routine
                                        ( Action "LEFT",
                                          Callee
                                            (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0
                                           ^ "_" ^ r ^ int_value ^ "_return") )
                                    );
                              },
                              -1 );
                            ( {
                                inputs = Inputs "e";
                                write = Write "d";
                                next =
                                  Normal
                                    ( Null,
                                      Routine
                                        ( Action "LEFT",
                                          Callee
                                            (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0
                                           ^ "_" ^ r ^ int_value ^ "_return") )
                                    );
                              },
                              -1 );
                            ( {
                                inputs = Inputs "f";
                                write = Write "e";
                                next =
                                  Normal
                                    ( Null,
                                      Routine
                                        ( Action "LEFT",
                                          Callee
                                            (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0
                                           ^ "_" ^ r ^ int_value ^ "_return") )
                                    );
                              },
                              -1 );
                          ];
                      };
                    ]
                | "mov" ->
                    [
                      {
                        name =
                          w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0 ^ "_" ^ r
                          ^ int_value ^ "_begin";
                        starts_at = -1;
                        definition =
                          [
                            ( {
                                inputs = Inputs "0123456789abcdef";
                                write = Write (String.make 1 int_value.[0]);
                                next =
                                  Normal
                                    ( Null,
                                      Routine
                                        ( Action "RIGHT",
                                          Callee
                                            (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0
                                           ^ "_" ^ r ^ int_value ^ "_1") ) );
                              },
                              -1 );
                          ];
                      };
                      {
                        name =
                          w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0 ^ "_" ^ r
                          ^ int_value ^ "_1";
                        starts_at = -1;
                        definition =
                          [
                            ( {
                                inputs = Inputs "0123456789abcdef";
                                write = Write (String.make 1 int_value.[1]);
                                next =
                                  Normal
                                    ( Null,
                                      Routine
                                        ( Action "RIGHT",
                                          Callee
                                            (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0
                                           ^ "_" ^ r ^ int_value ^ "_2") ) );
                              },
                              -1 );
                          ];
                      };
                      {
                        name =
                          w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0 ^ "_" ^ r
                          ^ int_value ^ "_2";
                        starts_at = -1;
                        definition =
                          [
                            ( {
                                inputs = Inputs "0123456789abcdef";
                                write = Write (String.make 1 int_value.[2]);
                                next =
                                  Normal
                                    ( Null,
                                      Routine
                                        ( Action "RIGHT",
                                          Callee
                                            (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0
                                           ^ "_" ^ r ^ int_value ^ "_3") ) );
                              },
                              -1 );
                          ];
                      };
                      {
                        name =
                          w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0 ^ "_" ^ r
                          ^ int_value ^ "_3";
                        starts_at = -1;
                        definition =
                          [
                            ( {
                                inputs = Inputs "0123456789abcdef";
                                write = Write (String.make 1 int_value.[3]);
                                next =
                                  Normal
                                    ( Null,
                                      Routine
                                        ( Action "LEFT",
                                          Callee
                                            (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0
                                           ^ "_" ^ r ^ int_value ^ "_return") )
                                    );
                              },
                              -1 );
                          ];
                      };
                    ]
                | _ ->
                    raise
                      (Invalid_argument
                         ("Error at line " ^ string_of_int i ^ ": '" ^ r
                        ^ "' is not a valid register."))
              in
              let func_sub_base =
                [
                  {
                    name =
                      w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0 ^ "_" ^ r ^ int_value;
                    starts_at = -1;
                    definition =
                      [
                        ( {
                            inputs = Inputs alphabet_without_register;
                            write = Write "self";
                            next =
                              Normal
                                ( Null,
                                  Routine
                                    ( Action "RIGHT",
                                      Callee
                                        (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0 ^ "_"
                                       ^ r ^ int_value) ) );
                          },
                          -1 );
                        ( {
                            inputs = Inputs (String.make 1 register);
                            write = Write "self";
                            next =
                              Normal
                                ( Null,
                                  Routine
                                    ( Action "RIGHT",
                                      Callee
                                        (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0 ^ "_"
                                       ^ r ^ int_value ^ "_begin") ) );
                          },
                          -1 );
                      ];
                  };
                  {
                    name =
                      w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0 ^ "_" ^ r ^ int_value
                      ^ "_return";
                    starts_at = -1;
                    definition =
                      [
                        ( {
                            inputs = Inputs "#";
                            write = Write "self";
                            next =
                              Normal
                                ( Null,
                                  Routine
                                    ( Action "RIGHT",
                                      Callee
                                        (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0 ^ "_"
                                       ^ r ^ int_value ^ "_return_fw") ) );
                          },
                          -1 );
                        ( {
                            inputs = Inputs alphabet_without_tag;
                            write = Write "self";
                            next =
                              Normal
                                ( Null,
                                  Routine
                                    ( Action "LEFT",
                                      Callee
                                        (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0 ^ "_"
                                       ^ r ^ int_value ^ "_return") ) );
                          },
                          -1 );
                      ];
                  };
                  {
                    name =
                      w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0 ^ "_" ^ r ^ int_value
                      ^ "_return_fw";
                    starts_at = -1;
                    definition =
                      [
                        ( {
                            inputs = Inputs alphabet_without_tag;
                            write = Write "self";
                            next =
                              Normal
                                ( Null,
                                  Routine
                                    ( Action "LEFT",
                                      Callee
                                        (w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0 ^ "_"
                                       ^ r ^ int_value ^ "_returned") ) );
                          },
                          -1 );
                      ];
                  };
                  {
                    name =
                      w ^ "_" ^ a ^ "_" ^ c1 ^ ":" ^ c0 ^ "_" ^ r ^ int_value
                      ^ "_returned";
                    starts_at = -1;
                    definition =
                      [
                        ( {
                            inputs = Inputs "#";
                            write = line.write;
                            next = Normal (Null, Routine (Action a, Callee c1));
                          },
                          i );
                      ];
                  };
                ]
              in
              List.append func_sub_base func_sub_heart
          | Eq
              ( Subroutine (Callee c0, Register r, Int nb),
                Eq_res bool,
                Routine (Action a1, Callee c1),
                Routine (Action a2, Callee c2) ) ->
              let int_value =
                if String.for_all (String.contains "0123456789abcdef") nb then
                  nb
                else
                  raise
                    (Invalid_argument
                       ("Error at line " ^ string_of_int i ^ ": '" ^ nb
                      ^ "' is not a valid hexa. Hexa needs to be of the form \
                         0000. Example: 04a6"))
              in
              let alphabet_without_0 =
                String.fold_left
                  (fun acc c ->
                    if c = int_value.[0] then acc else acc ^ String.make 1 c)
                  "" updated_alphabet
              in
              let alphabet_without_1 =
                String.fold_left
                  (fun acc c ->
                    if c = int_value.[1] then acc else acc ^ String.make 1 c)
                  "" updated_alphabet
              in
              let alphabet_without_2 =
                String.fold_left
                  (fun acc c ->
                    if c = int_value.[2] then acc else acc ^ String.make 1 c)
                  "" updated_alphabet
              in
              let alphabet_without_3 =
                String.fold_left
                  (fun acc c ->
                    if c = int_value.[3] then acc else acc ^ String.make 1 c)
                  "" updated_alphabet
              in
              let register =
                match r with
                | "eax" -> 'A'
                | "ebx" -> 'B'
                | "ecx" -> 'C'
                | "edx" -> 'D'
                | _ ->
                    raise
                      (Invalid_argument
                         ("Error at line " ^ string_of_int i ^ ": '" ^ r
                        ^ "' is not a valid register."))
              in
              let the_bool =
                if ":true" = bool then true
                else if ":false" = bool then false
                else
                  raise
                    (Invalid_argument
                       ("Error at line " ^ string_of_int i ^ ": '" ^ nb
                      ^ "' is not a valid bool. Bool needs to be :true or \
                         :false"))
              in
              let alphabet_without_register =
                String.fold_left
                  (fun acc c ->
                    if c = register then acc else acc ^ String.make 1 c)
                  "" updated_alphabet
              in
              let eq_func_base =
                [
                  {
                    name =
                      w ^ "_" ^ a1 ^ "_" ^ c1 ^ "_or_" ^ a2 ^ "_" ^ c2 ^ "_if_"
                      ^ bool ^ ":" ^ c0 ^ "_" ^ r ^ "_" ^ int_value;
                    starts_at = -1;
                    definition =
                      [
                        ( {
                            inputs = Inputs alphabet_without_register;
                            write = Write "self";
                            next =
                              Normal
                                ( Null,
                                  Routine
                                    ( Action "RIGHT",
                                      Callee
                                        (w ^ "_" ^ a1 ^ "_" ^ c1 ^ "_or_" ^ a2
                                       ^ "_" ^ c2 ^ "_if_" ^ bool ^ ":" ^ c0
                                       ^ "_" ^ r ^ "_" ^ nb) ) );
                          },
                          -1 );
                        ( {
                            inputs = Inputs (String.make 1 register);
                            write = Write "self";
                            next =
                              Normal
                                ( Null,
                                  Routine
                                    ( Action "RIGHT",
                                      Callee
                                        (w ^ "_" ^ a1 ^ "_" ^ c1 ^ "_or_" ^ a2
                                       ^ "_" ^ c2 ^ "_if_" ^ bool ^ ":" ^ c0
                                       ^ "_" ^ r ^ "_" ^ nb ^ "_begin") ) );
                          },
                          -1 );
                      ];
                  };
                  {
                    name =
                      w ^ "_" ^ a1 ^ "_" ^ c1 ^ "_or_" ^ a2 ^ "_" ^ c2 ^ "_if_"
                      ^ bool ^ ":" ^ c0 ^ "_" ^ r ^ "_" ^ nb ^ "_return_true";
                    starts_at = -1;
                    definition =
                      [
                        ( {
                            inputs = Inputs "#";
                            write = Write "self";
                            next =
                              Normal
                                ( Null,
                                  Routine
                                    ( Action "RIGHT",
                                      Callee
                                        (w ^ "_" ^ a1 ^ "_" ^ c1 ^ "_or_" ^ a2
                                       ^ "_" ^ c2 ^ "_if_" ^ bool ^ ":" ^ c0
                                       ^ "_" ^ r ^ "_" ^ nb ^ "_return_fw_true"
                                        ) ) );
                          },
                          -1 );
                        ( {
                            inputs = Inputs alphabet_without_tag;
                            write = Write "self";
                            next =
                              Normal
                                ( Null,
                                  Routine
                                    ( Action "LEFT",
                                      Callee
                                        (w ^ "_" ^ a1 ^ "_" ^ c1 ^ "_or_" ^ a2
                                       ^ "_" ^ c2 ^ "_if_" ^ bool ^ ":" ^ c0
                                       ^ "_" ^ r ^ "_" ^ nb ^ "_return_true") )
                                );
                          },
                          -1 );
                      ];
                  };
                  {
                    name =
                      w ^ "_" ^ a1 ^ "_" ^ c1 ^ "_or_" ^ a2 ^ "_" ^ c2 ^ "_if_"
                      ^ bool ^ ":" ^ c0 ^ "_" ^ r ^ "_" ^ nb ^ "_return_fw_true";
                    starts_at = -1;
                    definition =
                      [
                        ( {
                            inputs = Inputs alphabet_without_tag;
                            write = Write "self";
                            next =
                              Normal
                                ( Null,
                                  Routine
                                    ( Action "LEFT",
                                      Callee
                                        (w ^ "_" ^ a1 ^ "_" ^ c1 ^ "_or_" ^ a2
                                       ^ "_" ^ c2 ^ "_if_" ^ bool ^ ":" ^ c0
                                       ^ "_" ^ r ^ "_" ^ nb ^ "_returned_true")
                                    ) );
                          },
                          -1 );
                      ];
                  };
                  {
                    name =
                      w ^ "_" ^ a1 ^ "_" ^ c1 ^ "_or_" ^ a2 ^ "_" ^ c2 ^ "_if_"
                      ^ bool ^ ":" ^ c0 ^ "_" ^ r ^ "_" ^ nb ^ "_return_false";
                    starts_at = -1;
                    definition =
                      [
                        ( {
                            inputs = Inputs "#";
                            write = Write "self";
                            next =
                              Normal
                                ( Null,
                                  Routine
                                    ( Action "RIGHT",
                                      Callee
                                        (w ^ "_" ^ a1 ^ "_" ^ c1 ^ "_or_" ^ a2
                                       ^ "_" ^ c2 ^ "_if_" ^ bool ^ ":" ^ c0
                                       ^ "_" ^ r ^ "_" ^ nb ^ "_return_fw_false"
                                        ) ) );
                          },
                          -1 );
                        ( {
                            inputs = Inputs alphabet_without_tag;
                            write = Write "self";
                            next =
                              Normal
                                ( Null,
                                  Routine
                                    ( Action "LEFT",
                                      Callee
                                        (w ^ "_" ^ a1 ^ "_" ^ c1 ^ "_or_" ^ a2
                                       ^ "_" ^ c2 ^ "_if_" ^ bool ^ ":" ^ c0
                                       ^ "_" ^ r ^ "_" ^ nb ^ "_return_false")
                                    ) );
                          },
                          -1 );
                      ];
                  };
                  {
                    name =
                      w ^ "_" ^ a1 ^ "_" ^ c1 ^ "_or_" ^ a2 ^ "_" ^ c2 ^ "_if_"
                      ^ bool ^ ":" ^ c0 ^ "_" ^ r ^ "_" ^ nb
                      ^ "_return_fw_false";
                    starts_at = -1;
                    definition =
                      [
                        ( {
                            inputs = Inputs alphabet_without_tag;
                            write = Write "self";
                            next =
                              Normal
                                ( Null,
                                  Routine
                                    ( Action "LEFT",
                                      Callee
                                        (w ^ "_" ^ a1 ^ "_" ^ c1 ^ "_or_" ^ a2
                                       ^ "_" ^ c2 ^ "_if_" ^ bool ^ ":" ^ c0
                                       ^ "_" ^ r ^ "_" ^ nb ^ "_returned_false"
                                        ) ) );
                          },
                          -1 );
                      ];
                  };
                  {
                    name =
                      w ^ "_" ^ a1 ^ "_" ^ c1 ^ "_or_" ^ a2 ^ "_" ^ c2 ^ "_if_"
                      ^ bool ^ ":" ^ c0 ^ "_" ^ r ^ "_" ^ nb ^ "_returned_true";
                    starts_at = -1;
                    definition =
                      [
                        ( {
                            inputs = Inputs "#";
                            write = line.write;
                            next = Normal (Null, Routine (Action a1, Callee c1));
                          },
                          i );
                      ];
                  };
                  {
                    name =
                      w ^ "_" ^ a1 ^ "_" ^ c1 ^ "_or_" ^ a2 ^ "_" ^ c2 ^ "_if_"
                      ^ bool ^ ":" ^ c0 ^ "_" ^ r ^ "_" ^ nb ^ "_returned_false";
                    starts_at = -1;
                    definition =
                      [
                        ( {
                            inputs = Inputs "#";
                            write = line.write;
                            next = Normal (Null, Routine (Action a2, Callee c2));
                          },
                          i );
                      ];
                  };
                ]
              in
              let eq_func_bool =
                if the_bool then
                  [
                    {
                      name =
                        w ^ "_" ^ a1 ^ "_" ^ c1 ^ "_or_" ^ a2 ^ "_" ^ c2
                        ^ "_if_" ^ bool ^ ":" ^ c0 ^ "_" ^ r ^ "_" ^ int_value
                        ^ "_begin";
                      starts_at = -1;
                      definition =
                        [
                          ( {
                              inputs = Inputs (String.make 1 int_value.[0]);
                              write = Write "self";
                              next =
                                Normal
                                  ( Null,
                                    Routine
                                      ( Action "RIGHT",
                                        Callee
                                          (w ^ "_" ^ a1 ^ "_" ^ c1 ^ "_or_" ^ a2
                                         ^ "_" ^ c2 ^ "_if_" ^ bool ^ ":" ^ c0
                                         ^ "_" ^ r ^ "_" ^ int_value ^ "_1") )
                                  );
                            },
                            -1 );
                          ( {
                              inputs = Inputs alphabet_without_0;
                              write = Write "self";
                              next =
                                Normal
                                  ( Null,
                                    Routine
                                      ( Action "LEFT",
                                        Callee
                                          (w ^ "_" ^ a1 ^ "_" ^ c1 ^ "_or_" ^ a2
                                         ^ "_" ^ c2 ^ "_if_" ^ bool ^ ":" ^ c0
                                         ^ "_" ^ r ^ "_" ^ int_value
                                         ^ "_return_false") ) );
                            },
                            -1 );
                        ];
                    };
                    {
                      name =
                        w ^ "_" ^ a1 ^ "_" ^ c1 ^ "_or_" ^ a2 ^ "_" ^ c2
                        ^ "_if_" ^ bool ^ ":" ^ c0 ^ "_" ^ r ^ "_" ^ int_value
                        ^ "_1";
                      starts_at = -1;
                      definition =
                        [
                          ( {
                              inputs = Inputs (String.make 1 int_value.[1]);
                              write = Write "self";
                              next =
                                Normal
                                  ( Null,
                                    Routine
                                      ( Action "RIGHT",
                                        Callee
                                          (w ^ "_" ^ a1 ^ "_" ^ c1 ^ "_or_" ^ a2
                                         ^ "_" ^ c2 ^ "_if_" ^ bool ^ ":" ^ c0
                                         ^ "_" ^ r ^ "_" ^ int_value ^ "_2") )
                                  );
                            },
                            -1 );
                          ( {
                              inputs = Inputs alphabet_without_1;
                              write = Write "self";
                              next =
                                Normal
                                  ( Null,
                                    Routine
                                      ( Action "LEFT",
                                        Callee
                                          (w ^ "_" ^ a1 ^ "_" ^ c1 ^ "_or_" ^ a2
                                         ^ "_" ^ c2 ^ "_if_" ^ bool ^ ":" ^ c0
                                         ^ "_" ^ r ^ "_" ^ int_value
                                         ^ "_return_false") ) );
                            },
                            -1 );
                        ];
                    };
                    {
                      name =
                        w ^ "_" ^ a1 ^ "_" ^ c1 ^ "_or_" ^ a2 ^ "_" ^ c2
                        ^ "_if_" ^ bool ^ ":" ^ c0 ^ "_" ^ r ^ "_" ^ int_value
                        ^ "_2";
                      starts_at = -1;
                      definition =
                        [
                          ( {
                              inputs = Inputs (String.make 1 int_value.[2]);
                              write = Write "self";
                              next =
                                Normal
                                  ( Null,
                                    Routine
                                      ( Action "RIGHT",
                                        Callee
                                          (w ^ "_" ^ a1 ^ "_" ^ c1 ^ "_or_" ^ a2
                                         ^ "_" ^ c2 ^ "_if_" ^ bool ^ ":" ^ c0
                                         ^ "_" ^ r ^ "_" ^ int_value ^ "_3") )
                                  );
                            },
                            -1 );
                          ( {
                              inputs = Inputs alphabet_without_2;
                              write = Write "self";
                              next =
                                Normal
                                  ( Null,
                                    Routine
                                      ( Action "LEFT",
                                        Callee
                                          (w ^ "_" ^ a1 ^ "_" ^ c1 ^ "_or_" ^ a2
                                         ^ "_" ^ c2 ^ "_if_" ^ bool ^ ":" ^ c0
                                         ^ "_" ^ r ^ "_" ^ int_value
                                         ^ "_return_false") ) );
                            },
                            -1 );
                        ];
                    };
                    {
                      name =
                        w ^ "_" ^ a1 ^ "_" ^ c1 ^ "_or_" ^ a2 ^ "_" ^ c2
                        ^ "_if_" ^ bool ^ ":" ^ c0 ^ "_" ^ r ^ "_" ^ int_value
                        ^ "_3";
                      starts_at = -1;
                      definition =
                        [
                          ( {
                              inputs = Inputs (String.make 1 int_value.[3]);
                              write = Write "self";
                              next =
                                Normal
                                  ( Null,
                                    Routine
                                      ( Action "LEFT",
                                        Callee
                                          (w ^ "_" ^ a1 ^ "_" ^ c1 ^ "_or_" ^ a2
                                         ^ "_" ^ c2 ^ "_if_" ^ bool ^ ":" ^ c0
                                         ^ "_" ^ r ^ "_" ^ int_value
                                         ^ "_return_true") ) );
                            },
                            -1 );
                          ( {
                              inputs = Inputs alphabet_without_3;
                              write = Write "self";
                              next =
                                Normal
                                  ( Null,
                                    Routine
                                      ( Action "LEFT",
                                        Callee
                                          (w ^ "_" ^ a1 ^ "_" ^ c1 ^ "_or_" ^ a2
                                         ^ "_" ^ c2 ^ "_if_" ^ bool ^ ":" ^ c0
                                         ^ "_" ^ r ^ "_" ^ int_value
                                         ^ "_return_false") ) );
                            },
                            -1 );
                        ];
                    };
                  ]
                else
                  [
                    {
                      name =
                        w ^ "_" ^ a1 ^ "_" ^ c1 ^ "_or_" ^ a2 ^ "_" ^ c2
                        ^ "_if_" ^ bool ^ ":" ^ c0 ^ "_" ^ r ^ "_" ^ int_value
                        ^ "_begin";
                      starts_at = -1;
                      definition =
                        [
                          ( {
                              inputs = Inputs alphabet_without_0;
                              write = Write "self";
                              next =
                                Normal
                                  ( Null,
                                    Routine
                                      ( Action "RIGHT",
                                        Callee
                                          (w ^ "_" ^ a1 ^ "_" ^ c1 ^ "_or_" ^ a2
                                         ^ "_" ^ c2 ^ "_if_" ^ bool ^ ":" ^ c0
                                         ^ "_" ^ r ^ "_" ^ int_value ^ "_1") )
                                  );
                            },
                            -1 );
                          ( {
                              inputs = Inputs (String.make 1 int_value.[0]);
                              write = Write "self";
                              next =
                                Normal
                                  ( Null,
                                    Routine
                                      ( Action "LEFT",
                                        Callee
                                          (w ^ "_" ^ a1 ^ "_" ^ c1 ^ "_or_" ^ a2
                                         ^ "_" ^ c2 ^ "_if_" ^ bool ^ ":" ^ c0
                                         ^ "_" ^ r ^ "_" ^ int_value
                                         ^ "_return_false") ) );
                            },
                            -1 );
                        ];
                    };
                    {
                      name =
                        w ^ "_" ^ a1 ^ "_" ^ c1 ^ "_or_" ^ a2 ^ "_" ^ c2
                        ^ "_if_" ^ bool ^ ":" ^ c0 ^ "_" ^ r ^ "_" ^ int_value
                        ^ "_1";
                      starts_at = -1;
                      definition =
                        [
                          ( {
                              inputs = Inputs alphabet_without_1;
                              write = Write "self";
                              next =
                                Normal
                                  ( Null,
                                    Routine
                                      ( Action "RIGHT",
                                        Callee
                                          (w ^ "_" ^ a1 ^ "_" ^ c1 ^ "_or_" ^ a2
                                         ^ "_" ^ c2 ^ "_if_" ^ bool ^ ":" ^ c0
                                         ^ "_" ^ r ^ "_" ^ int_value ^ "_2") )
                                  );
                            },
                            -1 );
                          ( {
                              inputs = Inputs (String.make 1 int_value.[1]);
                              write = Write "self";
                              next =
                                Normal
                                  ( Null,
                                    Routine
                                      ( Action "LEFT",
                                        Callee
                                          (w ^ "_" ^ a1 ^ "_" ^ c1 ^ "_or_" ^ a2
                                         ^ "_" ^ c2 ^ "_if_" ^ bool ^ ":" ^ c0
                                         ^ "_" ^ r ^ "_" ^ int_value
                                         ^ "_return_false") ) );
                            },
                            -1 );
                        ];
                    };
                    {
                      name =
                        w ^ "_" ^ a1 ^ "_" ^ c1 ^ "_or_" ^ a2 ^ "_" ^ c2
                        ^ "_if_" ^ bool ^ ":" ^ c0 ^ "_" ^ r ^ "_" ^ int_value
                        ^ "_2";
                      starts_at = -1;
                      definition =
                        [
                          ( {
                              inputs = Inputs alphabet_without_2;
                              write = Write "self";
                              next =
                                Normal
                                  ( Null,
                                    Routine
                                      ( Action "RIGHT",
                                        Callee
                                          (w ^ "_" ^ a1 ^ "_" ^ c1 ^ "_or_" ^ a2
                                         ^ "_" ^ c2 ^ "_if_" ^ bool ^ ":" ^ c0
                                         ^ "_" ^ r ^ "_" ^ int_value ^ "_3") )
                                  );
                            },
                            -1 );
                          ( {
                              inputs = Inputs (String.make 1 int_value.[2]);
                              write = Write "self";
                              next =
                                Normal
                                  ( Null,
                                    Routine
                                      ( Action "LEFT",
                                        Callee
                                          (w ^ "_" ^ a1 ^ "_" ^ c1 ^ "_or_" ^ a2
                                         ^ "_" ^ c2 ^ "_if_" ^ bool ^ ":" ^ c0
                                         ^ "_" ^ r ^ "_" ^ int_value
                                         ^ "_return_false") ) );
                            },
                            -1 );
                        ];
                    };
                    {
                      name =
                        w ^ "_" ^ a1 ^ "_" ^ c1 ^ "_or_" ^ a2 ^ "_" ^ c2
                        ^ "_if_" ^ bool ^ ":" ^ c0 ^ "_" ^ r ^ "_" ^ int_value
                        ^ "_3";
                      starts_at = -1;
                      definition =
                        [
                          ( {
                              inputs = Inputs alphabet_without_3;
                              write = Write "self";
                              next =
                                Normal
                                  ( Null,
                                    Routine
                                      ( Action "LEFT",
                                        Callee
                                          (w ^ "_" ^ a1 ^ "_" ^ c1 ^ "_or_" ^ a2
                                         ^ "_" ^ c2 ^ "_if_" ^ bool ^ ":" ^ c0
                                         ^ "_" ^ r ^ "_" ^ int_value
                                         ^ "_return_true") ) );
                            },
                            -1 );
                          ( {
                              inputs = Inputs (String.make 1 int_value.[3]);
                              write = Write "self";
                              next =
                                Normal
                                  ( Null,
                                    Routine
                                      ( Action "LEFT",
                                        Callee
                                          (w ^ "_" ^ a1 ^ "_" ^ c1 ^ "_or_" ^ a2
                                         ^ "_" ^ c2 ^ "_if_" ^ bool ^ ":" ^ c0
                                         ^ "_" ^ r ^ "_" ^ int_value
                                         ^ "_return_false") ) );
                            },
                            -1 );
                        ];
                    };
                  ]
              in
              List.append eq_func_base eq_func_bool
          | _ -> [])
        func.definition
      |> List.concat
    in
    func_begin :: funcs_subroutines
  in

  let generate_memmove funcs =
    List.map
      (fun func ->
        if
          List.exists
            (fun (line, _) ->
              match line.next with
              | Eq _ -> true
              | Normal (Subroutine _, _) -> true
              | _ -> false)
            func.definition
        then build_funcs func
        else [ func ])
      funcs
    |> List.concat
  in
  let updated_parsed_operator =
    if mem_opt.(0) then generate_memmove parsed_operator |> init_mem
    else parsed_operator
  in

  (file_name, updated_alphabet, updated_parsed_operator)

let to_transitions (line, i) alphabet =
  let inputs =
    match line.inputs with
    | Inputs str ->
        if str = "any" then
          List.fold_left (fun acc c -> String.make 1 c :: acc) [] alphabet
        else
          String.fold_left (fun acc c -> String.make 1 c :: acc) [] str
          |> List.rev
    | _ ->
        raise
          (Invalid_argument
             ("Error at line " ^ string_of_int i ^ ": Incorrect input"))
  in
  let state =
    match line.next with
    | Normal (Null, Routine (_, Callee state)) -> state
    | _ ->
        raise
          (Invalid_argument
             ("Error at line " ^ string_of_int i ^ ": Incorrect state"))
  in
  let write =
    match line.write with
    | Write w -> w
    | _ ->
        raise
          (Invalid_argument
             ("Error at line " ^ string_of_int i ^ ": Incorrect write"))
  in
  let action =
    match line.next with
    | Normal (Null, Routine (Action action, _)) -> action
    | _ ->
        raise
          (Invalid_argument
             ("Error at line " ^ string_of_int i ^ ": Incorrect action"))
  in
  inputs
  |> List.map (fun input ->
         [|
           ("read", input);
           ("to_state", state);
           ("write", if "self" = write then input else write);
           ("action", action);
         |])

let parse_func as_funcs alphabet =
  let folded_alphabet = String.fold_left (fun acc c -> c :: acc) [] alphabet in
  List.map
    (fun func ->
      ( func.name,
        List.concat
          (let initial_transistions =
             List.map
               (fun def -> to_transitions def folded_alphabet)
               func.definition
           in
           let used_inputs =
             List.map
               (fun (line, i) ->
                 match line.inputs with
                 | Inputs str ->
                  if str = "any" then folded_alphabet
                  else
                     let inputs =
                       String.fold_left (fun acc c -> c :: acc) [] str
                       |> List.rev
                     in
                     if
                       List.length
                         (List.filter
                            (fun c -> not (String.contains alphabet c))
                            inputs)
                       = 0
                       || str = "any"
                     then inputs
                     else
                       raise
                         (Invalid_argument
                            ("Error at line " ^ string_of_int i ^ ": one of '"
                           ^ str ^ "' is not in alphabet"))
                 | _ ->
                     raise
                       (Invalid_argument
                          ("Error at line " ^ string_of_int i
                         ^ ": Incorrect input")))
               func.definition
             |> List.concat
             |> List.fold_left (fun acc c -> acc ^ String.make 1 c) ""
           in
           let filtered_alphabet =
             List.filter
               (fun c -> not (String.contains used_inputs c))
               folded_alphabet
           in
           let error_transitions =
             List.fold_left
               (fun acc c ->
                 [|
                   ("read", String.make 1 c);
                   ("to_state", "ERROR");
                   ("write", String.make 1 c);
                   ("action", "LEFT");
                 |]
                 :: acc)
               [] filtered_alphabet
           in
           error_transitions :: initial_transistions |> List.rev) ))
    as_funcs

let to_tm_json (file_name, alphabet, as_tree) =
  let parsed_state_names =
    let state_names =
      "HALT" :: "ERROR" :: List.map (fun func -> func.name) as_tree
    in
    let module StringSet = Set.Make (String) in
    let has_duplicates lst =
      let rec aux seen = function
        | [] -> false
        | x :: xs ->
            if StringSet.mem x seen then (
              print_endline x;
              true)
            else aux (StringSet.add x seen) xs
      in
      aux StringSet.empty lst
    in
    if has_duplicates state_names then
      raise (Invalid_argument "Error: duplicated functions are present in file")
    else state_names
  in
  let parsed_transitions = parse_func as_tree alphabet in
  let parsed_alphabet =
    String.fold_left (fun acc c -> String.make 1 c :: acc) [] alphabet
    |> List.rev
  in
  {
    file_name;
    alphabet = Array.of_list parsed_alphabet;
    blank = List.hd parsed_alphabet;
    states = Array.of_list parsed_state_names;
    transitions = parsed_transitions;
  }

let to_file tm_json =
  let string_of_array (arr : string array) =
    let new_string =
      Array.fold_left (fun acc str -> acc ^ "\"" ^ str ^ "\", ") "" arr
    in
    String.sub new_string 0 (String.length new_string - 2)
  in
  let string_of_transition (name, definition) =
    "\"" ^ name ^ "\" : [\n                "
    ^ (let new_list =
         List.map
           (fun arr ->
             "{ \"read\" : \""
             ^ snd arr.(0)
             ^ "\", \"to_state\" : \""
             ^ snd arr.(1)
             ^ "\", \"write\" : \""
             ^ snd arr.(2)
             ^ "\", \"action\" : \""
             ^ snd arr.(3)
             ^ "\" }, ")
           definition
       in
       let new_string = List.fold_left (fun acc str -> acc ^ str) "" new_list in
       String.sub new_string 0 (String.length new_string - 2))
    ^ "\n        ], "
  in

  "{\n        \"name\"          : \"" ^ tm_json.file_name
  ^ "\",\n        \"alphabet\"      : ["
  ^ string_of_array tm_json.alphabet
  ^ "],\n        \"blank\"         : \"" ^ tm_json.blank
  ^ "\",\n        \"states\"        : ["
  ^ string_of_array tm_json.states
  ^ "],\n\
    \        \"initial\"       : \"_start\",\n\
    \        \"finals\"        : [\"HALT\", \"ERROR\"],\n\
    \        \"transitions\"   : {\n\
    \                "
  ^ (let new_string =
       List.map string_of_transition tm_json.transitions
       |> List.fold_left (fun acc str -> acc ^ str) ""
     in
     String.sub new_string 0 (String.length new_string - 2))
  ^ "\n        }\n}"

let compile file_name =
  let output = file_name |> to_list |> to_funcs |> to_tm_json |> to_file in
  let file_name =
    file_name |> String.split_on_char '/' |> List.rev |> List.hd
    |> String.split_on_char '.' |> List.hd
  in
  let json_file = open_out (file_name ^ ".json") in
  Out_channel.output_string json_file output;
  Out_channel.close json_file
