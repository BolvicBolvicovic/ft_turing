open Ft_turing

let print_help () =
  print_endline "USAGE: ft_turing [OPTION] machine_name.json 'input_to_machine'";
  print_endline "OPTIONS:";
  print_endline
    "    machine_name.json input_to_machine   -> runs the Turing machine with \
     its input";
  print_endline
    "    -c machine_name.s                    -> compile an .s file to a .json \
     file. The .s file specifications are written at lib/compiler.md";
  print_endline
    "    -UTMc machine_file input_to_machine  -> compile a .s or a .json file \
     to a string that can be passed as an argument for the UTM";
  print_endline
    "    --help -h                            -> prints this message"

let () =
  try
    match Array.length Sys.argv with
    | 2 ->
        if Sys.argv.(1) = "--help" || Sys.argv.(1) = "-h" then print_help ()
        else
          invalid_arg
            "USAGE: ft_turing [OPTION] machine_name.json 'input_to_machine'"
    | 3 ->
        if Sys.argv.(1) = "-c" && String.ends_with ~suffix:".s" Sys.argv.(2)
        then Compiler.compile Sys.argv.(2)
        else if String.ends_with ~suffix:".json" Sys.argv.(1) = true then (
          let input_module = Turing.from_input Sys.argv.(1) in
          let module Input = (val input_module : Turing.INPUT) in
          let module Machine = Turing.Make (Input) in
          Machine.print_machine ();
          Machine.execute Sys.argv.(2))
        else
          invalid_arg
            "USAGE: ft_turing [OPTION] machine_name.json 'input_to_machine' "
    | 4 ->
        if
          Sys.argv.(1) = "-UTMc"
          && String.ends_with ~suffix:".json" Sys.argv.(2) = true
        then
          let input_module = Turing.from_input Sys.argv.(2) in
          let module Input = (val input_module : Turing.INPUT) in
          let module Machine = Turing.Make (Input) in
          Machine.compile Sys.argv.(3)
        else
          invalid_arg
            "USAGE: ft_turing [OPTION] machine_name.json 'input_to_machine'"
    | _ ->
        invalid_arg
          "USAGE: ft_turing [OPTION] machine_name.json 'input_to_machine'"
  with
  | Invalid_argument e -> print_endline e
  | e -> print_endline (Printexc.to_string e)
