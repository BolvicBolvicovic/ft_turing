open Ft_turing

let () =
        let input_module = Turing.from_input Sys.argv.(1) in
        let module Input = (val input_module : Turing.INPUT) in
        let module Machine = Turing.Make(Input) in
        Machine.print_machine ();
        Machine.execute (Sys.argv.(2) ^ Machine.blank);
