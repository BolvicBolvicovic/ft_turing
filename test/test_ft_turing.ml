open Ft_turing

module Sub : Turing.INPUT = struct
        type transition = string * string * string * string
        let name = "unary_sub"
        let alphabet = ["1"; "."; "-"; "="]
        let blank = "."
        let states = [ "scanright"; "eraseone"; "subone"; "skip"; "HALT" ]
        let initial = "scanright"
        let finals = ["HALT"]
        let transitions =
                let ht = Turing.StateHashtbl.create 4 in
                Turing.StateHashtbl.add ht "scanright" [|(".", "scanright", ".", "RIGHT"); ("1", "scanright", "1", "RIGHT"); ("-", "scanright", "-", "RIGHT"); ("=", "eraseone", ".", "LEFT")|];
                Turing.StateHashtbl.add ht "eraseone" [|("1", "subone", "=", "LEFT"); ("-", "HALT", ".", "LEFT")|];
                Turing.StateHashtbl.add ht "subone"[|("1", "subone", "1", "LEFT"); ("-", "skip", "-", "LEFT")|]; 
                Turing.StateHashtbl.add ht "skip"[|(".", "skip", ".", "LEFT"); ("1", "scanright", ".", "RIGHT")|]; 
                ht
                
end

module Add : Turing.INPUT = struct
        type transition = string * string * string * string
        let name = "unary_add"
        let alphabet = ["1"; "."; "+"; "="]
        let blank = "."
        let states = [ "scanright"; "delone"; "HALT" ]
        let initial = "scanright"
        let finals = ["HALT"]
        let transitions =
                let ht = Turing.StateHashtbl.create 4 in
                Turing.StateHashtbl.add ht "scanright" [|(".", "scanright", ".", "RIGHT"); ("1", "scanright", "1", "RIGHT"); ("+", "scanright", "1", "RIGHT"); ("=", "delone", ".", "LEFT")|];
                Turing.StateHashtbl.add ht "delone" [|("1", "HALT", ".", "LEFT");|];
                ht
                
end

module UnarySub = Turing.Make(Sub)
module UnaryAdd = Turing.Make(Add)

let () =
        let str_input = "111-11=............." in
        UnarySub.print_machine ();
        UnarySub.execute str_input

let () =
        let str_input = "111+11=............." in
        UnaryAdd.print_machine ();
        UnaryAdd.execute str_input
