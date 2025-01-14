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
                Turing.StateHashtbl.add ht "scanright" [|
                        (".", "scanright", ".", "RIGHT");
                        ("1", "scanright", "1", "RIGHT");
                        ("-", "scanright", "-", "RIGHT");
                        ("=", "eraseone", ".", "LEFT");
                |];
                Turing.StateHashtbl.add ht "eraseone" [|
                        ("1", "subone", "=", "LEFT");
                        ("-", "HALT", ".", "LEFT");
                |];
                Turing.StateHashtbl.add ht "subone"[|
                        ("1", "subone", "1", "LEFT");
                        ("-", "skip", "-", "LEFT");
                |]; 
                Turing.StateHashtbl.add ht "skip"[|
                        (".", "skip", ".", "LEFT");
                        ("1", "scanright", ".", "RIGHT");
                |]; 
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
                Turing.StateHashtbl.add ht "scanright" [|
                        (".", "HALT", ".", "LEFT");
                        ("1", "scanright", "1", "RIGHT");
                        ("+", "scanright", "1", "RIGHT");
                        ("=", "delone", ".", "LEFT");
                |];
                Turing.StateHashtbl.add ht "delone" [|("1", "HALT", ".", "LEFT");|];
                ht
                
end

module Pal : Turing.INPUT = struct
        type transition = string * string * string * string
        let name = "palindrom"
        let alphabet = ["a"; "b"; "y"; "n"; "."]
        let blank = "."
        let states = [ "reach_end_a"; "reach_end_b"; "reach_start"; "check_left_a"; "check_left_b"; "check_right"; "HALT"; "NO" ]
        let initial = "check_right"
        let finals = ["HALT"]
        let transitions =
                let ht = Turing.StateHashtbl.create 7 in
                Turing.StateHashtbl.add ht "reach_end_a" [|
                        (".", "check_left_a", ".", "LEFT");
                        ("a", "reach_end_a", "a", "RIGHT");
                        ("b", "reach_end_a", "b", "RIGHT");
                |];
                Turing.StateHashtbl.add ht "reach_end_b" [|
                        (".", "check_left_b", ".", "LEFT");
                        ("a", "reach_end_b", "a", "RIGHT");
                        ("b", "reach_end_b", "b", "RIGHT");
                |];
                Turing.StateHashtbl.add ht "reach_start" [|
                        (".", "check_right", ".", "RIGHT");
                        ("a", "reach_start", "a", "LEFT");
                        ("b", "reach_start", "b", "LEFT");
                |];
                Turing.StateHashtbl.add ht "check_right" [|
                        (".", "HALT", "y", "LEFT");
                        ("a", "reach_end_a", ".", "RIGHT");
                        ("b", "reach_end_b", ".", "RIGHT");
                |];
                Turing.StateHashtbl.add ht "check_left_a" [|
                        ("b", "NO", "b", "RIGHT");
                        ("a", "reach_start", ".", "LEFT");
                        (".", "HALT", "y", "RIGHT");
                |];
                Turing.StateHashtbl.add ht "check_left_b" [|
                        ("a", "NO", "a", "RIGHT");
                        ("b", "reach_start", ".", "LEFT");
                        (".", "HALT", "y", "RIGHT");
                |];
                Turing.StateHashtbl.add ht "NO" [|(".", "HALT", "n", "LEFT");|];

                ht
                
end

module LanDet : Turing.INPUT = struct
        type transition = string * string * string * string
        let name = "language_detector 0^n and 1^n"
        let alphabet = ["0"; "1"; "y"; "n"; "."]
        let blank = "."
        let states = [ "reach_end_0"; "reach_end_1"; "reach_start"; "check_left_0"; "check_left_1"; "check_right"; "HALT"; "NO"; "check_alone_0"; "check_alone_1"; ]
        let initial = "check_right"
        let finals = ["HALT"]
        let transitions =
                let ht = Turing.StateHashtbl.create 10 in
                Turing.StateHashtbl.add ht "check_alone_0" [|
                        (".", "HALT", "n", "LEFT");
                        ("0", "reach_end_0", "0", "RIGHT");
                        ("1", "reach_end_0", "1", "RIGHT");
                |];
                Turing.StateHashtbl.add ht "check_alone_1" [|
                        (".", "HALT", "n", "LEFT");
                        ("0", "reach_end_1", "0", "RIGHT");
                        ("1", "reach_end_1", "1", "RIGHT");
                |];
                Turing.StateHashtbl.add ht "reach_end_0" [|
                        (".", "check_left_0", ".", "LEFT");
                        ("0", "reach_end_0", "0", "RIGHT");
                        ("1", "reach_end_0", "1", "RIGHT");
                |];
                Turing.StateHashtbl.add ht "reach_end_1" [|
                        (".", "check_left_1", ".", "LEFT");
                        ("0", "reach_end_1", "0", "RIGHT");
                        ("1", "reach_end_1", "1", "RIGHT");
                |];
                Turing.StateHashtbl.add ht "reach_start" [|
                        (".", "check_right", ".", "RIGHT");
                        ("0", "reach_start", "0", "LEFT");
                        ("1", "reach_start", "1", "LEFT");
                |];
                Turing.StateHashtbl.add ht "check_right" [|
                        (".", "HALT", "y", "LEFT");
                        ("0", "check_alone_0", ".", "RIGHT");
                        ("1", "check_alone_1", ".", "RIGHT");
                |];
                Turing.StateHashtbl.add ht "check_left_0" [|
                        ("0", "NO", "0", "RIGHT");
                        ("1", "reach_start", ".", "LEFT");
                        (".", "HALT", "y", "RIGHT");
                |];
                Turing.StateHashtbl.add ht "check_left_1" [|
                        ("1", "NO", "1", "RIGHT");
                        ("0", "reach_start", ".", "LEFT");
                        (".", "HALT", "y", "RIGHT");
                |];
                Turing.StateHashtbl.add ht "NO" [|(".", "HALT", "n", "LEFT");|];

                ht
end

module NewLanDet : Turing.INPUT = struct
        type transition = string * string * string * string
        let name = "language_detector 0^2n"
        let alphabet = ["0"; "2"; "1"; ".";]
        let blank = "."
        let states = [ "START"; "is_first" ; "scanright_0"; "scanright_2"; "skip_left"; "HALT"; ]
        let initial = "START"
        let finals = ["HALT"]
        let transitions =
                let ht = Turing.StateHashtbl.create 5 in
                Turing.StateHashtbl.add ht "START" [|
                        ("0", "is_first", ".", "RIGHT");
                |];
                Turing.StateHashtbl.add ht "is_first" [|
                        (".", "HALT", "y", "LEFT");
                        ("0", "scanright_0", "2", "RIGHT");
                        ("2", "is_first", "2", "RIGHT");
                |];
                Turing.StateHashtbl.add ht "scanright_0" [|
                        (".", "skip_left", ".", "LEFT");
                        ("0", "scanright_2", "0", "RIGHT");
                        ("2", "scanright_0", "2", "RIGHT");
                |];
                Turing.StateHashtbl.add ht "scanright_2" [|
                        (".", "HALT", "n", "LEFT");
                        ("0", "scanright_0", "2", "RIGHT");
                        ("2", "scanright_2", "2", "RIGHT");
                |];
                Turing.StateHashtbl.add ht "skip_left" [|
                        ("0", "skip_left", "0", "LEFT");
                        ("2", "skip_left", "2", "LEFT");
                        (".", "is_first", ".", "RIGHT");
                |];
                
                ht
                
end

module UnarySub = Turing.Make(Sub)
module UnaryAdd = Turing.Make(Add)
module Palindrom = Turing.Make(Pal)
module LanguageDetector = Turing.Make(LanDet)
module NewLanguageDetector = Turing.Make(NewLanDet)

(*
let () =
        let str_input = "111-11=............." in
        UnarySub.print_machine ();
        UnarySub.execute str_input;

let () =
        let str_input = "111+11=............." in
        UnaryAdd.print_machine ();
        UnaryAdd.execute str_input;

let () =
        let str_input = "abba................" in
        Palindrom.print_machine ();
        Palindrom.execute str_input;

let () =
        let str_input = "0001111............." in
        LanguageDetector.print_machine ();
        LanguageDetector.execute str_input;

let () =
        let str_input = "00000000............" in
        NewLanguageDetector.print_machine ();
        NewLanguageDetector.execute str_input;
*)
