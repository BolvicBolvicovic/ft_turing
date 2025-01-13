(* THIS IS THE TEST FILE FOR THE TURING MACHINE *)

module Input : Turing.INPUT = struct
        type transition = string * string * string * string
        let alphabet = ["1", ".", "-", "="]
        let blank = "."
        let states = [ "scanright", "eraseone", "subone", "skip", "HALT" ]
        let initial = "scanright"
        let finals = ["HALT"]
        let transitions =
                let ht = Turing.StateHashtbl.create 4 in
                Turing.StateHashtbl.add ht "scanright" [(".", "scanright", ".", "RIGHT"); ("1", "scanright", "1", "RIGHT"); ("-", "scanright", "-", "RIGHT"); ("=", "eraseone", ".", "LEFT")];
                Turing.StateHashtbl.add ht "eraseone" [("1", "subone", "=", "LEFT"); ("-", "HALT", ".", "LEFT")];
                Turing.StateHashtbl.add ht "subone"[("1", "subone", "1", "LEFT"); ("-", "skip", "-", "LEFT")]; 
                Turing.StateHashtbl.add ht "skip"[(".", "skip", ".", "LEFT"); ("1", "scanright", ".", "RIGHT")]; 
                ht
                
end

module MyMachine = Turing.Make(Input)

let () =
        let str_input = "111-11=" in
        MyMachine.execute str_input
