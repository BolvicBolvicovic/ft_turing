let to_list file_name = 
        open_in file_name 
        |> In_channel.input_lines 
        |> List.map (fun str -> List.hd (String.split_on_char '#' str)) 
        |> List.filter (fun str -> "" <> str)

let print_newfile list = List.iter print_endline list

let compile file_name = 
        let trimmed_file = file_name |> to_list in
        if  String.starts_with ~prefix:"alphabet[" (List.hd trimmed_file) && String.ends_with ~suffix:"]" (List.hd trimmed_file) then print_newfile (to_list file_name)
        else invalid_arg ("alphabet is missing or incorrectly formated at start of the file.\n<" ^ List.hd trimmed_file ^ "> should be of the form alphabet[01abc]")
