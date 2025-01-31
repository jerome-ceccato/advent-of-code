exception ParseError of string;;

let read_file file =
    In_channel.with_open_text file In_channel.input_all;;

let count_groups_and_garbage l =
    let rec inner l is_garbage score depth garbage = 
        match is_garbage, l with
        | false, '{' :: xs -> inner xs false (score + depth) (depth + 1) garbage
        | false, '}' :: xs -> inner xs false score (depth - 1) garbage
        | false, ',' :: xs -> inner xs false score depth garbage
        | false, '<' :: xs -> inner xs true score depth garbage
        | false, _ :: xs -> raise (ParseError "Invalid non-garbage character")
        | true, '!' :: xs -> inner (List.tl xs) true score depth garbage
        | true, '>' :: xs -> inner xs false score depth garbage
        | true, _ :: xs -> inner xs true score depth (garbage + 1)
        | _, [] -> (score, garbage) in
    inner l false 0 1 0;;

let input = "input" |> read_file |> String.to_seq |> List.of_seq;;
let score, garbage = count_groups_and_garbage input in
Printf.printf "%d\n" score;
Printf.printf "%d\n" garbage;;
