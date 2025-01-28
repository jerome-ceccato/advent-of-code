module StringSet = Set.Make(String);;

let read_lines file =
    let contents = In_channel.with_open_text file In_channel.input_all in
    String.split_on_char '\n' contents;;

let split_whitespaces s = s
    |> String.split_on_char ' ' 
    |> List.concat_map (String.split_on_char '\t')
    |> List.filter ((<>) "");;

let is_valid_passphrase l =
    StringSet.of_list l |> StringSet.cardinal |> (=) (List.length l);;

let sort_str str = str
    |> String.to_seq
    |> List.of_seq
    |> List.sort Char.compare
    |> List.to_seq
    |> String.of_seq

let input = "input" |> read_lines |> List.map split_whitespaces;;

let part1 = List.filter is_valid_passphrase input |> List.length in
Printf.printf "%d\n" part1;;

let part2 = input |> List.map (List.map sort_str) |> List.filter is_valid_passphrase |> List.length in
Printf.printf "%d\n" part2;;
