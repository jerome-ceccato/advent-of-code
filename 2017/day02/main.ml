let read_lines file =
    let contents = In_channel.with_open_text file In_channel.input_all in
    String.split_on_char '\n' contents;;

let split_whitespaces s = 
    s
    |> String.split_on_char ' ' 
    |> List.concat_map (String.split_on_char '\t')
    |> List.filter ((<>) "");;

let parse_ints lines =
    let parse_line line =
        let raw = split_whitespaces line in
        List.map int_of_string raw in
    List.map parse_line lines;;

let get_min_max l = 
    let sorted = List.sort Stdlib.compare l in 
    (List.hd sorted), (sorted |> List.rev |> List.hd);;

let checksum data =
    data
    |> List.map get_min_max
    |> List.map (fun both -> snd both - fst both)
    |> List.fold_left (+) 0;;

let divides_evenly a b =
    if a <= b then
        false
    else
        a mod b == 0;;

let combinations l =
    let inner l a = List.map (fun b -> a,b) l in
    let all = List.map (inner l) l in
    List.flatten all;;

let find_even_div l = 
    combinations l
    |> List.filter (fun both -> divides_evenly (fst both) (snd both))
    |> List.hd
    |> (fun both -> (fst both) / (snd both));;

let part2 data =
    data
    |> List.map find_even_div
    |> List.fold_left (+) 0;;

let input = "input" |> read_lines |> parse_ints;;
Printf.printf "%d\n" (checksum input);;
Printf.printf "%d\n" (part2 input);;
