let read_file file =
    In_channel.with_open_text file In_channel.input_all;;

let split_whitespaces s = s
    |> String.split_on_char ' ' 
    |> List.concat_map (String.split_on_char '\t')
    |> List.filter ((<>) "");;

let find_max arr =
    let max_index arr acc (i, a) =
        if arr.(acc) < a then i else acc in
    Array.to_seqi arr |> Seq.fold_left (max_index arr) 0;;

let redistribute arr i amount =
    let range = List.init amount (fun x -> x + 1) in
    let inc_offset arr i offset =
        let target = (i + offset) mod (Array.length arr) in
        arr.(target) <- (arr.(target) + 1) in
    arr.(i) <- 0;
    List.iter (inc_offset arr i) range;;

let detect_infinite_loop orig =
    let arr = Array.copy orig in
    let cache = Hashtbl.create 1000 in
    let rec inner cache arr acc =
        if Hashtbl.find_opt cache arr != None then
            acc, (acc - (Hashtbl.find cache arr))
        else
            let largest_index = find_max arr in
            Hashtbl.add cache (Array.copy arr) acc;
            redistribute arr largest_index arr.(largest_index);
            inner cache arr (acc + 1) in
    inner cache arr 0;;

let input = "input" |> read_file |> split_whitespaces |> List.map int_of_string |> Array.of_list;;

let (part1, part2) = detect_infinite_loop input in
Printf.printf "%d\n" part1;
Printf.printf "%d\n" part2;;
