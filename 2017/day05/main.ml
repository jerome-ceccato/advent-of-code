module StringSet = Set.Make(String);;

let read_lines file =
    let contents = In_channel.with_open_text file In_channel.input_all in
    String.split_on_char '\n' contents;;

let p1_next_offset offset = offset + 1;;

let p2_next_offset offset = 
    if offset >= 3 then
        offset - 1
    else
        offset + 1;;

let process_jmp next_offset_f orig =
    let arr = Array.copy orig in
    let rec inner arr i acc =
        let offset = arr.(i) in
        arr.(i) <- (next_offset_f offset);
        if (offset + i) < 0 || (offset + i) >= (Array.length arr) then
            acc + 1
        else
        inner arr (offset + i) (acc + 1) in
    inner arr 0 0;;

let input = "input" |> read_lines |> List.map int_of_string |> Array.of_list;;

let part1 = process_jmp p1_next_offset input in
Printf.printf "%d\n" part1;;
let part2 = process_jmp p2_next_offset input in
Printf.printf "%d\n" part2;;
