let read_file file =
    In_channel.with_open_text file In_channel.input_all;;

let as_ints contents = contents
    |> String.split_on_char ','
    |> List.map int_of_string;;

let print_circle circle =
    Array.iter (Printf.printf "%d ") circle;
    Printf.printf "\n";;

let knot_hash circle lenghts =
    let swap circle pos last =
        let sz = Array.length circle in
        let a = circle.(pos mod sz) in
        let b = circle.(last mod sz) in
        circle.(last mod sz) <- a;
        circle.(pos mod sz) <- b in
    let rec rev_circle circle pos last =
        if pos < last then
            (swap circle pos last;
            rev_circle circle (pos + 1) (last - 1)) in
    let inner circle pos skip len =
        rev_circle circle pos (pos + len - 1);
        (pos + len + skip) mod (Array.length circle) in
    let _ = List.fold_left (fun (pos, skip) len -> (inner circle pos skip len, skip + 1)) (0, 0) lenghts in
    circle.(0) * circle.(1);;

let decode_input input = input
    |> String.to_seq
    |> Seq.map Char.code
    |> List.of_seq;;

let add_std_suffix l = l @ [17; 31; 73; 47; 23];;

let repeat_list n l =
    List.fold_left (fun res _ -> res @ l) [] (List.init n (fun _ -> 0));;

let xor_chunks size arr =
    let starts = List.init ((Array.length arr) / size) (fun x -> x * size) in
    let xor_from pos size arr =
        let subarr = Array.sub arr (pos + 1) (size - 1) in
        Array.fold_left (lxor) arr.(pos) subarr in
    List.map (fun pos -> xor_from pos size arr) starts;;

let to_hex_string hash =
    let as_hex n = [(n lsr 4) land 15; n land 15] in
    let flat = List.concat_map as_hex hash in
    let hex_codes = List.map (Printf.sprintf "%x") flat in
    String.concat "" hex_codes;;

let input_ints = "input" |> read_file |> as_ints in
let circle = List.init 256 (fun a -> a) |> Array.of_list in
Printf.printf "%d\n" (knot_hash circle input_ints);;

let real_input = "input" |> read_file |> decode_input |> add_std_suffix |> repeat_list 64 in
let sparse_hash = List.init 256 (fun a -> a) |> Array.of_list in
let _ = knot_hash sparse_hash real_input in
let dense_hash = xor_chunks 16 sparse_hash in
let hash_str = to_hex_string dense_hash in
Printf.printf "%s\n" hash_str;;
