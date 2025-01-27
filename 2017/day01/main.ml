let read_input filename =
    let fc = open_in filename in
    let line = input_line fc in
    close_in fc; line;;

let explode s = List.init (String.length s) (String.get s);;

let as_int l = List.map (fun c -> Char.code c - Char.code '0') l;;

let prep_rotate l = match l with
    | xs :: s -> xs :: s @ [xs]
    | s -> s;;

let rec part1 l acc = match l with 
    | a :: b :: s -> part1 (b :: s) (acc + match a == b with | true -> a | false -> 0)
    | _ -> acc;;


let split_half l =
    let rec split_n l left n =
        if n == 0 then
            List.rev left, l
        else
            match l with
            | xs :: s -> split_n s (xs :: left) (n - 1)
            | [] -> List.rev left, l in
    split_n l [] ((List.length l) / 2);;

let rec part2 left right acc = match left, right with
    | xa :: a, xb :: b -> part2 a b (acc + match xa == xb with | true -> xa * 2 | false -> 0)
    | _ -> acc;;

let input = "input" |> read_input |> explode |> as_int;;

let p1 = part1 (prep_rotate input) 0 in
Printf.printf "%d\n" p1;;

let l, r = split_half input in
let p2 = part2 l r 0 in
Printf.printf "%d\n" p2;;
