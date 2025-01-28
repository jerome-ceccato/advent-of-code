let parse_input file =
    let contents = In_channel.with_open_text file In_channel.input_all in
    int_of_string contents;;

(* get next direction in the spiral *)
let rotate (x, y) = match x,y with
    | 1,0 -> 0,-1
    | 0,-1 -> -1,0
    | -1,0 -> 0,1
    | 0,1 -> 1,0
    | _ -> x,y

(* increase spiral size if direction just became right *)
let next_sz sz (dx, dy) =
    if dx == 1 && dy == 0 then sz + 1 else sz;;

let spiral_get_position v =
    (* recursively go through the spiral
    v is the target value
    curr is the current value
    x,y is the current position in 2d
    dx,dy is the current direction
    sz is the current size of the spiral, to determine when to turn *)
    let rec inner v curr (x, y) (dx, dy) sz =
        if curr == v then
            x,y
        else
            let tx = x + dx and ty = y + dy in
            if (abs tx) > sz || (abs ty) > sz then
                let ndx, ndy = rotate (dx, dy) in
                inner v curr (x, y) (ndx, ndy) (next_sz sz (ndx, ndy))
            else
                inner v (curr + 1) (tx, ty) (dx, dy) sz in
        inner v 1 (0,0) (1,0) 1;;

let distance_to_center (x, y) =
    abs(x) + abs(y);;

let cache_get cache v =
    match (Hashtbl.find_opt cache v) with
    | Some a -> a
    | None -> 0;;

let calculate_value cache (x,y) =
    let offsets = [
        (1, 0); 
        (1, -1);
        (0, -1);
        (-1, -1);
        (-1, 0);
        (-1, 1);
        (0, 1);
        (1, 1)
    ] in
    offsets
    |> List.map (fun (dx,dy) -> (x+dx),(y+dy))
    |> List.fold_left (fun acc a -> acc + (cache_get cache a)) 0;;

let cached_spiral_get v =
    let cache = Hashtbl.create v in
    Hashtbl.add cache (0,0) 1;
    let rec inner cache v (x,y) (dx,dy) sz =
        let tx = x + dx and ty = y + dy in
        if (abs tx) > sz || (abs ty) > sz then
            let ndx, ndy = rotate (dx, dy) in
            inner cache v (x, y) (ndx, ndy) (next_sz sz (ndx, ndy))
        else
            let this = calculate_value cache (tx, ty) in
            if this > v then
                this
            else
                (Hashtbl.add cache (tx, ty) this;
                inner cache v (tx, ty) (dx, dy) sz) in
    inner cache v (0,0) (1,0) 1;;

let input = "input" |> parse_input;;
Printf.printf "%d\n" (input |> spiral_get_position |> distance_to_center);;
Printf.printf "%d\n" (input |> cached_spiral_get);;
