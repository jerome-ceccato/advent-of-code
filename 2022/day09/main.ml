(* types *)
type point = {x: int; y: int};;
module PointSet = Set.Make(
  struct
    type t = point
    let compare = compare
  end
);;
type rope = {head: point; tail: point list};;
type simulation = {rope: rope; visited: PointSet.t};;

(* i/o *)
let read_input filename =
  let fc = open_in filename in
  let read_ln () =
      try Some(input_line fc) with End_of_file -> None in
  let rec loop acc = match read_ln () with
    | Some(ln) -> loop (ln :: acc)
    | None -> close_in fc; List.rev acc in
  loop [];;

(* solution *)
let parse_direction = function
  | "U" -> {x = 0; y = -1}
  | "D" -> {x = 0; y = 1}
  | "L" -> {x = -1; y = 0}
  | "R" -> {x = 1; y = 0}
  |  _  -> {x = 0; y = 0};;

let move_towards h t = match (h, t) with
  | _ when h > t -> t + 1
  | _ when h < t -> t - 1
  | _ -> t;;

let move_one_tail head tail = 
  if (abs (head.x - tail.x) > 1) || (abs (head.y - tail.y) > 1) then
    {x = move_towards head.x tail.x; y = move_towards head.y tail.y}
  else tail;;

let rec move_tail head tail = match tail with
  | [] -> []
  | x :: xs -> let new_tail = move_one_tail head x in
    new_tail :: (move_tail new_tail xs);;

let apply_move simulation vector =
  let new_head = {x = simulation.rope.head.x + vector.x;
                  y = simulation.rope.head.y + vector.y} in
  let new_tail = move_tail new_head simulation.rope.tail in
  let new_visited = PointSet.add (List.hd (List.rev new_tail)) simulation.visited in
    {rope = {head = new_head; tail = new_tail}; visited = new_visited};;

let move_rope rope commands =
  let apply_command simulation line =
    let parts = String.split_on_char ' ' line in
    let (dir::n::_) = parts in
    let vector = parse_direction dir in
    let rec apply_n n sim = match n with
      | 0 -> sim
      | n -> apply_n (n - 1) (apply_move sim vector) in
      apply_n (int_of_string n) simulation in
  let simulation = {rope = rope; visited = PointSet.singleton (List.hd (List.rev rope.tail))} in
    List.fold_left apply_command simulation commands;;

let solve n =
  let rope = {head = {x = 0; y = 0}; tail = List.init n (fun _ -> {x = 0; y = 0}) } in
  let sim = move_rope rope (read_input "input") in
    Printf.printf "%d\n" (PointSet.cardinal sim.visited);;

(* part 1 *)
solve 1;;

(* part 2 *)
solve 9;;
