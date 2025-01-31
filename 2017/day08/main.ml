let read_lines file =
    let contents = In_channel.with_open_text file In_channel.input_all in
    String.split_on_char '\n' contents;;

let parse_line line = line 
    |> String.split_on_char ' '
    |> Array.of_list;;

let parse_cond_op = function
    | ">" -> (>)
    | "<" -> (<)
    | ">=" -> (>=)
    | "<=" -> (<=)
    | "!=" -> (!=)
    | "==" -> (==)
    | _ -> (fun _ _ -> false)

let parse_inst_op = function
    | "inc" -> (+)
    | "dec" -> (-)
    | _ -> (fun lhs _ -> lhs)

let reg_get reg what =
    match (Hashtbl.find_opt reg what) with 
    | Some v -> v
    | None -> 0

let max_reg reg =
    Hashtbl.fold (fun k v acc -> (max acc v)) reg 0;;

let process_all reg instructions =
    let process reg line =
        let cond = parse_cond_op line.(5) in
        let cond_lhs = reg_get reg line.(4) in
        let cond_rhs = int_of_string line.(6) in
        if (cond cond_lhs cond_rhs) then
            let op = parse_inst_op line.(1) in
            let target = line.(0) in
            let value = int_of_string line.(2) in
            Hashtbl.replace reg target (op (reg_get reg target) value) in
    let process_and_get_max reg line =
        process reg line;
        max_reg reg in
    List.fold_left (fun acc line -> (max acc (process_and_get_max reg line))) 0 instructions;;


let input = "input" |> read_lines |> List.map parse_line;;

let reg = Hashtbl.create 1000 in
let best = process_all reg input in
Printf.printf "%d\n" (max_reg reg);
Printf.printf "%d\n" best;;
