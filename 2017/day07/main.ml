module StringSet = Set.Make(String);;

let read_lines file =
    let contents = In_channel.with_open_text file In_channel.input_all in
    String.split_on_char '\n' contents;;

type disc = {weight: int; children: string list};;

let extract_children s = s
    |> String.split_on_char ','
    |> List.map (fun p -> if p.[0] == ' ' then String.sub p 1 (String.length p - 1) else p);;

let rec parse_children = function
    | "" -> []
    | s ->
        if String.starts_with ~prefix:" -> " s then
            extract_children (String.sub s 4 (String.length s - 4))
        else
            parse_children (String.sub s 1 (String.length s - 1));;

let parse_line_into hm line =
    let s, d = Scanf.sscanf line "%s (%d)" (fun s w -> s, {weight = w; children = (parse_children line)}) in
    Hashtbl.add hm s d;; 

let parse_tree input =
    let hm = Hashtbl.create (List.length input) in
    List.iter (parse_line_into hm) input;
    hm;;

let find_root hm = 
    let nodes = StringSet.of_seq (Hashtbl.to_seq_keys hm) in
    let children = StringSet.of_seq (Seq.concat_map (fun d -> List.to_seq d.children) (Hashtbl.to_seq_values hm)) in
    List.hd (StringSet.to_list (StringSet.diff nodes children));;


let rec get_full_weight hm root =
    let this = Hashtbl.find hm root in
    let children_w = List.fold_left (fun acc cstr -> acc + (get_full_weight hm cstr)) 0 this.children in
    this.weight + children_w;;

let rec get_diff_offset = function
    | (sa,a) :: (sb,b) :: (sc,c) :: rest -> 
        if a != b || a != c || b != c then
            if a != b && a == c then
                (sb, a - b)
            else if a != c && a == b then
                (sc, a - c)
            else
                (sa, b - a) 
        else
            get_diff_offset ((sb,b) :: (sc,c) :: rest)
    | _ -> ("", 0) (* No children or cannot find odd-one out *)
;;

let rec find_unbalanced_weight hm root = 
    let this = Hashtbl.find hm root in
    let children_w = List.map (fun cstr -> cstr, (get_full_weight hm cstr)) this.children in
    List.iter (fun c -> Printf.printf "%s: %d\n" (fst c) (snd c)) children_w;
    let offset = get_diff_offset children_w in
    
    if (snd offset) == 0 then 
        0 (* Children are balanced, self is the wrong one *)
    else
        let inner = find_unbalanced_weight hm (fst offset) in
        if inner == 0 then
            (* offset[0] is the unbalanced one, return its fixed weight *)
            (Hashtbl.find hm (fst offset)).weight + (snd offset)
        else
            (* The unbalanced weight was found downstream, just forward it up *)
            inner;;

let input = "input" |> read_lines |> parse_tree;;
let root = find_root input;;

Printf.printf "%s\n" root;;
Printf.printf "%d\n" (find_unbalanced_weight input root);;
