use std::fs;

struct Node {
    children: Vec<Node>,
    metadata: Vec<i32>,
}

fn get_input(path: &str) -> Vec<i32> {
    fs::read_to_string(path)
        .unwrap()
        .split(" ")
        .map(|l| l.parse::<i32>().unwrap())
        .collect()
}

fn decode_tree(raw: &Vec<i32>, i: &mut usize) -> Node {
    let n_children = raw[*i];
    let n_metadata = raw[*i + 1];
    let mut res = Node { children: vec![], metadata: vec![] };

    *i += 2;
    for _ in 0..n_children {
        res.children.push(decode_tree(raw, i));
    }
    for _ in 0..n_metadata {
        res.metadata.push(raw[*i]);
        *i += 1;
    }

    res
}

fn sum_metadata(tree: &Node) -> i32 {
    tree.metadata.iter().sum::<i32>() + tree.children.iter().map(sum_metadata).sum::<i32>()
}

fn sum_metadata_ref(tree: &Node) -> i32 {
    if tree.children.is_empty() {
        tree.metadata.iter().sum::<i32>()
    } else {
        tree.metadata
            .iter()
            .map(|&m| tree.children.get((m - 1) as usize).map_or(0, sum_metadata_ref))
            .sum::<i32>()
    }
}

fn main() {
    let raw = get_input("input");
    let tree = decode_tree(&raw, &mut 0);

    let part1 = sum_metadata(&tree);
    println!("{:?}", part1);

    let part2 = sum_metadata_ref(&tree);
    println!("{:?}", part2);
}
