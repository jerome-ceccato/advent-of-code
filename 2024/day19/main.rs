use std::{
    collections::{HashMap, HashSet},
    fs,
};

fn get_input(path: &str) -> (HashSet<String>, Vec<String>) {
    let blocks: Vec<String> = fs::read_to_string(path)
        .unwrap()
        .split("\n\n")
        .map(String::from)
        .collect();
    let patterns: HashSet<String> = blocks[0].split(", ").map(String::from).collect();
    let designs: Vec<String> = blocks[1].split("\n").map(String::from).collect();

    (patterns, designs)
}

fn ways_to_build_design(
    patterns: &HashSet<String>,
    design: &str,
    memo: &mut HashMap<String, u64>,
) -> u64 {
    if design.is_empty() {
        return 1;
    }

    if memo.contains_key(design) {
        return memo[design];
    }

    let mut ways = 0;
    for pattern in patterns.iter() {
        if design.starts_with(pattern) {
            ways += ways_to_build_design(patterns, &design[pattern.len()..], memo);
        }
    }
    memo.insert(design.to_string(), ways);
    return ways;
}

fn main() {
    let (patterns, designs) = get_input("input");
    let mut memo: HashMap<String, u64> = HashMap::new();
    let design_combinations: Vec<u64> = designs
        .iter()
        .map(|d| ways_to_build_design(&patterns, d, &mut memo))
        .collect();

    let part1 = design_combinations.iter().filter(|&x| *x > 0).count();
    let part2 = design_combinations.iter().sum::<u64>();
    println!("{:?}", part1);
    println!("{:?}", part2);
}
