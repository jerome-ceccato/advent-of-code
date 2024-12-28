use std::{
    collections::{HashMap, HashSet},
    fs,
};

fn get_input(path: &str) -> Vec<(String, String)> {
    fs::read_to_string(path)
        .unwrap()
        .split("\n")
        .map(|l| l.split_once("-").unwrap())
        .map(|(a, b)| (a.to_string(), b.to_string()))
        .collect()
}

fn build_network_map(raw: &Vec<(String, String)>) -> HashMap<String, Vec<String>> {
    let mut result: HashMap<String, Vec<String>> = HashMap::new();
    for (lhs, rhs) in raw {
        result
            .entry(lhs.clone())
            .or_insert(vec![])
            .push(rhs.clone());
        result
            .entry(rhs.clone())
            .or_insert(vec![])
            .push(lhs.clone());
    }
    result
}

fn is_fully_linked(network: &HashMap<String, Vec<String>>, slice: &HashSet<String>) -> bool {
    for left in slice {
        for right in slice {
            if left != right {
                if !network[left].contains(right) {
                    return false;
                }
            }
        }
    }
    true
}

fn find_three_links(network: &HashMap<String, Vec<String>>) -> Vec<HashSet<String>> {
    let keys: Vec<&String> = network.keys().collect();
    let mut result: Vec<HashSet<String>> = vec![];

    for a in 0..keys.len() {
        for b in (a + 1)..keys.len() {
            for c in (b + 1)..keys.len() {
                let current = HashSet::from_iter(
                    vec![keys[a].clone(), keys[b].clone(), keys[c].clone()].into_iter(),
                );
                if is_fully_linked(network, &current) {
                    result.push(current);
                }
            }
        }
    }
    result
}

fn is_candidate(slice: &HashSet<String>) -> bool {
    for s in slice {
        if s.starts_with("t") {
            return true;
        }
    }
    false
}

fn can_fit(network: &HashMap<String, Vec<String>>, lan: &HashSet<String>, other: &String) -> bool {
    for item in lan {
        if !network[item].contains(other) {
            return false;
        }
    }
    true
}

fn find_next_n_links(
    network: &HashMap<String, Vec<String>>,
    current: &Vec<HashSet<String>>,
) -> Vec<HashSet<String>> {
    let mut next: Vec<HashSet<String>> = vec![];
    for lan in current {
        for other in network.keys() {
            if !lan.contains(other) {
                if can_fit(network, lan, other) {
                    let mut next_slice = lan.clone();
                    next_slice.insert(other.clone());
                    if !next.contains(&next_slice) {
                        next.push(next_slice);
                    }
                }
            }
        }
    }
    next
}

fn into_password(lan: HashSet<String>) -> String {
    let mut items: Vec<String> = lan.into_iter().collect();
    items.sort();
    items.join(",")
}

fn main() {
    let network = build_network_map(&get_input("input"));
    let three_links = find_three_links(&network);
    let candidates: Vec<HashSet<String>> = three_links
        .clone()
        .into_iter()
        .filter(is_candidate)
        .collect();
    println!("{:?}", candidates.len());

    let mut current = three_links;
    while current.len() > 1 {
        current = find_next_n_links(&network, &current);
    }
    println!("{:?}", into_password(current.first().unwrap().clone()));
}
