use std::{
    collections::{HashMap, HashSet},
    fs,
};

#[derive(Debug, Clone, PartialEq, Eq)]
struct Operation {
    left: String,
    op: String,
    right: String,
}

fn get_input(path: &str) -> (HashMap<String, bool>, HashMap<String, Operation>) {
    let input = fs::read_to_string(path).unwrap();
    let (wires, connections) = input.split_once("\n\n").unwrap();

    let mut wire_cache: HashMap<String, bool> = HashMap::new();
    for item in wires.split("\n") {
        let (name, value) = item.split_once(": ").unwrap();
        wire_cache.insert(
            name.to_string(),
            match value {
                "1" => true,
                _ => false,
            },
        );
    }

    let mut gates: HashMap<String, Operation> = HashMap::new();
    for item in connections.split("\n") {
        let parts: Vec<&str> = item.split(" ").collect();
        gates.insert(
            parts[4].to_string(),
            Operation {
                left: parts[0].to_string(),
                op: parts[1].to_string(),
                right: parts[2].to_string(),
            },
        );
    }

    (wire_cache, gates)
}

fn collect_wires(connections: &HashMap<String, Operation>) -> HashSet<String> {
    let mut result: HashSet<String> = HashSet::new();
    for (k, v) in connections {
        result.insert(k.clone());
        result.insert(v.left.clone());
        result.insert(v.right.clone());
    }
    result
}

fn resolve(
    wire: &String,
    wire_cache: &mut HashMap<String, bool>,
    connections: &HashMap<String, Operation>,
) -> bool {
    if wire_cache.contains_key(wire) {
        return wire_cache[wire];
    }

    let op = &connections[wire];
    let left = resolve(&op.left, wire_cache, connections);
    let right = resolve(&op.right, wire_cache, connections);
    let result = match op.op.as_str() {
        "AND" => left && right,
        "OR" => left || right,
        "XOR" => left ^ right,
        _ => unreachable!(),
    };

    wire_cache.insert(wire.clone(), result);
    result
}

fn solve(wire_cache: &mut HashMap<String, bool>, connections: &HashMap<String, Operation>) -> u64 {
    let mut result: u64 = 0;
    let z_outputs = collect_wires(&connections)
        .into_iter()
        .filter(|s| s.starts_with("z"));
    for wire in z_outputs {
        if resolve(&wire, wire_cache, connections) {
            result |= 1 << (wire[1..].parse::<u64>().unwrap());
        }
    }
    result
}

fn swap(connections: &mut HashMap<String, Operation>, left: &String, right: &String) {
    let left_v = connections.remove(left).unwrap();
    let right_v = connections.remove(right).unwrap();
    connections.insert(left.clone(), right_v);
    connections.insert(right.clone(), left_v);
}

fn find_swapped_nodes(connections: &mut HashMap<String, Operation>) -> Vec<String> {
    let mut fixes: Vec<String> = vec![];
    let total_bits = collect_wires(&connections)
        .into_iter()
        .filter(|w| w.starts_with("z"))
        .count() as i32;
    for bit in 0..(total_bits - 1) {
        let res = validate_z(bit as i32, &connections);
        if let Some(wrong) = res {
            let (fix_a, fix_b) = fix_or_children(bit, &wrong, &connections);
            swap(connections, &fix_a, &fix_b);
            fixes.push(fix_a);
            fixes.push(fix_b);
        }
    }

    fixes.sort();
    fixes
}

fn main() {
    let (mut wire_cache, mut connections) = get_input("input");
    let solution = solve(&mut wire_cache, &connections);
    println!("{:?}", solution);

    let fixes = find_swapped_nodes(&mut connections);
    println!("{}", fixes.join(","));
}

impl Operation {
    fn is(&self, a: &str, op: &str, b: &str) -> bool {
        self.op == op
            && ((self.left == a && self.right == b) || (self.left == b && self.right == a))
    }
}

fn match_one<'a, F>(m: F, a: &'a Operation, b: &'a Operation) -> Option<&'a Operation>
where
    F: Fn(&Operation) -> bool,
{
    if m(a) {
        Some(b)
    } else if m(b) {
        Some(a)
    } else {
        None
    }
}

fn validate_z(z: i32, connections: &HashMap<String, Operation>) -> Option<String> {
    let zstr = format!("z{:02}", z);
    validate_z_rec(
        &connections[format!("z{:02}", z).as_str()],
        &zstr,
        z,
        connections,
        "XOR",
    )
}

fn fix_or_children(
    z: i32,
    wrong: &String,
    connections: &HashMap<String, Operation>,
) -> (String, String) {
    if let Some(fixed) = fix(z, wrong, connections) {
        (wrong.clone(), fixed)
    } else {
        let op = &connections[wrong];

        if let Some(fixed) = fix(z, &op.left, connections) {
            (op.left.clone(), fixed)
        } else if let Some(fixed) = fix(z, &op.right, connections) {
            (op.right.clone(), fixed)
        } else {
            panic!()
        }
    }
}

fn fix(z: i32, wrong: &String, connections: &HashMap<String, Operation>) -> Option<String> {
    for other in connections.keys() {
        if !other.starts_with("x") && !other.starts_with("y") && other != wrong {
            let mut test_connections = connections.clone();
            swap(&mut test_connections, wrong, other);
            if validate_z(z, &test_connections) == None {
                return Some(other.clone());
            }
        }
    }
    None
}

fn validate_z_rec(
    zop: &Operation,
    name: &String,
    depth: i32,
    connections: &HashMap<String, Operation>,
    toplevel_op: &str,
) -> Option<String> {
    if depth == 0 {
        if zop.is("x00", "XOR", "y00") {
            return None;
        } else {
            return Some(name.clone());
        }
    } else if depth == 1 {
        if !connections.contains_key(&zop.left) || !connections.contains_key(&zop.right) {
            return Some(name.clone());
        }
        let a = &connections[&zop.left];
        let b = &connections[&zop.right];

        let valid = zop.op == toplevel_op
            && match_one(|op| op.is("x00", "AND", "y00"), a, b)
                .and_then(|op| Some(op.is("x01", "XOR", "y01")))
                .unwrap_or(false);
        if valid {
            return None;
        } else {
            return Some(name.clone());
        }
    } else {
        if !connections.contains_key(&zop.left) || !connections.contains_key(&zop.right) {
            return Some(name.clone());
        }
        let a = &connections[&zop.left];
        let b = &connections[&zop.right];

        if zop.op != toplevel_op {
            return Some(name.clone());
        }

        if let Some(other) = match_one(
            |op| {
                op.is(
                    format!("x{:02}", depth).as_str(),
                    "XOR",
                    format!("y{:02}", depth).as_str(),
                )
            },
            a,
            b,
        ) {
            if !connections.contains_key(&other.left) || !connections.contains_key(&other.right) {
                if other == a {
                    return Some(zop.left.clone());
                } else {
                    return Some(zop.right.clone());
                }
            }
            let a2 = &connections[&other.left];
            let b2 = &connections[&other.right];

            if other.op != "OR" {
                if other == a {
                    return Some(zop.left.clone());
                } else {
                    return Some(zop.right.clone());
                }
            }

            if let Some(other2) = match_one(
                |op| {
                    op.is(
                        format!("x{:02}", depth - 1).as_str(),
                        "AND",
                        format!("y{:02}", depth - 1).as_str(),
                    )
                },
                a2,
                b2,
            ) {
                let next_name = if other2 == a2 {
                    other.left.clone()
                } else {
                    other.right.clone()
                };

                return validate_z_rec(other2, &next_name, depth - 1, connections, "AND");
            } else {
                if other == a {
                    return Some(zop.left.clone());
                } else {
                    return Some(zop.right.clone());
                }
            }
        } else {
            return Some(name.clone());
        }
    }
}

// z0 = x0 ^ y0
// z1 = (x0 & y0) ^ (x1 ^ y1)
// z2 = ((x1 & y1) | ((x1 ^ y1) & (x0 & y0))) ^ (x2 ^ y2)
// z3 = (((x2 ^ y2) & ((x1 & y1) | ((x1 ^ y1) & (x0 & y0)))) | (x2 & y2)) ^ (x3 ^ y3)

// zn = (xn ^ yn) ^ ((xn-1 & yn-1) | (zn-1#left & zn-1#right))
