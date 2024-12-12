use std::{collections::HashMap, fs};

macro_rules! insert_add {
    ($map:ident, $key:expr, $value:expr) => {
        $map.insert($key, $map.get(&$key).unwrap_or(&0) + $value);
    };
}

fn get_input(path: &str) -> HashMap<u64, u64> {
    let mut stones = HashMap::<u64, u64>::new();
    let input: Vec<u64> = fs::read_to_string(path)
        .unwrap()
        .split(" ")
        .map(|n| n.parse::<u64>().unwrap())
        .collect();
    for n in input {
        insert_add!(stones, n, 1);
    }
    return stones;
}

fn split_number(n: u64) -> (u64, u64) {
    let nstr = n.to_string();
    let both = nstr.split_at(nstr.len() / 2);

    return (both.0.parse().unwrap(), both.1.parse().unwrap());
}

fn blink(stones: HashMap<u64, u64>) -> HashMap<u64, u64> {
    let mut next = HashMap::<u64, u64>::new();

    for (n, amount) in stones {
        if n == 0 {
            insert_add!(next, 1, amount);
        } else if n.to_string().len() % 2 == 0 {
            let values = split_number(n);
            insert_add!(next, values.0, amount);
            insert_add!(next, values.1, amount);
        } else {
            insert_add!(next, n * 2024, amount);
        }
    }
    return next;
}

fn blink_n(mut stones: HashMap<u64, u64>, n: i32) -> HashMap<u64, u64> {
    for _ in 0..n {
        stones = blink(stones);
    }
    return stones;
}

fn main() {
    let mut stones = get_input("input");

    stones = blink_n(stones, 25);
    println!("{:?}", stones.values().sum::<u64>());

    stones = blink_n(stones, 50);
    println!("{:?}", stones.values().sum::<u64>());
}
