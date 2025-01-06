use std::{collections::HashMap, fs};

use regex::Regex;

fn get_input(path: &str) -> (i64, i64) {
    let re = Regex::new(r"(\d+) players; last marble is worth (\d+) points").unwrap();
    let input = fs::read_to_string(path).unwrap();
    let res = re.captures(&input).unwrap();
    (res[1].parse().unwrap(), res[2].parse().unwrap())
}

fn run(player_count: i64, last: i64) -> i64 {
    let mut circle: Vec<i64> = Vec::with_capacity(last as usize + 1);
    let mut players: HashMap<i64, i64> = HashMap::new();
    let mut current: usize = 0;

    circle.push(0);
    for turn in 1..=last {
        if turn % 23 == 0 {
            let pos = (current + circle.len() - 7) % circle.len();
            let removed = circle.remove(pos);
            let current_player = turn % player_count;
            *players.entry(current_player).or_default() += turn + removed;
            current = pos;
        } else {
            let pos = ((current + 1) % circle.len()) + 1;
            circle.insert(pos, turn);
            current = pos;
        }
    }

    players.into_values().max().unwrap_or(0)
}

fn main() {
    let (players, last) = get_input("input");

    let result = run(players, last);
    println!("{:?}", result);

    let result2 = run(players, last * 100);
    println!("{:?}", result2);
}
