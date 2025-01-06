#![feature(linked_list_cursors)]
#![feature(linked_list_remove)]

use std::{collections::{HashMap, LinkedList}, fs};

use regex::Regex;

fn get_input(path: &str) -> (i64, i64) {
    let re = Regex::new(r"(\d+) players; last marble is worth (\d+) points").unwrap();
    let input = fs::read_to_string(path).unwrap();
    let res = re.captures(&input).unwrap();
    (res[1].parse().unwrap(), res[2].parse().unwrap())
}

fn run(player_count: i64, last: i64) -> i64 {
    let mut circle: LinkedList<i64> = LinkedList::new();
    let mut players: HashMap<i64, i64> = HashMap::new();

    circle.push_front(0);
    let mut current = circle.cursor_front_mut();
    for turn in 1..=last {
        if turn % 23 == 0 {
            for _ in 0..7 {
                current.move_prev();
                if current.current() == None {
                    current.move_prev();
                }
            }
            let removed = current.remove_current().unwrap();
            if current.current() == None {
                current.move_next();
            }
            let current_player = turn % player_count;
            *players.entry(current_player).or_default() += turn + removed;
        } else {
            current.move_next();
            if current.current() == None {
                current.move_next();
            }
            current.insert_after(turn);
            current.move_next();
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
