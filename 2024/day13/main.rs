use std::{
    convert::identity,
    fs,
    ops::{self},
};

use regex::Regex;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct Point {
    x: i32,
    y: i32,
}

impl ops::Add for &Point {
    type Output = Point;
    fn add(self, rhs: &Point) -> Self::Output {
        Point {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

impl ops::Mul<&i32> for &Point {
    type Output = Point;
    fn mul(self, rhs: &i32) -> Self::Output {
        Point {
            x: self.x * rhs,
            y: self.y * rhs,
        }
    }
}

#[derive(Debug)]
struct Machine {
    a: Point,
    b: Point,
    prize: Point,
}

fn get_input(path: &str) -> Vec<Machine> {
    let re = Regex::new(r"\d+").unwrap();
    fs::read_to_string(path)
        .unwrap()
        .split("\n\n")
        .map(|block| {
            re.find_iter(block)
                .map(|m| m.as_str().parse::<i32>().unwrap())
                .collect()
        })
        .map(|raw: Vec<i32>| Machine {
            a: Point {
                x: raw[0],
                y: raw[1],
            },
            b: Point {
                x: raw[2],
                y: raw[3],
            },
            prize: Point {
                x: raw[4],
                y: raw[5],
            },
        })
        .collect()
}

fn get_cost(machine: &Machine, attempt: (i32, i32)) -> Option<i32> {
    if (&(&machine.a * &attempt.0) + &(&machine.b * &attempt.1)) == machine.prize {
        Some(attempt.0 * 3 + attempt.1)
    } else {
        None
    }
}

fn find_min_token_cost(machine: &Machine) -> Option<i32> {
    (0..101)
        .flat_map(|y| (0..101).map(move |x| (x, y)))
        .filter_map(|p| get_cost(machine, p))
        .min()
}

fn main() {
    let blocks = get_input("input");

    let part1: i32 = blocks
        .iter()
        .map(find_min_token_cost)
        .filter_map(identity)
        .sum();
    println!("{:?}", part1);
}
