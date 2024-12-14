use std::{
    collections::HashSet,
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

#[derive(Debug)]
struct Robot {
    pos: Point,
    velocity: Point,
}

fn get_input(path: &str) -> Vec<Robot> {
    let re = Regex::new(r"-?\d+").unwrap();
    fs::read_to_string(path)
        .unwrap()
        .split("\n")
        .map(|block| {
            re.find_iter(block)
                .map(|m| m.as_str().parse::<i32>().unwrap())
                .collect()
        })
        .map(|raw: Vec<i32>| Robot {
            pos: Point {
                x: raw[0],
                y: raw[1],
            },
            velocity: Point {
                x: raw[2],
                y: raw[3],
            },
        })
        .collect()
}

fn run(robots: &mut Vec<Robot>, room_size: &Point, time: i32) {
    for robot in robots {
        robot.pos = Point {
            x: (robot.pos.x + robot.velocity.x * time).rem_euclid(room_size.x),
            y: (robot.pos.y + robot.velocity.y * time).rem_euclid(room_size.y),
        }
    }
}

fn count_quadrants(robots: &Vec<Robot>, room_size: &Point) -> Vec<i32> {
    let mut quadrants = vec![0, 0, 0, 0];

    let mid_point = Point {
        x: room_size.x / 2,
        y: room_size.y / 2,
    };
    for robot in robots {
        if robot.pos.x < mid_point.x {
            if robot.pos.y < mid_point.y {
                quadrants[0] += 1;
            } else if robot.pos.y > mid_point.y {
                quadrants[1] += 1;
            }
        } else if robot.pos.x > mid_point.x {
            if robot.pos.y < mid_point.y {
                quadrants[2] += 1;
            } else if robot.pos.y > mid_point.y {
                quadrants[3] += 1;
            }
        }
    }

    return quadrants;
}

fn show_map(robots: &Vec<Robot>, room_size: &Point) {
    let mut board: Vec<Vec<char>> = vec![vec!['.'; room_size.x as usize]; room_size.y as usize];

    for robot in robots {
        board[robot.pos.y as usize][robot.pos.x as usize] = '#';
    }

    let output: String = board
        .into_iter()
        .map(|line| line.into_iter().collect())
        .collect::<Vec<String>>()
        .join("\n");
    println!("{}", output);
}

fn is_tree_candidate(robots: &Vec<Robot>) -> bool {
    // We assume the tree is centered and symetric, so all quadrants must have an equal value (that didn't work)
    // count_quadrants(robots, room_size)
    //     .windows(2)
    //     .all(|q| q[0] == q[1])

    // Let's assume it only works if none of the drones are overlapping
    robots.len() == robots.iter().map(|r| r.pos).collect::<HashSet<_>>().len()
}

fn main() {
    let mut robots = get_input("input");
    let room = Point { x: 101, y: 103 };
    run(&mut robots, &room, 100);
    let safety: i32 = count_quadrants(&robots, &room).into_iter().product();

    println!("{:?}", safety);

    robots = get_input("input");

    let mut runs = 0;
    while !is_tree_candidate(&robots) {
        run(&mut robots, &room, 1);
        runs += 1;
    }
    // show_map(&robots, &room);
    println!("{}", runs);
}
