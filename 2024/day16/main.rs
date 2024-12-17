use std::{
    cmp::Reverse,
    collections::HashMap,
    fs,
    ops::{self},
};

use priority_queue::PriorityQueue;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, PartialOrd, Ord)]
struct Point {
    x: i32,
    y: i32,
}

impl Point {
    const NORTH: Point = Point { x: 0, y: -1 };
    const EAST: Point = Point { x: 1, y: 0 };
    const SOUTH: Point = Point { x: 0, y: 1 };
    const WEST: Point = Point { x: -1, y: 0 };

    const DIRECTIONS: &'static [Point] = &[Point::NORTH, Point::EAST, Point::SOUTH, Point::WEST];
}

impl ops::Neg for &Point {
    type Output = Point;
    fn neg(self) -> Self::Output {
        Point {
            x: -self.x,
            y: -self.y,
        }
    }
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

fn get_input(path: &str) -> Vec<Vec<char>> {
    fs::read_to_string(path)
        .unwrap()
        .split("\n")
        .map(|line| line.chars().collect())
        .collect()
}

fn find_start(world: &Vec<Vec<char>>) -> Point {
    for y in 0..world.len() {
        for x in 0..world[y].len() {
            if world[y][x] == 'S' {
                return Point {
                    x: x as i32,
                    y: y as i32,
                };
            }
        }
    }
    panic!("No start");
}

#[derive(Debug, Hash, PartialEq, Eq)]
struct Path {
    head: Point,
    direction: Point,
    score: i32,
}

fn get(world: &Vec<Vec<char>>, point: &Point) -> Option<char> {
    if point.y >= 0
        && point.y < world.len() as i32
        && point.x >= 0
        && point.x < world[point.y as usize].len() as i32
    {
        return Some(world[point.y as usize][point.x as usize]);
    }
    return None;
}

fn next_score(score: i32, old_dir: &Point, new_dir: &Point) -> i32 {
    if old_dir == new_dir {
        score + 1
    } else {
        score + 1001
    }
}

fn pathfind(world: &Vec<Vec<char>>, start: &Point, start_dir: &Point) -> i32 {
    let mut queue = PriorityQueue::new();
    queue.push(
        Path {
            head: start.clone(),
            direction: start_dir.clone(),
            score: 0,
        },
        Reverse(0),
    );

    let mut best_scores: HashMap<Point, i32> = HashMap::new();

    while let Some((item, _)) = queue.pop() {
        if best_scores.contains_key(&item.head) && best_scores[&item.head] < item.score {
            continue;
        }

        for direction in Point::DIRECTIONS.iter() {
            if direction != &-&item.direction {
                let next = &item.head + direction;
                if let Some(neighbor) = get(world, &next) {
                    if neighbor == '.' || neighbor == 'E' {
                        let score = next_score(item.score, &item.direction, direction);
                        if neighbor == '.' {
                            best_scores.insert(next, score);
                            queue.push(
                                Path {
                                    head: next,
                                    direction: direction.clone(),
                                    score,
                                },
                                Reverse(score),
                            );
                        } else {
                            return score;
                        }
                    }
                }
            }
        }
    }

    return -1;
}

fn main() {
    let world = get_input("input");
    let start = find_start(&world);
    let direction = Point::EAST;

    // Horribly slow because of the PriorityQueue hashing + the map is too large
    // Can't work for p2 because saving the paths would take hours instead of seconds
    println!("{}", pathfind(&world, &start, &direction));
}
