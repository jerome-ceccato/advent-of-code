use std::{collections::HashSet, fs, ops};

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, PartialOrd, Ord)]
struct Point {
    x: i32,
    y: i32,
}

impl Point {
    const ZERO: Point = Point { x: 0, y: 0 };
    const NORTH: Point = Point { x: 0, y: -1 };
    const EAST: Point = Point { x: 1, y: 0 };
    const SOUTH: Point = Point { x: 0, y: 1 };
    const WEST: Point = Point { x: -1, y: 0 };

    const DIRECTIONS: &'static [Point] = &[Point::NORTH, Point::EAST, Point::SOUTH, Point::WEST];
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

fn get_input(path: &str) -> Vec<Point> {
    fs::read_to_string(path)
        .unwrap()
        .split("\n")
        .map(|l| {
            let both: Vec<_> = l.split(",").collect();
            Point {
                x: both[0].parse().unwrap(),
                y: both[1].parse().unwrap(),
            }
        })
        .collect()
}

fn make_world(size: usize, bytes: Vec<Point>) -> Vec<Vec<bool>> {
    let mut world = vec![vec![false; size]; size];
    for p in bytes {
        world[p.y as usize][p.x as usize] = true;
    }
    world
}

fn can_travel(world: &Vec<Vec<bool>>, point: &Point) -> bool {
    if point.y >= 0
        && point.y < world.len() as i32
        && point.x >= 0
        && point.x < world[point.y as usize].len() as i32
    {
        return !world[point.y as usize][point.x as usize];
    }
    return false;
}

fn pathfind(world: &Vec<Vec<bool>>, size: usize) -> i32 {
    let mut visited: HashSet<Point> = HashSet::new();
    let mut queue: Vec<Point> = vec![Point::ZERO];
    let mut depth = 0;

    let target = Point {
        x: size as i32 - 1,
        y: size as i32 - 1,
    };

    while !queue.is_empty() {
        let mut next_queue: Vec<Point> = vec![];
        while let Some(current) = queue.pop() {
            if current == target {
                return depth;
            } else if visited.contains(&current) {
                continue;
            }

            visited.insert(current);
            for dir in Point::DIRECTIONS {
                let next = &current + dir;
                if can_travel(world, &next) {
                    next_queue.push(next);
                }
            }
        }
        depth += 1;
        queue = next_queue;
    }
    -1
}

fn part1() {
    let bytes = get_input("input");
    let world_size = 71;
    let stripped_bytes: Vec<Point> = bytes.into_iter().take(1024).collect();
    println!(
        "{:?}",
        pathfind(&make_world(world_size, stripped_bytes), world_size)
    );
}

fn part2() {
    let bytes = get_input("input");
    let world_size = 71;

    for i in 1024..bytes.len() {
        let stripped_bytes: Vec<Point> = bytes.clone().into_iter().take(i).collect();
        if pathfind(&make_world(world_size, stripped_bytes), world_size) == -1 {
            println!("{},{}", bytes[i - 1].x, bytes[i - 1].y);
            return;
        }
    }
}

fn main() {
    part1();
    part2();
}
