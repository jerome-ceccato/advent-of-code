use core::panic;
use std::{
    collections::{HashMap, HashSet},
    fs, ops,
};

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
    const DIAMOND: &'static [Point] = &[
        Point { x: 0, y: -2 },
        Point { x: -1, y: -1 },
        Point { x: 1, y: -1 },
        Point { x: 2, y: 0 },
        Point { x: -2, y: 0 },
        Point { x: 0, y: 2 },
        Point { x: -1, y: 1 },
        Point { x: 1, y: 1 },
    ];
}

impl ops::Add for Point {
    type Output = Point;
    fn add(self, rhs: Point) -> Self::Output {
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
        .map(|l| l.chars().collect())
        .collect()
}

fn find_in_world(world: &Vec<Vec<char>>, target: char) -> Point {
    for (y, row) in world.iter().enumerate() {
        for (x, &cell) in row.iter().enumerate() {
            if cell == target {
                return Point {
                    x: x as i32,
                    y: y as i32,
                };
            }
        }
    }
    panic!();
}

fn can_travel(world: &Vec<Vec<char>>, point: Point) -> bool {
    if point.y >= 0
        && point.y < world.len() as i32
        && point.x >= 0
        && point.x < world[point.y as usize].len() as i32
    {
        return world[point.y as usize][point.x as usize] != '#';
    }
    return false;
}

fn collect_distances(world: &Vec<Vec<char>>, end: Point) -> HashMap<Point, i32> {
    let mut distance = 0;
    let mut current = end;
    let mut result: HashMap<Point, i32> = HashMap::new();

    loop {
        result.insert(current, distance);
        if world[current.y as usize][current.x as usize] == 'S' {
            return result;
        }
        for dir in Point::DIRECTIONS {
            let other = current + *dir;
            if can_travel(world, other) && !result.contains_key(&other) {
                current = other;
                break;
            }
        }
        distance += 1;
    }
}

#[derive(Debug)]
struct Cheat {
    origin: Point,
    end: Point,
    score: i32,
}

fn useful_cheats<'a>(
    distances: &'a HashMap<Point, i32>,
    point: &'a Point,
    distance: &'a i32,
) -> impl Iterator<Item = (Point, i32)> + 'a {
    Point::DIAMOND.iter().filter_map(|dir| {
        let end = *point + *dir;
        if let Some(&other_value) = distances.get(&end) {
            if other_value < (*distance - 2) {
                return Some((end, (*distance - 2) - other_value));
            }
        }
        None
    })
}

fn all_cheating_options(distances: &HashMap<Point, i32>) -> Vec<Cheat> {
    distances
        .iter()
        .flat_map(|(point, distance)| {
            useful_cheats(distances, point, distance).map(|(end, score)| Cheat {
                origin: *point,
                end,
                score,
            })
        })
        .collect()
}

fn main() {
    let world = get_input("input");
    let end_pos = find_in_world(&world, 'E');
    let distances = collect_distances(&world, end_pos);
    let all_cheats = all_cheating_options(&distances);
    let part1 = all_cheats.iter().filter(|c| c.score >= 100).count();

    println!("Total cheats: {:?}", all_cheats.len());
    println!("{:?}", part1);
}
