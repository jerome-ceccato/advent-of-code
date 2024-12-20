use core::panic;
use std::{collections::HashMap, fs, ops};

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

fn make_diamond(size: i32) -> Vec<(Point, i32)> {
    let mut points: Vec<(Point, i32)> = vec![];
    for x in -size..=size {
        for y in -size..=size {
            let distance = x.abs() + y.abs();
            if distance <= size {
                points.push((Point { x, y }, distance));
            }
        }
    }
    points
}

fn useful_cheats<'a>(
    distances: &'a HashMap<Point, i32>,
    point: &'a Point,
    distance: &'a i32,
    diamond_offset: &'a Vec<(Point, i32)>,
) -> impl Iterator<Item = i32> + 'a {
    diamond_offset.iter().filter_map(|(dir, offset)| {
        let end = *point + *dir;
        if let Some(&other_value) = distances.get(&end) {
            if other_value < (*distance - offset) {
                return Some((*distance - offset) - other_value);
            }
        }
        None
    })
}

fn all_cheating_scores(
    distances: &HashMap<Point, i32>,
    cheat_max_len: i32,
    threshold: i32,
) -> Vec<i32> {
    let offset_map = make_diamond(cheat_max_len);
    distances
        .iter()
        .flat_map(|(point, distance)| useful_cheats(distances, point, distance, &offset_map))
        .filter(|&c| c >= threshold)
        .collect()
}

fn main() {
    let world = get_input("input");
    let end_pos = find_in_world(&world, 'E');
    let distances = collect_distances(&world, end_pos);

    let part1 = all_cheating_scores(&distances, 2, 100).into_iter().count();
    println!("{:?}", part1);

    let part2 = all_cheating_scores(&distances, 20, 100).into_iter().count();
    println!("{:?}", part2);
}
