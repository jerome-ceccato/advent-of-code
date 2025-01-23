use std::{collections::HashSet, fs, ops};

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct Point {
    x: i32,
    y: i32,
    z: i32,
    t: i32,
}

impl Point {
    fn distance_to(self, other: Point) -> i32 {
        (self.x - other.x).abs()
            + (self.y - other.y).abs()
            + (self.z - other.z).abs()
            + (self.t - other.t).abs()
    }
}

impl ops::Add for Point {
    type Output = Point;
    fn add(self, rhs: Point) -> Self::Output {
        Point {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
            z: self.z + rhs.z,
            t: self.t + rhs.t,
        }
    }
}

fn get_input(path: &str) -> Vec<Point> {
    fs::read_to_string(path)
        .unwrap()
        .split("\n")
        .map(|l| {
            let values = l
                .trim()
                .split(",")
                .map(|s| s.parse::<i32>().unwrap())
                .collect::<Vec<i32>>();
            Point {
                x: values[0],
                y: values[1],
                z: values[2],
                t: values[3],
            }
        })
        .collect()
}

fn can_join(constellation: &HashSet<Point>, other: Point) -> bool {
    for p in constellation {
        if p.distance_to(other) <= 3 {
            return true;
        }
    }
    false
}

fn form_constellations(points: &Vec<Point>) -> i32 {
    let mut remaining: HashSet<Point> = points.iter().cloned().collect();
    let mut constellations = 0;
    let mut current: HashSet<Point> = HashSet::new();

    while !remaining.is_empty() {
        if current.is_empty() {
            let first = *remaining.iter().next().unwrap();
            current.insert(first);
            remaining.remove(&first);
        }

        let mut modified = false;
        for &item in remaining.iter() {
            if can_join(&current, item) {
                current.insert(item);
                remaining.remove(&item);
                modified = true;
                break;
            }
        }
        if !modified {
            constellations += 1;
            current = HashSet::new();
        }
    }
    constellations + if current.is_empty() { 0 } else { 1 }
}

fn main() {
    let points = get_input("input");

    println!("{:?}", form_constellations(&points));
}
