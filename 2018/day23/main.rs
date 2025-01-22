use std::{
    collections::{HashMap, HashSet},
    fs, ops,
};

use regex::Regex;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct Point {
    x: i64,
    y: i64,
    z: i64,
}

impl Point {
    const ZERO: Point = Point { x: 0, y: 0, z: 0 };

    fn distance_to(self, other: Point) -> i64 {
        (self.x - other.x).abs() + (self.y - other.y).abs() + (self.z - other.z).abs()
    }
}

impl ops::Add for Point {
    type Output = Point;
    fn add(self, rhs: Point) -> Self::Output {
        Point {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
            z: self.z + rhs.z,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Nanobot {
    pos: Point,
    radius: i64,
}

impl Nanobot {
    fn detects(&self, other: &Nanobot) -> bool {
        self.pos.distance_to(other.pos) <= self.radius
    }

    fn intersects(&self, other: &Nanobot) -> bool {
        self.pos.distance_to(other.pos) <= (self.radius + other.radius)
    }
}

fn get_input(path: &str) -> Vec<Nanobot> {
    let re = Regex::new(r"pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(-?\d+)").unwrap();
    fs::read_to_string(path)
        .unwrap()
        .split("\n")
        .map(|l| {
            let caps = re.captures(l).unwrap();
            Nanobot {
                pos: Point {
                    x: caps[1].parse().unwrap(),
                    y: caps[2].parse().unwrap(),
                    z: caps[3].parse().unwrap(),
                },
                radius: caps[4].parse().unwrap(),
            }
        })
        .collect()
}

fn get_strongest(bots: &Vec<Nanobot>) -> &Nanobot {
    bots.iter().max_by_key(|&b| b.radius).unwrap()
}

fn count_in_range(bots: &Vec<Nanobot>, target: &Nanobot) -> usize {
    bots.iter().filter(|&bot| target.detects(bot)).count()
}

// Treat bots like a graph:
// - Each bot is a node
// - Bots are linked if they intersect
// Find biggest clique in graph, then discard all other bots, then find the best point from there

fn bron_kerbosch(
    r: HashSet<Nanobot>,
    mut p: HashSet<Nanobot>,
    mut x: HashSet<Nanobot>,
    neighbors: &HashMap<Nanobot, HashSet<Nanobot>>,
) -> Option<HashSet<Nanobot>> {
    if p.is_empty() && x.is_empty() {
        return Some(r);
    }

    let seq: Vec<Nanobot> = p.iter().cloned().collect();
    for v in seq {
        let mut next_r = r.clone();
        next_r.insert(v.clone());

        let next_p: HashSet<Nanobot> = p.intersection(&neighbors[&v]).cloned().collect();
        let next_x: HashSet<Nanobot> = x.intersection(&neighbors[&v]).cloned().collect();

        if let Some(result) = bron_kerbosch(next_r, next_p, next_x, neighbors) {
            return Some(result);
        }
        p.remove(&v);
        x.insert(v.clone());
    }
    None
}

fn find_best_point(bots: &Vec<Nanobot>) -> i64 {
    let mut bot_set: HashSet<Nanobot> = HashSet::new();
    let mut neighbors: HashMap<Nanobot, HashSet<Nanobot>> = HashMap::new();

    for a in bots {
        bot_set.insert(*a);
        for b in bots {
            if a != b && a.intersects(b) {
                neighbors.entry(*a).or_default().insert(*b);
            }
        }
    }

    // Biggest group of all nanobots that intersect with all others
    let clique = bron_kerbosch(HashSet::new(), bot_set, HashSet::new(), &neighbors).unwrap();
    find_best_dist(&clique)
}

fn find_best_dist(clique: &HashSet<Nanobot>) -> i64 {
    let mut low: i64 = 0;
    let mut high: i64 = 1_000_000_000;

    // Since all nanobots are in range of each other, there must exist at least a point in range of all of them
    // We don't need the point, but only the distance to origin
    // We can add a new nanobot at origin and find its smallest range that detects all other nanobots
    // Simple binary search it

    while low + 1 < high {
        let mid = low + (high - low) / 2;
        let test_bot = Nanobot {
            pos: Point::ZERO,
            radius: mid,
        };
        if clique.iter().all(|other| test_bot.intersects(other)) {
            high = mid;
        } else {
            low = mid;
        }
    }

    low + 1
}

fn main() {
    let bots = get_input("input");
    let strongest = get_strongest(&bots);

    println!("{:?}", count_in_range(&bots, strongest));
    println!("{:?}", find_best_point(&bots));
}
