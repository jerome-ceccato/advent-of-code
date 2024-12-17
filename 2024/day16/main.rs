use std::{
    cmp::Reverse,
    collections::{HashMap, HashSet},
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

impl ops::Sub for &Point {
    type Output = Point;
    fn sub(self, rhs: &Point) -> Self::Output {
        Point {
            x: self.x - rhs.x,
            y: self.y - rhs.y,
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

fn get_possible_paths(
    best_scores: &HashMap<Point, (i32, Vec<Point>)>,
    current: &Point,
) -> HashSet<Vec<Point>> {
    let mut all_paths: HashSet<Vec<Point>> = HashSet::new();
    if best_scores.contains_key(current) {
        for parent in best_scores[current].1.iter() {
            let paths = get_possible_paths(best_scores, &parent);
            for p in paths.iter() {
                let mut next = p.clone();
                next.push(current.clone());
                all_paths.insert(next);
            }
        }
        return all_paths;
    } else {
        let mut empty = HashSet::new();
        empty.insert(vec![]);
        return empty;
    }
}

fn get_path_score(start: &Point, start_dir: &Point, path: Vec<Point>) -> i32 {
    let mut dir = start_dir.clone();
    let mut current = start.clone();
    let mut score = 0;
    for p in path {
        if p == current {
            continue;
        }

        let this_dir = &p - &current;
        if this_dir == dir {
            score += 1;
        } else {
            score += 1001;
        }
        dir = this_dir;
        current = p;
    }
    return score;
}

fn pathfind(world: &Vec<Vec<char>>, start: &Point, start_dir: &Point) -> (i32, HashSet<Point>) {
    let mut queue = PriorityQueue::new();
    queue.push(
        Path {
            head: start.clone(),
            direction: start_dir.clone(),
            score: 0,
        },
        Reverse(0),
    );

    let mut goal: Option<i32> = None;
    let mut end = Point { x: -1, y: -1 };
    let mut best_scores: HashMap<Point, (i32, Vec<Point>)> = HashMap::new();

    // Run a priority queue to find the best score possible
    // For P2, this also keeps track of all the best score to reach a given node and the parents that achieved that score
    // Because of turning costing 1000 points, it's possible to have a second, equally as good path that has a score of exactly 1000 more than the best (because it's already in the correct orientation to proceed)
    // Unfortunately it also leads to some false positive, so we then build all the possible paths (in reverse, starting from the end and taking all parents that got the lowest score)
    // We then filter the paths that don't actually achieve the best score
    // Finally, we collect all unique points from those paths and that's our p2 result
    // Naturally, this is a horrible solution and it takes forever to get the result.
    while let Some((item, _)) = queue.pop() {
        if let Some(best) = goal {
            if item.score > best {
                continue;
            }
        }

        for direction in Point::DIRECTIONS.iter() {
            if direction != &-&item.direction {
                let next = &item.head + direction;
                if let Some(neighbor) = get(world, &next) {
                    if neighbor == '.' || neighbor == 'E' {
                        let score = next_score(item.score, &item.direction, direction);
                        if best_scores.contains_key(&next) {
                            if score < best_scores[&next].0 {
                                best_scores.insert(next, (score, vec![item.head.clone()]));
                            } else if score == best_scores[&next].0
                                || score == best_scores[&next].0 + 1000
                            {
                                best_scores
                                    .get_mut(&next)
                                    .unwrap()
                                    .1
                                    .push(item.head.clone());
                            }
                        } else {
                            best_scores.insert(next, (score, vec![item.head.clone()]));
                        }

                        if neighbor == '.' {
                            queue.push(
                                Path {
                                    head: next,
                                    direction: direction.clone(),
                                    score,
                                },
                                Reverse(score),
                            );
                        } else {
                            goal = Some(score);
                            end = next.clone();
                        }
                    }
                }
            }
        }
    }

    let all_paths = get_possible_paths(&best_scores, &end);
    let valid: Vec<&Vec<Point>> = all_paths
        .iter()
        .filter(|path| get_path_score(start, start_dir, path.to_vec()) == goal.unwrap())
        .collect();
    let mut visited: HashSet<Point> = HashSet::new();
    visited.insert(start.clone());
    for path in valid.into_iter() {
        for p in path.into_iter() {
            visited.insert(p.clone());
        }
    }

    return (goal.unwrap(), visited);
}

fn main() {
    let world = get_input("input");
    let start = find_start(&world);
    let direction = Point::EAST;

    let results = pathfind(&world, &start, &direction);
    println!("{}", results.0);
    println!("{}", results.1.len());
}
