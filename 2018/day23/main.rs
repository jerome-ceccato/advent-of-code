use std::{
    cmp::{max, min},
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

    fn distance_to_origin(self) -> i64 {
        self.distance_to(Point::ZERO)
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
    fn detects_p(&self, other: Point) -> bool {
        self.pos.distance_to(other) <= self.radius
    }

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

/*algorithm BronKerbosch1(R, P, X) is
    if P and X are both empty then
        report R as a maximal clique
    for each vertex v in P do
        BronKerbosch1(R ⋃ {v}, P ⋂ N(v), X ⋂ N(v))
        P := P \ {v}
        X := X ⋃ {v}

fn bron_kerbosch(r: HashSet<Nanobot>, mut p: HashSet<Nanobot>, mut x: HashSet<Nanobot>, neighbors: &HashMap<Nanobot, HashSet<Nanobot>>, output: &mut Vec<HashSet<Nanobot>>) {
    if p.is_empty() && x.is_empty() {
        println!("{:?}", r);
        output.push(r);
        println!("{:?}", output.len());
        return;
    }

    let seq: Vec<Nanobot> = p.iter().cloned().collect();
    for v in seq {
        let mut next_r = r.clone();
        next_r.insert(v.clone());

        let next_p: HashSet<Nanobot> = p.intersection(&neighbors[&v]).cloned().collect();
        let next_x: HashSet<Nanobot> = x.intersection(&neighbors[&v]).cloned().collect();

        bron_kerbosch(next_r, next_p, next_x, neighbors, output);
        p.remove(&v);
        x.insert(v.clone());
    }
}

        */

fn bron_kerbosch2(
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

        if let Some(result) = bron_kerbosch2(next_r, next_p, next_x, neighbors) {
            return Some(result);
        }
        p.remove(&v);
        x.insert(v.clone());
    }
    None
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct BBox {
    min: Point,
    max: Point,
}

impl BBox {
    fn intersects(&self, bot: &Nanobot) -> bool {
        let closest = Point {
            x: max(self.min.x, min(bot.pos.x, self.max.x)),
            y: max(self.min.y, min(bot.pos.y, self.max.y)),
            z: max(self.min.z, min(bot.pos.z, self.max.z)),
        };

        return bot.pos.distance_to(closest) <= bot.radius;

        // let dist = (bot.pos.x - closest.x).pow(2)
        //     + (bot.pos.y - closest.y).pow(2)
        //     + (bot.pos.z - closest.z).pow(2);
        // return dist <= bot.radius.pow(2);
    }

    fn closest_to_origin(&self) -> Point {
        Point {
            x: max(self.min.x, min(0, self.max.x)),
            y: max(self.min.y, min(0, self.max.y)),
            z: max(self.min.z, min(0, self.max.z)),
        }
    }

    fn furthest_from_origin(&self) -> Point {
        let corners = vec![
            Point {
                x: self.min.x,
                y: self.min.y,
                z: self.min.z,
            },
            Point {
                x: self.min.x,
                y: self.min.y,
                z: self.max.z,
            },
            Point {
                x: self.min.x,
                y: self.max.y,
                z: self.min.z,
            },
            Point {
                x: self.min.x,
                y: self.max.y,
                z: self.max.z,
            },
            Point {
                x: self.max.x,
                y: self.min.y,
                z: self.min.z,
            },
            Point {
                x: self.max.x,
                y: self.min.y,
                z: self.max.z,
            },
            Point {
                x: self.max.x,
                y: self.max.y,
                z: self.min.z,
            },
            Point {
                x: self.max.x,
                y: self.max.y,
                z: self.max.z,
            },
        ];

        corners
            .into_iter()
            .max_by_key(|p| p.x.pow(2) + p.y.pow(2) + p.z.pow(2))
            .unwrap()
    }

    fn split(&self) -> Vec<BBox> {
        let mid_point = Point {
            x: (self.min.x + self.max.x) / 2,
            y: (self.min.y + self.max.y) / 2,
            z: (self.min.z + self.max.z) / 2,
        };

        vec![
            BBox {
                min: self.min,
                max: mid_point,
            }, // Bottom-front-left
            BBox {
                min: Point {
                    x: mid_point.x,
                    y: self.min.y,
                    z: self.min.z,
                },
                max: Point {
                    x: self.max.x,
                    y: mid_point.y,
                    z: mid_point.z,
                },
            }, // Bottom-front-right
            BBox {
                min: Point {
                    x: self.min.x,
                    y: mid_point.y,
                    z: self.min.z,
                },
                max: Point {
                    x: mid_point.x,
                    y: self.max.y,
                    z: mid_point.z,
                },
            }, // Bottom-back-left
            BBox {
                min: Point {
                    x: mid_point.x,
                    y: mid_point.y,
                    z: self.min.z,
                },
                max: Point {
                    x: self.max.x,
                    y: self.max.y,
                    z: mid_point.z,
                },
            }, // Bottom-back-right
            BBox {
                min: Point {
                    x: self.min.x,
                    y: self.min.y,
                    z: mid_point.z,
                },
                max: Point {
                    x: mid_point.x,
                    y: mid_point.y,
                    z: self.max.z,
                },
            }, // Top-front-left
            BBox {
                min: Point {
                    x: mid_point.x,
                    y: self.min.y,
                    z: mid_point.z,
                },
                max: Point {
                    x: self.max.x,
                    y: mid_point.y,
                    z: self.max.z,
                },
            }, // Top-front-right
            BBox {
                min: Point {
                    x: self.min.x,
                    y: mid_point.y,
                    z: mid_point.z,
                },
                max: Point {
                    x: mid_point.x,
                    y: self.max.y,
                    z: self.max.z,
                },
            }, // Top-back-left
            BBox {
                min: mid_point,
                max: self.max,
            }, // Top-back-right
        ]
    }

    fn complexity(&self) -> i64 {
        self.max.x - self.min.x + self.max.y - self.min.y + self.max.z - self.min.z
    }
}

fn find_best_point(bots: &Vec<Nanobot>) {
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
    let clique = bron_kerbosch2(HashSet::new(), bot_set, HashSet::new(), &neighbors).unwrap();
    zoom(&clique);
}

fn zoom(clique: &HashSet<Nanobot>) {
    // get the bounding box of the spheres
    // divide into 2^3, find cubes closest to 0 with all overlaps
    // keep going until a point is found
    let mut queue = vec![get_bbox(clique)];
    let mut small_enough: HashSet<BBox> = HashSet::new();

    let mut best_furthest: i64 = get_bbox(clique).furthest_from_origin().distance_to_origin();
    while let Some(bbox) = queue.pop() {
        println!("sz: {}", queue.len());
        if bbox.complexity() <= 1000 {
            println!("found {:?}", bbox);
            small_enough.insert(bbox);
        } else {
            // Split into 8 sub cubes, prunes the ones that can't have the result
            let valid_cubes: Vec<BBox> = bbox
                .split()
                .into_iter()
                .filter(|bbox| clique.iter().all(|bot| bbox.intersects(bot)))
                .collect();

            let mut all_cubes = valid_cubes;
            all_cubes.append(&mut queue);

            // For each considered cube, get the furthest point from the origin, and find the best distance
            let local_furthest = all_cubes
                .iter()
                .map(|bbox| bbox.furthest_from_origin().distance_to_origin())
                .min()
                .unwrap();

            best_furthest = min(best_furthest, local_furthest);

            // If the closest point in the cube is further than the best furthest distance,
            // It means that cube cannot possibly contain a better point than the others, so purge it
            let filtered = all_cubes
                .into_iter()
                .filter(|bbox| bbox.closest_to_origin().distance_to_origin() <= best_furthest)
                .collect::<Vec<BBox>>();

            queue = filtered;
        }
    }
    println!("{:?}", small_enough.len());

    let best_points = small_enough
        .into_iter()
        .map(|bbox| bbox_get_best_valid(&bbox, clique))
        .collect::<Vec<(i64, Point)>>();

    println!("{:?}", best_points);
    let best_intersect = best_points.iter().max_by_key(|t| t.0).unwrap().0;
    let only_points = best_points
        .into_iter()
        .filter(|p| p.0 == best_intersect)
        .map(|p| p.1)
        .collect::<Vec<Point>>();

    let final_best = only_points
        .into_iter()
        .min_by_key(|p| p.distance_to_origin())
        .unwrap();

    println!("{:?}", final_best);
    println!("{:?}", final_best.distance_to_origin());

    // let target = best_points
    //     .into_iter()
    //     .min_by_key(|p| p.distance_to_origin())
    //     .unwrap();
    // println!("{:?}", target);
    // println!("{:?}", target.distance_to_origin());

    // let target = small_enough.iter().next().unwrap();

    // let mut best = target.min;
    // for x in target.min.x..=target.max.x {
    //     for y in target.min.y..=target.max.y {
    //         for z in target.min.z..=target.max.z {
    //             let best_dist = Point::ZERO.distance_to(best);
    //             let this_dist = Point::ZERO.distance_to(Point { x, y, z });

    //             if this_dist < best_dist {
    //                 best = Point { x, y, z };
    //             }
    //         }
    //     }
    // }

    // println!("{:?}", best);
    // println!("{:?}", target.closest_to_origin());
    // println!("{:?}", Point::ZERO.distance_to(best));
}

fn bbox_get_best_valid(target: &BBox, clique: &HashSet<Nanobot>) -> (i64, Point) {
    let mut best: Option<(i64, Point)> = None;
    println!(
        "{}",
        clique.iter().filter(|bot| target.intersects(bot)).count()
    );
    for x in target.min.x..=target.max.x {
        for y in target.min.y..=target.max.y {
            for z in target.min.z..=target.max.z {
                let point = Point { x, y, z };
                let number_of_intersections =
                    clique.iter().filter(|bot| bot.detects_p(point)).count() as i64;
                if let Some(best_v) = best {
                    if number_of_intersections < best_v.0 {
                        best = Some((number_of_intersections, point));
                    } else if number_of_intersections == best_v.0 {
                        let best_dist = Point::ZERO.distance_to(best_v.1);
                        let this_dist = Point::ZERO.distance_to(point);

                        if this_dist < best_dist {
                            best = Some((number_of_intersections, point));
                        }
                    }
                } else {
                    best = Some((number_of_intersections, point));
                }
            }
        }
    }
    best.unwrap()
}

// 60474030
// 60474071

// 60474082 too high
// 51825212 too low
// 51825216 too low
// 51811834

fn get_bbox(clique: &HashSet<Nanobot>) -> BBox {
    let first = clique.iter().next().unwrap();
    let mut bbox = BBox {
        min: first.pos,
        max: first.pos,
    };
    for bot in clique {
        bbox.min.x = min(bbox.min.x, bot.pos.x - bot.radius);
        bbox.min.y = min(bbox.min.y, bot.pos.y - bot.radius);
        bbox.min.z = min(bbox.min.z, bot.pos.z - bot.radius);

        bbox.max.x = max(bbox.max.x, bot.pos.x + bot.radius);
        bbox.max.y = max(bbox.max.y, bot.pos.y + bot.radius);
        bbox.max.z = max(bbox.max.z, bot.pos.z + bot.radius);
    }
    bbox
}

fn main() {
    let bots = get_input("input");
    let strongest = get_strongest(&bots);
    let part1 = count_in_range(&bots, strongest);

    // println!("{:?}", part1);

    find_best_point(&bots);
}
