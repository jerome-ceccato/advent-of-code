use std::{cmp::{min, Ordering}, collections::{BinaryHeap, HashMap}, fs, ops};

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct Point {
    x: i64,
    y: i64,
}

impl Point {
    const ZERO: Point = Point { x: 0, y: 0 };
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

struct World {
    depth: i64,
    geo_index_memo: HashMap<Point, i64>,
    erosion_memo: HashMap<Point, i64>,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
enum RegionType {
    Rocky,
    Wet,
    Narrow,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
enum Tool {
    Neither,
    Torch,
    Gear,
}

impl World {
    fn get_geo_index(&mut self, position: Point) -> i64 {
        if let Some(&index) = self.geo_index_memo.get(&position) {
            return index
        }
    
        let index = 
            if position.y == 0 {
                position.x * 16807
            } else if position.x == 0 {
                position.y * 48271
            } else {
                let left = self.get_erosion_level(position + Point::WEST);
                let up = self.get_erosion_level(position + Point::NORTH);
                left * up
            };
        self.geo_index_memo.insert(position, index);
        index
    }

    fn get_erosion_level(&mut self, position: Point) -> i64 {
        if let Some(&level) = self.erosion_memo.get(&position) {
            return level
        }
        let geo_index = self.get_geo_index(position);
        let level = (geo_index + self.depth) % 20183;
        self.erosion_memo.insert(position, level);
        level
    }

    fn get_region_type_int(&mut self, position: Point) -> i64 {
        self.get_erosion_level(position) % 3
    }

    fn get_region_type(&mut self, position: Point) -> RegionType {
        match self.get_region_type_int(position) {
            0 => RegionType::Rocky,
            1 => RegionType::Wet,
            2 => RegionType::Narrow,
            _ => unreachable!()
        }
    }

    fn get_risk_level(&mut self, target: Point) -> i64 {
        (0..=target.y)
            .flat_map(|y| (0..=target.x).map(move |x| Point { x, y }))
            .map(|p| self.get_region_type_int(p))
            .sum()
    }
}

fn get_input(path: &str) -> (i64, Point) {
    let contents = fs::read_to_string(path).unwrap();
    let (depth_raw, target_raw) = contents.split_once("\n").unwrap();
    let depth = depth_raw.split_once(": ").unwrap().1.parse::<i64>().unwrap();
    let target_str = target_raw.split_once(": ").unwrap().1.split_once(",").unwrap();
    
    (depth, Point { x: target_str.0.parse().unwrap(), y: target_str.1.parse().unwrap()})
}


#[derive(Debug, PartialEq, Eq, Hash)]
struct PathItem {
    position: Point,
    tool: Tool,
    cost: i64,
}

impl Ord for PathItem {
    fn cmp(&self, other: &Self) -> Ordering {
        other.cost.cmp(&self.cost)
    }
}

impl PartialOrd for PathItem {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

fn other_tool(tool: Tool, region: RegionType) -> Option<Tool> {
    match (tool, region) {
        (Tool::Torch, RegionType::Rocky) => Some(Tool::Gear),
        (Tool::Gear, RegionType::Rocky) => Some(Tool::Torch),

        (Tool::Gear, RegionType::Wet) => Some(Tool::Neither),
        (Tool::Neither, RegionType::Wet) => Some(Tool::Gear),

        (Tool::Torch, RegionType::Narrow) => Some(Tool::Neither),
        (Tool::Neither, RegionType::Narrow) => Some(Tool::Torch),

        _ => None
    }
}

fn can_move_at(position: Point, tool: Tool, world: &mut World) -> bool {
    if position.x < 0 || position.y < 0 {
        return false
    }
    // If there is no match, it means we're trying to use a tool that's not supported in that region type
    other_tool(tool, world.get_region_type(position)).is_some()
}

fn find_best_path(world: &mut World, target: Point) -> Option<i64> {
    let mut best_memo: HashMap<(Point, Tool), i64> = HashMap::new();
    let mut pq = BinaryHeap::new();
    pq.push(PathItem { position: Point::ZERO, tool: Tool::Torch, cost: 0 });

    let mut current_best: Option<i64> = None;
    while let Some(item) = pq.pop() {
        let memo_key = (item.position, item.tool);
        if let Some(&best) = best_memo.get(&memo_key) {
            if item.cost >= best {
                continue;
            }
        }
        best_memo.insert(memo_key, item.cost);

        // Stop if we've exeeded the best attempt
        if let Some(value) = current_best {
            if item.cost > value {
                continue;
            }
        }

        // Store the best value when reaching the target
        // Because we can discover faster paths using different tools, we need to keep exploring
        // the queue to determine the best value
        if item.position == target && item.tool == Tool::Torch {
            current_best = Some(min(item.cost, current_best.unwrap_or(item.cost)));
            continue;
        }

        // Try to swap tools
        pq.push(PathItem { 
            position: item.position, 
            tool: other_tool(item.tool, world.get_region_type(item.position)).unwrap(),
            cost: item.cost + 7
        });

        // Try to move if possible
        for dir in Point::DIRECTIONS {
            let next = item.position + *dir;
            if can_move_at(next, item.tool, world) {
                pq.push(PathItem { 
                    position: next, 
                    tool: item.tool,
                    cost: item.cost + 1 
                });
            }
        }
    }
    current_best
}

fn main() {
    let (depth, target) = get_input("input");
    let mut world = World {
        depth,
        geo_index_memo: HashMap::new(),
        erosion_memo: HashMap::new()
    };

    world.geo_index_memo.insert(Point::ZERO, 0);
    world.geo_index_memo.insert(target, 0);

    let part1 = world.get_risk_level(target);
    println!("{:?}", part1);

    let part2 = find_best_path(&mut world, target);
    println!("{:?}", part2);
}
