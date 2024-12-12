use std::{collections::HashSet, fs};

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct Point {
    x: i32,
    y: i32,
}

fn get_input(path: &str) -> Vec<Vec<u8>> {
    return fs::read_to_string(path)
        .unwrap()
        .split("\n")
        .map(|line| line.as_bytes().to_vec())
        .collect();
}

fn get_plot(garden: &Vec<Vec<u8>>, point: &Point) -> Option<u8> {
    if point.y >= 0
        && point.y < garden.len() as i32
        && point.x >= 0
        && point.x < garden[point.y as usize].len() as i32
    {
        return Some(garden[point.y as usize][point.x as usize]);
    }
    return None;
}

fn get_valid_surrounding(garden: &Vec<Vec<u8>>, point: &Point) -> Vec<Point> {
    let directions = vec![
        Point { x: 0, y: -1 },
        Point { x: 0, y: 1 },
        Point { x: -1, y: 0 },
        Point { x: 1, y: 0 },
    ];

    return directions
        .iter()
        .map(|dir| Point {
            x: point.x + dir.x,
            y: point.y + dir.y,
        })
        .filter(|p| get_plot(garden, p).is_some())
        .collect();
}

fn find_region(
    garden: &Vec<Vec<u8>>,
    consumed: &HashSet<Point>,
    start: Point,
) -> Option<HashSet<Point>> {
    if consumed.contains(&start) {
        return None;
    }

    let mut region: HashSet<Point> = HashSet::new();
    let mut queue = vec![start];

    while let Some(current) = queue.pop() {
        if region.contains(&current) {
            continue;
        }

        region.insert(current);
        for other in get_valid_surrounding(garden, &current) {
            if get_plot(garden, &other) == get_plot(garden, &current) {
                queue.push(other);
            }
        }
    }

    return Some(region);
}

fn find_regions(garden: &Vec<Vec<u8>>) -> Vec<HashSet<Point>> {
    let mut regions: Vec<HashSet<Point>> = vec![];
    let mut consumed: HashSet<Point> = HashSet::new();

    for y in 0..garden.len() {
        for x in 0..garden[y].len() {
            if let Some(region) = find_region(
                garden,
                &consumed,
                Point {
                    x: x as i32,
                    y: y as i32,
                },
            ) {
                regions.push(region.clone());
                for p in region {
                    consumed.insert(p);
                }
            }
        }
    }

    return regions;
}

fn get_price(garden: &Vec<Vec<u8>>, region: &HashSet<Point>) -> i32 {
    let mut area: i32 = 0;
    let mut perimeter: i32 = 0;
    let plant = get_plot(garden, region.into_iter().next().unwrap()).unwrap();
    for point in region {
        let surrounding = get_valid_surrounding(garden, point);

        perimeter += 4 - surrounding.len() as i32;
        for other in surrounding {
            if get_plot(garden, &other) != Some(plant) {
                perimeter += 1;
            }
        }
        area += 1;
    }

    return area * perimeter;
}

fn main() {
    let garden = get_input("input");
    let regions = find_regions(&garden);
    println!("{:?}", regions);

    let all_prices: Vec<i32> = regions
        .into_iter()
        .map(|r| get_price(&garden, &r))
        .collect();
    println!("{:?}", all_prices);

    println!("{:?}", all_prices.into_iter().sum::<i32>());
}
