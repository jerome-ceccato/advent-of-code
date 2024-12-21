use std::{collections::HashMap, fs, ops, vec};

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, PartialOrd, Ord)]
struct Point {
    x: i32,
    y: i32,
}

impl Point {
    const UP: Point = Point { x: 0, y: -1 };
    const RIGHT: Point = Point { x: 1, y: 0 };
    const DOWN: Point = Point { x: 0, y: 1 };
    const LEFT: Point = Point { x: -1, y: 0 };
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

impl ops::AddAssign for Point {
    fn add_assign(&mut self, other: Self) {
        self.x += other.x;
        self.y += other.y;
    }
}

fn get_input(path: &str) -> Vec<Vec<char>> {
    fs::read_to_string(path)
        .unwrap()
        .split("\n")
        .map(|l| l.chars().collect())
        .collect()
}

fn numeric_keypad(item: char) -> Point {
    match item {
        '7' => Point { x: 0, y: 0 },
        '8' => Point { x: 1, y: 0 },
        '9' => Point { x: 2, y: 0 },
        '4' => Point { x: 0, y: 1 },
        '5' => Point { x: 1, y: 1 },
        '6' => Point { x: 2, y: 1 },
        '1' => Point { x: 0, y: 2 },
        '2' => Point { x: 1, y: 2 },
        '3' => Point { x: 2, y: 2 },
        ' ' => Point { x: 0, y: 3 },
        '0' => Point { x: 1, y: 3 },
        'A' => Point { x: 2, y: 3 },
        _ => unreachable!(),
    }
}

fn directional_keypad(item: char) -> Point {
    match item {
        ' ' => Point { x: 0, y: 0 },
        '^' => Point { x: 1, y: 0 },
        'A' => Point { x: 2, y: 0 },
        '<' => Point { x: 0, y: 1 },
        'v' => Point { x: 1, y: 1 },
        '>' => Point { x: 2, y: 1 },
        _ => unreachable!(),
    }
}

fn encode_dir(dir: Point) -> char {
    match dir {
        Point::UP => '^',
        Point::RIGHT => '>',
        Point::DOWN => 'v',
        Point::LEFT => '<',
        _ => unreachable!(),
    }
}

fn all_paths(from: Point, to: Point, gap: Point, path: Vec<char>) -> Vec<Vec<char>> {
    if from == to {
        return vec![path];
    }

    let mut paths: Vec<Vec<char>> = vec![];
    if from.x != to.x {
        let mut current_path = path.clone();
        let dir = if from.x > to.x {
            Point::LEFT
        } else {
            Point::RIGHT
        };
        let target = from + dir;
        if target != gap {
            current_path.push(encode_dir(dir));
            paths.append(&mut all_paths(target, to, gap, current_path));
        }
    }
    if from.y != to.y {
        let mut current_path = path.clone();
        let dir = if from.y > to.y {
            Point::UP
        } else {
            Point::DOWN
        };
        let target = from + dir;
        if target != gap {
            current_path.push(encode_dir(dir));
            paths.append(&mut all_paths(target, to, gap, current_path));
        }
    }

    paths
}

fn keypad_get_moves<'a>(
    memo: &'a mut HashMap<(char, char), Vec<Vec<char>>>,
    from: char,
    to: char,
    position_encoder: fn(char) -> Point,
) -> &'a Vec<Vec<char>> {
    let key = (from, to);
    if !memo.contains_key(&key) {
        let from_pos = position_encoder(from);
        let to_pos = position_encoder(to);
        let results = all_paths(from_pos, to_pos, position_encoder(' '), vec![]);
        memo.insert((from, to), results);
    }

    &memo[&key]
}

fn encode_message_rec(
    mut paths: Vec<Vec<char>>,
    message: &[char],
    current: char,
    memo: &mut HashMap<(char, char), Vec<Vec<char>>>,
    position_encoder: fn(char) -> Point,
) -> Vec<Vec<char>> {
    if message.is_empty() {
        return paths;
    }

    let next_c = message[0];
    if current != next_c {
        let branches = keypad_get_moves(memo, current, next_c, position_encoder);
        let mut combinations: Vec<Vec<char>> = vec![];
        for branch in branches {
            for p in paths.iter() {
                let mut new_path = p.clone();
                new_path.append(&mut branch.clone());
                combinations.push(new_path);
            }
        }
        paths = combinations;
    }

    for p in paths.iter_mut() {
        p.push('A');
    }

    encode_message_rec(paths, &message[1..], next_c, memo, position_encoder)
}

fn code_complexity(
    code: &Vec<char>,
    n_memo: &mut HashMap<(char, char), Vec<Vec<char>>>,
    d_memo: &mut HashMap<(char, char), Vec<Vec<char>>>,
) -> i32 {
    let first_pass = encode_message_rec(vec![vec![]], &code, 'A', n_memo, numeric_keypad);
    let second_pass: Vec<Vec<char>> = first_pass
        .iter()
        .flat_map(|p| encode_message_rec(vec![vec![]], &p, 'A', d_memo, directional_keypad))
        .collect();

    let third_pass: Vec<Vec<char>> = second_pass
        .iter()
        .flat_map(|p| encode_message_rec(vec![vec![]], &p, 'A', d_memo, directional_keypad))
        .collect();

    let min_len = third_pass.iter().map(|n| n.len()).min().unwrap();
    let code_number: i32 = code.iter().take(3).collect::<String>().parse().unwrap();

    println!(
        "{:?} -> {} * {}",
        code.iter().collect::<String>(),
        min_len,
        code_number
    );
    code_number * min_len as i32
}

fn main() {
    let codes = get_input("input");
    let mut nkeypad_memo: HashMap<(char, char), Vec<Vec<char>>> = HashMap::new();
    let mut dkeypad_memo: HashMap<(char, char), Vec<Vec<char>>> = HashMap::new();

    let result = codes
        .iter()
        .map(|code| code_complexity(code, &mut nkeypad_memo, &mut dkeypad_memo))
        .sum::<i32>();
    println!("{:?}", result);

    // let possibilities = encode_message_rec(
    //     vec![vec![]],
    //     &codes[0],
    //     'A',
    //     &mut nkeypad_memo,
    //     numeric_keypad,
    // );
    // let next: Vec<Vec<char>> = possibilities
    //     .iter()
    //     .flat_map(|p| {
    //         encode_message_rec(vec![vec![]], &p, 'A', &mut dkeypad_memo, directional_keypad)
    //     })
    //     .collect();

    // let next2: Vec<Vec<char>> = next
    //     .iter()
    //     .flat_map(|p| {
    //         encode_message_rec(vec![vec![]], &p, 'A', &mut dkeypad_memo, directional_keypad)
    //     })
    //     .collect();
    // println!(
    //     "{:?}",
    //     possibilities
    //         .iter()
    //         .map(|v| v.iter().collect::<String>())
    //         .collect::<Vec<String>>()
    // );

    // println!(
    //     "{:?}",
    //     next.iter()
    //         .map(|v| v.iter().collect::<String>())
    //         .collect::<Vec<String>>()
    // );

    // println!("{:?}", possibilities.len());
    // println!("{:?}", next.len());
    // println!("{:?}", next2.len());
    // println!("-> {:?}", next2.iter().map(|n| n.len()).min());

    // let from = numeric_keypad('0');
    // let to = numeric_keypad('1');
    // println!("{:?}", all_paths(from, to, numeric_keypad(' '), vec![]));
}

// 165374 too high
