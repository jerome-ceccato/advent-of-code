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
        '0' => Point { x: 1, y: 3 },
        'A' => Point { x: 2, y: 3 },
        _ => unreachable!(),
    }
}

fn directional_keypad(item: char) -> Point {
    match item {
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

fn any_direction(from: Point, to: Point) -> Point {
    if from.x == to.x {
        if from.y > to.y {
            Point::UP
        } else {
            Point::DOWN
        }
    } else {
        if from.x > to.x {
            Point::LEFT
        } else {
            Point::RIGHT
        }
    }
}

fn get_moves(from: Point, to: Point, starting_dir: Point) -> Vec<char> {
    let mut moves: Vec<char> = vec![];
    let mut current = from;
    let encoded_dir = encode_dir(starting_dir);
    if starting_dir.x == 0 {
        while current.y != to.y {
            moves.push(encoded_dir);
            current += starting_dir;
        }
    } else {
        while current.x != to.x {
            moves.push(encoded_dir);
            current += starting_dir;
        }
    }

    if current != to {
        moves.append(&mut get_moves(current, to, any_direction(current, to)));
    }
    moves
}

fn nkeypad_get_moves<'a>(
    memo: &'a mut HashMap<(char, char), Vec<char>>,
    from: char,
    to: char,
) -> &'a Vec<char> {
    let key = (from, to);
    if !memo.contains_key(&key) {
        let from_pos = numeric_keypad(from);
        let to_pos = numeric_keypad(to);
        let result = match (from, to) {
            ('0', 'A') => vec!['>'],
            ('A', '0') => vec!['<'],
            ('0', _) | ('A', _) => get_moves(from_pos, to_pos, Point::UP),
            _ => get_moves(from_pos, to_pos, any_direction(from_pos, to_pos)),
        };
        memo.insert((from, to), result);
    }

    &memo[&key]
}

fn dkeypad_get_moves<'a>(
    memo: &'a mut HashMap<(char, char), Vec<char>>,
    from: char,
    to: char,
) -> &'a Vec<char> {
    let key = (from, to);
    if !memo.contains_key(&key) {
        let from_pos = directional_keypad(from);
        let to_pos = directional_keypad(to);
        let result = match (from, to) {
            ('^', 'A') => vec!['>'],
            ('A', '^') => vec!['<'],
            ('^', _) | ('A', _) => get_moves(from_pos, to_pos, Point::DOWN),
            _ => get_moves(from_pos, to_pos, any_direction(from_pos, to_pos)),
        };
        memo.insert((from, to), result);
    }

    &memo[&key]
}

fn encode_message(
    message: &Vec<char>,
    initial: char,
    memo: &mut HashMap<(char, char), Vec<char>>,
    move_encoder: fn(&mut HashMap<(char, char), Vec<char>>, char, char) -> &Vec<char>,
) -> Vec<char> {
    let mut current = initial;
    let mut result: Vec<char> = vec![];
    for &c in message {
        if c != current {
            move_encoder(memo, current, c)
                .iter()
                .for_each(|&item| result.push(item));
        }
        result.push('A');
        current = c;
    }
    result
}

fn code_complexity(
    code: &Vec<char>,
    n_memo: &mut HashMap<(char, char), Vec<char>>,
    d_memo: &mut HashMap<(char, char), Vec<char>>,
) -> i32 {
    let first_pass = encode_message(&code, 'A', n_memo, nkeypad_get_moves);
    let second_pass = encode_message(&first_pass, 'A', d_memo, dkeypad_get_moves);
    let third_pass = encode_message(&second_pass, 'A', d_memo, dkeypad_get_moves);
    let code_number: i32 = code.iter().take(3).collect::<String>().parse().unwrap();

    println!(
        "{:?} -> {} * {}",
        code.iter().collect::<String>(),
        third_pass.len(),
        code_number
    );
    code_number * third_pass.len() as i32
}

fn main() {
    let codes = get_input("input");
    let mut nkeypad_memo: HashMap<(char, char), Vec<char>> = HashMap::new();
    let mut dkeypad_memo: HashMap<(char, char), Vec<char>> = HashMap::new();

    let result = codes
        .iter()
        .map(|code| code_complexity(code, &mut nkeypad_memo, &mut dkeypad_memo))
        .sum::<i32>();
    println!("{:?}", result);
}

// 165374 too high
