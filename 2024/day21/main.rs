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

        let mut target = from;
        let mut over_gap = false;
        while target.x != to.x {
            if target == gap {
                over_gap = true;
                break;
            }
            target += dir;
            current_path.push(encode_dir(dir));
        }
        if !over_gap {
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
        let mut target = from;
        let mut over_gap = false;
        while target.y != to.y {
            if target == gap {
                over_gap = true;
                break;
            }
            target += dir;
            current_path.push(encode_dir(dir));
        }
        if !over_gap {
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

type Sequence = Vec<char>;
fn nkeypad_all_sequences(target: char, current: char) -> Vec<Sequence> {
    let mut paths = all_paths(
        numeric_keypad(current),
        numeric_keypad(target),
        numeric_keypad(' '),
        vec![],
    );
    for p in paths.iter_mut() {
        p.push('A');
    }
    paths
}

fn all_expanded_seq(
    sequence: &[char],
    current: char,
    move_memo: &mut HashMap<(char, char), Vec<Vec<char>>>,
) -> Vec<Vec<Sequence>> {
    if sequence.is_empty() {
        return vec![vec![]];
    }

    let next_c = sequence[0];
    let mut paths = all_expanded_seq(&sequence[1..], next_c, move_memo);

    if current == next_c {
        for p in paths.iter_mut() {
            p.insert(0, vec!['A']);
        }
    } else {
        let mut branches = keypad_get_moves(move_memo, current, next_c, directional_keypad).clone();
        for b in branches.iter_mut() {
            b.push('A');
        }
        let mut combinations: Vec<Vec<Sequence>> = vec![];
        for branch in branches {
            for p in paths.iter() {
                let mut new_path = p.clone();
                new_path.insert(0, branch.clone());
                combinations.push(new_path);
            }
        }
        paths = combinations;
    }
    return paths;
}

fn min_size(
    memo: &mut HashMap<(Sequence, i32), i64>,
    move_memo: &mut HashMap<(char, char), Vec<Vec<char>>>,
    seq: Sequence,
    depth: i32,
) -> i64 {
    if depth == 0 {
        return seq.len() as i64;
    } else {
        let key = (seq.clone(), depth);
        if memo.contains_key(&key) {
            return memo[&key];
        }
        let next = all_expanded_seq(&seq, 'A', move_memo)
            .into_iter()
            .map(|choice| {
                choice
                    .into_iter()
                    .map(|subseq| min_size(memo, move_memo, subseq, depth - 1))
                    .sum()
            })
            .min()
            .unwrap();
        memo.insert(key, next);
        next
    }
}

fn solve(
    code: &Vec<char>,
    seq_memo: &mut HashMap<(Sequence, i32), i64>,
    d_memo: &mut HashMap<(char, char), Vec<Vec<char>>>,
    depth: i32,
) -> i64 {
    let mut current = 'A';
    let mut total = 0;
    for &c in code {
        let sequences = nkeypad_all_sequences(c, current);
        let best = sequences
            .into_iter()
            .map(|s| min_size(seq_memo, d_memo, s, depth))
            .min()
            .unwrap();
        total += best;
        current = c;
    }
    total
}

fn code_complexity(
    code: &Vec<char>,
    seq_memo: &mut HashMap<(Sequence, i32), i64>,
    d_memo: &mut HashMap<(char, char), Vec<Vec<char>>>,
    depth: i32,
) -> i64 {
    let min_len = solve(code, seq_memo, d_memo, depth);
    let code_number: i64 = code.iter().take(3).collect::<String>().parse().unwrap();

    code_number * min_len
}

fn main() {
    let codes = get_input("input");
    let mut seq_memo: HashMap<(Sequence, i32), i64> = HashMap::new();
    let mut d_memo: HashMap<(char, char), Vec<Vec<char>>> = HashMap::new();

    let part1: i64 = codes
        .iter()
        .map(|code| code_complexity(code, &mut seq_memo, &mut d_memo, 2))
        .sum();
    println!("{:?}", part1);

    let part2: i64 = codes
        .iter()
        .map(|code| code_complexity(code, &mut seq_memo, &mut d_memo, 25))
        .sum();
    println!("{:?}", part2);
}
