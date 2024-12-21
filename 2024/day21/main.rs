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

// sequence is a list of directional inputs ending with A
fn find_best_encoding(
    sequence: Vec<char>,
    current: char,
    sequence_memo: &mut HashMap<Vec<char>, Vec<char>>,
    n_memo: &mut HashMap<(char, char), Vec<Vec<char>>>,
    d_memo: &mut HashMap<(char, char), Vec<Vec<char>>>,
    level: i32,
) -> Vec<char> {
    let max_depth = 25;
    if level > 0 && sequence_memo.contains_key(&sequence) {
        return sequence_memo[&sequence].clone();
    }

    let possibilities = if level == 0 {
        encode_sequence(&sequence, current, n_memo, numeric_keypad)
    } else {
        encode_sequence(&sequence, current, d_memo, directional_keypad)
    };
    let mut min_size = possibilities.iter().map(|p| p.len()).min().unwrap();
    let filtered: Vec<Vec<char>> = possibilities
        .into_iter()
        .filter(|p| p.len() == min_size)
        .collect();

    if filtered.len() == 1 {
        let item = filtered.into_iter().next().unwrap();
        // println!("only {:?} -> {:?}", sequence, item);
        if level > 0 {
            println!("new seq {}", sequence.iter().collect::<String>());
            sequence_memo.insert(sequence, item.clone());
        }
        return item;
    }

    let mut current: Vec<(Vec<char>, Vec<char>)> =
        filtered.into_iter().map(|n| (n.clone(), n)).collect();

    let mut depth = level + 1;
    while current.len() > 1 && depth <= max_depth {
        let deeper: Vec<(Vec<char>, Vec<char>)> = current
            .into_iter()
            .map(|(orig, f)| {
                (
                    orig.clone(),
                    encode_sequence(&f, 'A', d_memo, directional_keypad)
                        .into_iter()
                        .next()
                        .unwrap(),
                )
            })
            .collect();

        min_size = deeper.iter().map(|(_, p)| p.len()).min().unwrap();
        current = deeper
            .into_iter()
            .filter(|(_, p)| p.len() == min_size)
            .collect();
        depth += 1;
    }

    let (item, _) = current.into_iter().next().unwrap();
    // println!("best {:?} -> {:?}", sequence, item);
    if level > 0 {
        println!("new seq {}", sequence.iter().collect::<String>());
        sequence_memo.insert(sequence, item.clone());
    }

    return item;
}

fn encode_sequence(
    sequence: &[char],
    current: char,
    move_memo: &mut HashMap<(char, char), Vec<Vec<char>>>,
    position_encoder: fn(char) -> Point,
) -> Vec<Vec<char>> {
    if sequence.is_empty() {
        return vec![vec![]];
    }

    let next_c = sequence[0];
    let mut paths = encode_sequence(&sequence[1..], next_c, move_memo, position_encoder);

    if current == next_c {
        for p in paths.iter_mut() {
            p.insert(0, 'A');
        }
    } else {
        let mut branches = keypad_get_moves(move_memo, current, next_c, position_encoder).clone();
        for b in branches.iter_mut() {
            b.push('A');
        }
        let mut combinations: Vec<Vec<char>> = vec![];
        for branch in branches {
            for p in paths.iter() {
                let mut new_path = branch.clone();
                new_path.append(&mut p.clone());
                combinations.push(new_path);
            }
        }
        paths = combinations;
    }
    return paths;
}

fn split_seq(seq: &Vec<char>) -> Vec<Vec<char>> {
    let mut res: Vec<Vec<char>> = vec![];
    let mut current: Vec<char> = vec![];
    for &c in seq {
        current.push(c);
        if c == 'A' {
            res.push(current);
            current = vec![];
        }
    }
    res
}

fn split_message(message: &[char], level: i32) -> Vec<Vec<char>> {
    if level == 0 {
        message.chunks(1).map(|c| c.to_vec()).collect()
    } else {
        split_seq(&message.to_vec())
    }
}

fn encode_message2(
    message: &[char],
    level: i32,
    sequence_memo: &mut HashMap<Vec<char>, Vec<char>>,
    n_memo: &mut HashMap<(char, char), Vec<Vec<char>>>,
    d_memo: &mut HashMap<(char, char), Vec<Vec<char>>>,
) -> Vec<char> {
    if level == 0 {
        let mut current = 'A';
        let mut res: Vec<char> = vec![];
        for &c in message {
            res.append(&mut find_best_encoding(
                vec![c],
                current,
                sequence_memo,
                d_memo,
                n_memo,
                level,
            ));
            current = c;
        }
        res
    } else {
        split_seq(&message.to_vec())
            .into_iter()
            .flat_map(|chunk| find_best_encoding(chunk, 'A', sequence_memo, n_memo, d_memo, level))
            .collect()
    }
}

fn code_complexity(
    code: &Vec<char>,
    sequence_memo: &mut HashMap<Vec<char>, Vec<char>>,
    n_memo: &mut HashMap<(char, char), Vec<Vec<char>>>,
    d_memo: &mut HashMap<(char, char), Vec<Vec<char>>>,
    directional_passes: i32,
) -> i32 {
    let mut current = encode_message2(&code, 0, sequence_memo, n_memo, d_memo);

    for level in 0..directional_passes {
        use std::time::Instant;
        let now = Instant::now();
        println!("{} ({})", level, current.len());
        current = encode_message2(&current, level + 1, sequence_memo, n_memo, d_memo);
        let elapsed = now.elapsed();
        println!("Elapsed: {:.2?}", elapsed);
    }
    // println!("{:?}", current.iter().collect::<String>());
    // encode_message(&code, 'A', &mut message_memo, n_memo, numeric_keypad);
    // let mut current = encode_message_rec(vec![vec![]], &code, 'A', n_memo, numeric_keypad);
    // for _ in 0..directional_passes {
    //     current = current
    //         .into_iter()
    //         .flat_map(|p| encode_message(&p, 'A', &mut message_memo, d_memo, directional_keypad))
    //         .collect();
    // }

    let code_number: i32 = code.iter().take(3).collect::<String>().parse().unwrap();

    println!(
        "{:?} -> {} * {}",
        code.iter().collect::<String>(),
        current.len(),
        code_number
    );
    code_number * current.len() as i32
}

fn main() {
    let codes = get_input("input");
    let mut nkeypad_memo: HashMap<(char, char), Vec<Vec<char>>> = HashMap::new();
    let mut dkeypad_memo: HashMap<(char, char), Vec<Vec<char>>> = HashMap::new();
    let mut sequence_memo: HashMap<Vec<char>, Vec<char>> = HashMap::new();

    let result = codes
        .iter()
        .map(|code| {
            code_complexity(
                code,
                &mut sequence_memo,
                &mut nkeypad_memo,
                &mut dkeypad_memo,
                25,
            )
        })
        .sum::<i32>();
    println!("{:?}", result);

    // let test: Vec<char> = "AA<^A^^Av>AvvA".to_string().chars().collect();
    // let split_test: Vec<Vec<char>> = split_seq(&test);
    // println!("{:?}", split_test);
    // let toto = encode_message(
    //     &codes[0],
    //     'A',
    //     &mut message_memo,
    //     &mut nkeypad_memo,
    //     numeric_keypad,
    // );
    // println!(
    //     "{:?}",
    //     toto.iter()
    //         .map(|n| n.iter().collect::<String>())
    //         .collect::<Vec<String>>()
    // );
    // for path in toto {
    //     println!("--- {:?}:", path.iter().collect::<String>());
    //     let next = encode_message(
    //         &path,
    //         'A',
    //         &mut message_memo,
    //         &mut dkeypad_memo,
    //         directional_keypad,
    //     );
    //     for p in next {
    //         println!("- {:?}:", p.iter().collect::<String>());
    //         let next2 = encode_message(
    //             &p,
    //             'A',
    //             &mut message_memo,
    //             &mut dkeypad_memo,
    //             directional_keypad,
    //         );
    //         println!(
    //             "{}:",
    //             next2.first().unwrap().into_iter().collect::<String>()
    //         );
    //     }
    // }
    // let from = numeric_keypad('0');
    // let to = numeric_keypad('1');
    // println!("{:?}", all_paths(from, to, numeric_keypad(' '), vec![]));
}

/*
   look at all the ways to write 029A, why are some slower
   once we have the optimal way to write 029A, the rest should all be infinite recursive patterns, we don't need to list them just count, if their pos doesn't matter because they rest in A, can we pop count?
*/

/*
"286A" -> 68 * 286
"480A" -> 74 * 480
"140A" -> 70 * 140
"413A" -> 70 * 413
"964A" -> 72 * 964
163086
 */

/*
286A

--- "<^A^^A [v>A] vvA":
- "v<<A>^A>A<AA>A[<vA.>A.^A]<vAA>^A" better
-> <vA<AA>>^AvA<^A>AvA^Av<<A>>^AAvA^A| v<<A>A>^A.vA^A.<A>A |v<<A>A>^AAvA<^A>A

--- "<^A^^A [>vA] vvA"
- "v<<A>^A>A<AA>A [vA.<A.>^A] <vAA>^A" worse
-> <vA<AA>>^AvA<^A>AvA^Av<<A>>^AAvA^A| <vA>^A.v<<A>>^A.vA<^A>A |v<<A>A>^AAvA<^A>A

All alternatives have the same len
Test just one path from an alternative to find the one(s) with the best len, and grab all of them to continue further? still slow af
Try to figure out WHY, what is the heuristic to only have 1 path

 */
