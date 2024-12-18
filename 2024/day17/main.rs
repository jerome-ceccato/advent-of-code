use std::fs;

fn get_input(path: &str) -> ([i64; 3], Vec<u8>) {
    let blocks: Vec<String> = fs::read_to_string(path)
        .unwrap()
        .split("\n\n")
        .map(String::from)
        .collect();
    let registers: Vec<i64> = blocks[0]
        .split("\n")
        .map(|l| l.split(": ").last().unwrap().parse().unwrap())
        .collect();
    let program: Vec<u8> = blocks[1]
        .split(": ")
        .last()
        .unwrap()
        .split(",")
        .map(|n| n.parse().unwrap())
        .collect();
    return ([registers[0], registers[1], registers[2]], program);
}

fn get_combo(registers: &[i64; 3], value: u8) -> i64 {
    if value <= 3 {
        value as i64
    } else if value <= 6 {
        registers[(value - 4) as usize]
    } else {
        panic!()
    }
}

fn run(mut registers: [i64; 3], instructions: &Vec<u8>) -> Vec<u8> {
    let mut ip: i32 = 0;
    let mut output: Vec<u8> = vec![];

    while ip >= 0 && (ip as usize) < instructions.len() {
        match instructions[ip as usize] {
            // adv
            0 => {
                let value = get_combo(&registers, instructions[ip as usize + 1]);
                registers[0] = (registers[0] as f64 / 2_i64.pow(value as u32) as f64) as i64;
            }
            // bxl
            1 => {
                let value = instructions[ip as usize + 1] as i64;
                registers[1] = registers[1] ^ value;
            }
            // bst
            2 => {
                let value = get_combo(&registers, instructions[ip as usize + 1]);
                registers[1] = value % 8;
            }
            // jnz
            3 => {
                if registers[0] != 0 {
                    // we remove 2 because the ip will be increased afterwards
                    ip = instructions[ip as usize + 1] as i32 - 2;
                }
            }
            // bxc
            4 => {
                registers[1] = registers[1] ^ registers[2];
            }
            // out
            5 => {
                let value = get_combo(&registers, instructions[ip as usize + 1]);
                output.push((value % 8) as u8);
            }
            // bdv
            6 => {
                let value = get_combo(&registers, instructions[ip as usize + 1]);
                registers[1] = (registers[0] as f64 / 2_i64.pow(value as u32) as f64) as i64;
            }
            // cdv
            7 => {
                let value = get_combo(&registers, instructions[ip as usize + 1]);
                registers[2] = (registers[0] as f64 / 2_i64.pow(value as u32) as f64) as i64;
            }
            _ => panic!(),
        }

        ip += 2;
    }
    output
}

fn part1() {
    let input = get_input("input");
    let output = run(input.0, &input.1);
    let output_program = output
        .iter()
        .map(u8::to_string)
        .collect::<Vec<String>>()
        .join(",");
    println!("{:?}", output_program);
}

fn test_a(mut a: i64, instructions: &Vec<u8>) -> bool {
    let mut i = 0;
    while i < instructions.len()
        && instructions[i] == ((((a & 7) ^ 7) ^ (a >> ((a & 7) ^ 7)) ^ 7) & 7) as u8
    {
        a >>= 3;
        i += 1;
    }

    i == instructions.len()
}

fn part2() {
    let instructions = get_input("input").1;

    let mut x: i64 = 0;
    loop {
        for low in 0_i64..8 {
            let a = low | (0b010110010001110011 << 3) | (x << 21);
            if test_a(a, &instructions) {
                println!("{}", a);
                return;
            }
        }
        x += 1;
    }
}

fn main() {
    part1();
    part2();
}

// Program: 2,4,1,7,7,5,0,3,4,4,1,7,5,5,3,0
//
// 2,4
// B = A & 7 # last 3 bits
// 1,7
// B = (A & 7) ^ 7 # last 3 bits reversed
// 7,5
// C = A / (2^B), A / (1 << B), A >> B # A with B bits removed
// 0,3
// A >>= 3 # last 3 bits removed from A
// 4,4
// B ^= C, B = B ^ (A >> B), B = ((A & 7) ^ 7) ^ (A >> ((A & 7) ^ 7))
// 1,7
// B ^= 7, B = ((A & 7) ^ 7) ^ (A >> ((A & 7) ^ 7)) ^ 7
// 5,5
// output B & 7
// 3,0
// restart with A >>= 3

/*
Some examples of valid numbers,
they all contain (010110010001110011 << 3)

100110001000010110010001110011011
100110001000010110010001110011101
100110001011010110010001110011011
100110001011010110010001110011101
100110101000010110010001110011011
100110101000010110010001110011101
100111001000010110010001110011011
100111001000010110010001110011101
100111101000010110010001110011011
100111101000010110010001110011101
101000001011010110010001110011011
101000001011010110010001110011011
101000001011010110010001110011101
101000001011010110010001110011101
101000101010010110010001110011011
101000101010010110010001110011011
101000101010010110010001110011101
101000101010010110010001110011101
101010001001010110010001110011011
101010001001010110010001110011011
101010001001010110010001110011101
101010001001010110010001110011101
101010001011010110010001110011011
101010001011010110010001110011011
101010001011010110010001110011101
101010001011010110010001110011101
101010101001010110010001110011011
101010101001010110010001110011101
101100001011010110010001110011011
101100001011010110010001110011011
101100001011010110010001110011101
101100001011010110010001110011101
101100101010010110010001110011011
101100101010010110010001110011011
101100101010010110010001110011101
101100101010010110010001110011101
*/
