use itertools::iproduct;
use std::{collections::HashMap, fs};

fn get_input(path: &str) -> Vec<i64> {
    fs::read_to_string(path)
        .unwrap()
        .split("\n")
        .map(|l| l.parse().unwrap())
        .collect()
}

fn next(mut n: i64) -> i64 {
    n = (n ^ (n * 64)) % 16777216;
    n = (n ^ (n / 32)) % 16777216;
    n = (n ^ (n * 2048)) % 16777216;
    n
}

fn next_many(n: i64, times: usize) -> Vec<i64> {
    (0..=times)
        .scan(n, |v, _| {
            let res = *v;
            *v = next(*v);
            Some(res)
        })
        .collect()
}

type PurchaseWindow = (i8, i8, i8, i8);
fn change_windows(seq: Vec<i64>) -> HashMap<PurchaseWindow, i8> {
    seq.into_iter()
        .map(|n| (n % 10) as i8)
        .collect::<Vec<i8>>()
        .windows(2)
        .map(|n| (n[1], n[1] - n[0]))
        .collect::<Vec<(i8, i8)>>()
        .windows(4)
        .map(|w| ((w[0].1, w[1].1, w[2].1, w[3].1), w[3].0))
        .fold(HashMap::new(), |mut acc, item| {
            acc.entry(item.0).or_insert(item.1);
            acc
        })
}

fn calculate_profit(buyers: &Vec<HashMap<PurchaseWindow, i8>>, window: PurchaseWindow) -> i64 {
    buyers
        .iter()
        .map(|buyer| *buyer.get(&window).unwrap_or(&0) as i64)
        .sum()
}

fn find_best_purchase_window(buyers: &Vec<HashMap<PurchaseWindow, i8>>) -> (PurchaseWindow, i64) {
    iproduct!(-9..=9, -9..=9, -9..=9, -9..=9)
        .map(|w| (w, calculate_profit(buyers, w)))
        .max_by_key(|t| t.1)
        .unwrap()
}

fn main() {
    let market = get_input("input");
    let part1: i64 = market
        .iter()
        .map(|&buyer| *next_many(buyer, 2000).last().unwrap())
        .sum();
    println!("{:?}", part1);

    let windows = market
        .into_iter()
        .map(|buyer| change_windows(next_many(buyer, 2000)))
        .collect();
    let part2 = find_best_purchase_window(&windows).1;
    println!("{:?}", part2);
}
