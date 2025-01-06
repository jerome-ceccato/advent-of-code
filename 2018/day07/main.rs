use std::{
    collections::{HashMap, HashSet},
    fs,
};
use regex::Regex;

fn get_input(path: &str) -> HashMap<char, Vec<char>> {
    let re = Regex::new(r"Step (.) must be finished before step (.) can begin.").unwrap();
    let raw: Vec<(char, char)> = fs::read_to_string(path)
        .unwrap()
        .split("\n")
        .map(|l| {
            let caps = re.captures(l).unwrap();
            (caps[1].chars().next().unwrap(), caps[2].chars().next().unwrap())
        })
        .collect();
    let mut dependencies: HashMap<char, Vec<char>> = HashMap::new();
    for (lhs, rhs) in raw {
        dependencies.entry(rhs).or_default().push(lhs);
    }
    dependencies
}

fn is_ready(dependencies: &HashMap<char, Vec<char>>, completed: &Vec<char>, target: char) -> bool {
    if let Some(deps) = dependencies.get(&target) {
        for dep in deps {
            if !completed.contains(dep) {
                return false;
            }
        }
    }
    true
}

fn get_letters(dependencies: &HashMap<char, Vec<char>>) -> Vec<char> {
    let mut letters: HashSet<char> = HashSet::new();
    for (&k, v) in dependencies {
        letters.insert(k);
        for &c in v {
            letters.insert(c);
        }
    }

    let mut sorted: Vec<char> = letters.into_iter().collect();
    sorted.sort();
    sorted
}

fn get_available_task(dependencies: &HashMap<char, Vec<char>>, completed: &Vec<char>, letters: &Vec<char>) -> Option<char> {
    for &letter in letters.iter() {
        if !completed.contains(&letter) {
            if is_ready(dependencies, &completed, letter) {
                return Some(letter);
            }
        }
    }
    None
}

fn get_dependency_order(dependencies: &HashMap<char, Vec<char>>) -> Vec<char> {
    let mut order: Vec<char> = vec![];
    let letters = get_letters(dependencies);
    while order.len() < letters.len() {
        if let Some(letter) = get_available_task(dependencies, &order, &letters) {
            order.push(letter);
        }
    }
    order
}

#[derive(Debug, Clone)]
struct Worker {
    task: Option<char>,
    timer: i32,
}

fn get_available_worker<'a>(workers: &'a mut Vec<Worker>) -> Option<&'a mut Worker> {
    workers.iter_mut().filter(|w| w.task.is_none()).next()
}

fn get_available_task_workers(dependencies: &HashMap<char, Vec<char>>, completed: &Vec<char>, taken: &Vec<char>, letters: &Vec<char>) -> Option<char> {
    for &letter in letters.iter() {
        if !taken.contains(&letter) {
            if is_ready(dependencies, &completed, letter) {
                return Some(letter);
            }
        }
    }
    None
}

fn get_dependency_order_workers(dependencies: &HashMap<char, Vec<char>>, workers: usize, steps_time: i32) -> u32 {
    let mut workers: Vec<Worker> = vec![Worker { task: None, timer: 0}; workers];
    let mut order: Vec<char> = vec![];
    let mut taken: Vec<char> = vec![];
    let letters = get_letters(dependencies);
    let mut time_elapsed = 0;

    while order.len() < letters.len() {
        if let Some(worker) = get_available_worker(&mut workers) {
            if let Some(letter) = get_available_task_workers(dependencies, &order, &taken, &letters) {
                taken.push(letter);
                worker.task = Some(letter);
                worker.timer = steps_time + (letter as u8 - 'A' as u8) as i32 + 1;
                continue;
            }
        }

        for worker in workers.iter_mut() {
            if let Some(letter) = worker.task {
                worker.timer -= 1;
                if worker.timer == 0 {
                    order.push(letter);
                    worker.task = None;
                }
            }
        }
        time_elapsed += 1
    }
    time_elapsed
}

fn main() {
    let dependencies = get_input("input");
    let order: String = get_dependency_order(&dependencies).into_iter().collect();
    println!("{:?}", order);

    let order_time = get_dependency_order_workers(&dependencies, 5, 60);
    println!("{:?}", order_time);
}
