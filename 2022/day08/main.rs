use std::fs;

struct Tree {
    height: i8,
    visible: bool, 
}

fn mark_line(trees: &mut Vec<Tree>, w: i32, h: i32, x: i32, y: i32, dx: i32, dy: i32) {
    let coord_valid = |x, y| { x >= 0 && x < w && y >= 0 && y < h };
    let idx = |x, y| { (x + y * w) as usize };
    
    let mut x = x;
    let mut y = y;
    let mut tallest = -1;

    while coord_valid(x, y) {
        let mut tree = &mut trees[idx(x, y)];
        if tree.height > tallest {
            tree.visible = true;
            tallest = tree.height;
        }

        x += dx;
        y += dy;
    }
}

fn mark_trees(trees: &mut Vec<Tree>, w: i32, h: i32) {
    for x in 0..w {
        mark_line(trees, w, h, x, 0, 0, 1);
        mark_line(trees, w, h, x, h - 1, 0, -1);
    }

    for y in 0..h {
        mark_line(trees, w, h, 0, y, 1, 0);
        mark_line(trees, w, h, w - 1, y, -1, 0);
    }
}

fn main() {
    let input = fs::read_to_string("input").unwrap();
    let width = input.chars().position(|c| c == '\n').unwrap();
    let mut trees: Vec<Tree> = input.chars().filter_map(|c| match c {
        '0'..='9' => Some(Tree { height: c as i8 - '0' as i8, visible: false }),
        _ => None
    }).collect();
    let height = trees.len() / width;

    // part 1
    {
        mark_trees(&mut trees, width as i32, height as i32);

        let nvisible = trees.iter().fold(0u32, |acc, t| { acc + t.visible as u32 });
        println!("{nvisible}");
    }
}