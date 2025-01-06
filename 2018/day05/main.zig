const std = @import("std");

fn readInput(allocator: std.mem.Allocator) ![]u8 {
    return std.fs.cwd().readFileAlloc(allocator, "input", 1024 * 1024);
}

fn deleteAt(polymer: []u8, start: usize) void {
    var i: usize = start;
    while (i < polymer.len and polymer[i] != ' ') : (i += 1) {
        if ((i + 2) < polymer.len) {
            polymer[i] = polymer[i + 2];
        } else {
            polymer[i] = ' ';
        }
    }
}

fn polymer_size(polymer: []u8) usize {
    for (polymer, 0..) |c, i| {
        if (c == ' ') {
            return i;
        }
    }
    return polymer.len;
}

fn reducePolymer(polymer: []u8) void {
    var stable = false;
    var i: usize = 0;
    while (!stable) {
        stable = true;
        while (i < (polymer.len - 1)) : (i += 1) {
            if (std.ascii.toLower(polymer[i]) == std.ascii.toLower(polymer[i + 1]) and
                std.ascii.isLower(polymer[i]) != std.ascii.isLower(polymer[i + 1]))
            {
                deleteAt(polymer, i);
                stable = false;
                if (i > 0)
                    i -= 1;
                break;
            }
        }
    }
}

fn deleteLetter(polymer: []u8, letter: u8) void {
    var w: usize = 0;
    for (polymer, 0..) |c, r| {
        if (!(c == letter or c == std.ascii.toUpper(letter))) {
            polymer[w] = polymer[r];
            w += 1;
        }
    }
    while (w < polymer.len) : (w += 1) {
        polymer[w] = ' ';
    }
}

fn part1(allocator: std.mem.Allocator, stdout: anytype) !void {
    const input = try readInput(allocator);
    defer allocator.free(input);

    reducePolymer(input);
    const p1 = polymer_size(input);
    try stdout.print("{d}\n", .{p1});
}

fn part2(allocator: std.mem.Allocator, stdout: anytype) !void {
    var shortest: usize = 10000000;
    var letter: u8 = 'a';
    while (letter <= 'z') : (letter += 1) {
        const polymer = try readInput(allocator);
        defer allocator.free(polymer);

        deleteLetter(polymer, letter);
        reducePolymer(polymer);
        shortest = @min(shortest, polymer_size(polymer));
    }

    try stdout.print("{d}\n", .{shortest});
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    var stdout_bw = std.io.bufferedWriter(std.io.getStdOut().writer());
    const stdout = stdout_bw.writer();

    try part1(allocator, stdout);
    try stdout_bw.flush();

    try part2(allocator, stdout);
    try stdout_bw.flush();

    _ = gpa.deinit();
}
