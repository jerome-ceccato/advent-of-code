const std = @import("std");

const Point = struct {
    x: i32,
    y: i32,
};

const Bounds = struct {
    min: Point,
    max: Point,
};

fn readInput(allocator: std.mem.Allocator) !std.ArrayList(Point) {
    var file = try std.fs.cwd().openFile("input", .{ .mode = .read_only });
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var result = std.ArrayList(Point).init(allocator);
    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var splitter = std.mem.split(u8, line, ", ");
        const x_str = splitter.next() orelse return error.Unexpected;
        const x = try std.fmt.parseInt(i32, x_str, 10);
        const y_str = splitter.next() orelse return error.Unexpected;
        const y = try std.fmt.parseInt(i32, y_str, 10);
        try result.append(Point{ .x = x, .y = y });
    }
    return result;
}

fn getBounds(coords: std.ArrayList(Point)) Bounds {
    var min = Point{ .x = 10000, .y = 10000 };
    var max = Point{ .x = -1, .y = -1 };

    for (coords.items) |item| {
        min.x = @min(item.x, min.x);
        min.y = @min(item.y, min.y);
        max.x = @max(item.x, max.x);
        max.y = @max(item.y, max.y);
    }
    return Bounds{ .min = min, .max = max };
}

fn manhattan(lhs: Point, rhs: Point) u32 {
    return @abs(lhs.x - rhs.x) + @abs(lhs.y - rhs.y);
}

fn getClosest(coords: std.ArrayList(Point), point: Point) ?Point {
    var closest = coords.items[0];
    for (coords.items) |item| {
        if (manhattan(item, point) < manhattan(closest, point)) {
            closest = item;
        }
    }

    // Discard if there are multiple points equally as close
    var closest_count: u8 = 0;
    for (coords.items) |item| {
        if (manhattan(item, point) == manhattan(closest, point)) {
            closest_count += 1;
            if (closest_count > 1) {
                return null;
            }
        }
    }
    return closest;
}

fn countArea(allocator: std.mem.Allocator, coords: std.ArrayList(Point)) !std.AutoHashMap(Point, i32) {
    var result = std.AutoHashMap(Point, i32).init(allocator);
    const bounds = getBounds(coords);
    var x = bounds.min.x - 1;
    while (x <= (bounds.max.x + 1)) : (x += 1) {
        var y = bounds.min.y - 1;
        while (y <= (bounds.max.y + 1)) : (y += 1) {
            const maybe_closest = getClosest(coords, Point{ .x = x, .y = y });
            if (maybe_closest) |closest| {
                const entry = try result.getOrPutValue(closest, 0);

                if (x < bounds.min.x or x > bounds.max.x or y < bounds.min.y or y > bounds.max.y) {
                    // Lazy way to invalidate infinite areas
                    entry.value_ptr.* = -1000000;
                } else {
                    entry.value_ptr.* += 1;
                }
            }
        }
    }
    return result;
}

fn dangerRating(coords: std.ArrayList(Point), point: Point) u32 {
    var result: u32 = 0;
    for (coords.items) |item| {
        result += manhattan(item, point);
    }
    return result;
}

fn countSafe(coords: std.ArrayList(Point), threshold: u32) u32 {
    var result: u32 = 0;
    const bounds = getBounds(coords);
    var x = bounds.min.x - 1;
    while (x <= (bounds.max.x + 1)) : (x += 1) {
        var y = bounds.min.y - 1;
        while (y <= (bounds.max.y + 1)) : (y += 1) {
            if (dangerRating(coords, Point{ .x = x, .y = y }) < threshold) {
                result += 1;
            }
        }
    }
    return result;
}

fn getMaxArea(area: std.AutoHashMap(Point, i32)) i32 {
    var best: i32 = -1;
    var iterator = area.iterator();
    while (iterator.next()) |entry| {
        best = @max(best, entry.value_ptr.*);
    }
    return best;
}

fn part1(allocator: std.mem.Allocator, stdout: anytype) !void {
    const coords = try readInput(allocator);
    defer coords.deinit();

    var area = try countArea(allocator, coords);
    defer area.deinit();

    const max_area = getMaxArea(area);
    try stdout.print("{d}\n", .{max_area});
}

fn part2(allocator: std.mem.Allocator, stdout: anytype) !void {
    const coords = try readInput(allocator);
    defer coords.deinit();

    const safe = countSafe(coords, 10000);
    try stdout.print("{d}\n", .{safe});
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
