const std = @import("std");

const example = @embedFile("example");
const input = @embedFile("input");

var allocator: std.mem.Allocator = undefined;

pub fn main() anyerror!void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    allocator = arena.allocator();

    try std.testing.expectEqual(try solve(example, 20), 56000011);
    const result = try solve(input, 4000000);
    try std.io.getStdOut().writer().print("{}\n", .{result});
}

const Point = struct {
    x: i64,
    y: i64,
};

const Line = struct {
    a: Point,
    b: Point,
};

fn solve(puzzle: []const u8, size: i64) !i64 {
    var it = std.mem.tokenize(u8, puzzle, "\n");
    var descendingLines = std.ArrayList(Line).init(allocator);
    var ascendingLines = std.ArrayList(Line).init(allocator);
    // process input
    while (it.next()) |line| {
        var coords = std.mem.tokenize(u8, line, "Sensor at x=, y=: closest beacon is at x=, y=");
        const sx = try std.fmt.parseInt(i64, coords.next() orelse continue, 10);
        const sy = try std.fmt.parseInt(i64, coords.next() orelse unreachable, 10);
        const bx = try std.fmt.parseInt(i64, coords.next() orelse unreachable, 10);
        const by = try std.fmt.parseInt(i64, coords.next() orelse unreachable, 10);
        const d = try std.math.absInt(sx - bx) + try std.math.absInt(sy - by) + 1; // plus one!
        var i = try descendingLines.addOne();
        i.* = Line{ .a = Point{ .x = sx, .y = sy - d }, .b = Point{ .x = sx + d, .y = sy } };
        i = try ascendingLines.addOne();
        i.* = Line{ .a = Point{ .x = sx, .y = sy + d }, .b = Point{ .x = sx + d, .y = sy } };
        i = try descendingLines.addOne();
        i.* = Line{ .a = Point{ .x = sx - d, .y = sy }, .b = Point{ .x = sx, .y = sy + d } };
        i = try ascendingLines.addOne();
        i.* = Line{ .a = Point{ .x = sx - d, .y = sy }, .b = Point{ .x = sx, .y = sy - d } };
    }
    // We look for intersections
    var intersections = std.ArrayList(Point).init(allocator);
    for (ascendingLines.items) |al| { // for each ascending line
        var n = al.a.y - al.a.x; // y = x + n
        for (descendingLines.items) |dl| { // for each descending line
            var p = dl.a.y + dl.a.x; // y = -x + p
            if (@mod(p + n, 2) == 1) {
                continue;
            }
            var ix = @divExact(p - n, 2);
            var iy = @divExact(p + n, 2);
            if (ix < 0 or ix > size or iy < 0 or iy > size) {
                continue;
            }
            //if (ix >= al.a.x and ix <= al.b.x and iy >= al.b.y and iy <= al.a.y) {  // this is missing some + 1
            //    if (ix >= dl.a.x and ix <= dl.b.x and iy >= dl.a.y and iy <= dl.b.y) {
            try intersections.append(Point{ .x = ix, .y = iy });
            //    }
            //}
        }
    }
    // we look into those one by one
    outer: for (intersections.items) |i| {
        var it2 = std.mem.tokenize(u8, puzzle, "\n"); // should cache this parsing result
        while (it2.next()) |line| {
            var coords = std.mem.tokenize(u8, line, "Sensor at x=, y=: closest beacon is at x=, y=");
            const sx = try std.fmt.parseInt(i64, coords.next() orelse continue, 10);
            const sy = try std.fmt.parseInt(i64, coords.next() orelse unreachable, 10);
            const bx = try std.fmt.parseInt(i64, coords.next() orelse unreachable, 10);
            const by = try std.fmt.parseInt(i64, coords.next() orelse unreachable, 10);
            const d = try std.math.absInt(sx - bx) + try std.math.absInt(sy - by);
            if (try std.math.absInt(sx - i.x) + try std.math.absInt(sy - i.y) <= d) {
                continue :outer;
            }
        }
        return i.x * 4000000 + i.y;
    }
    return 0;
}
