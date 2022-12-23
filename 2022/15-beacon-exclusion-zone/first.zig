const std = @import("std");

const example = @embedFile("example");
const input = @embedFile("input");

var allocator: std.mem.Allocator = undefined;

pub fn main() anyerror!void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    allocator = arena.allocator();

    try std.testing.expectEqual(try solve(example, 10), 26);
    const result = try solve(input, 2000000);
    try std.io.getStdOut().writer().print("{}\n", .{result});
}

const Interval = struct {
    l: i64,
    r: i64,
};

fn lesserThan(context: void, a: *Interval, b: *Interval) bool {
    _ = context;
    return a.l < b.l;
}

fn solve(puzzle: []const u8, yline: i64) !i64 {
    var it = std.mem.tokenize(u8, puzzle, "\n");
    var intervals = std.ArrayList(*Interval).init(allocator);
    // process input
    while (it.next()) |line| {
        var coords = std.mem.tokenize(u8, line, "Sensor at x=, y=: closest beacon is at x=, y=");
        const sx = try std.fmt.parseInt(i64, coords.next() orelse continue, 10);
        const sy = try std.fmt.parseInt(i64, coords.next() orelse unreachable, 10);
        const bx = try std.fmt.parseInt(i64, coords.next() orelse unreachable, 10);
        const by = try std.fmt.parseInt(i64, coords.next() orelse unreachable, 10);
        const d = try std.math.absInt(sx - bx) + try std.math.absInt(sy - by);
        if (sy - d > yline or sy + d < yline) {
            continue;
        }
        const l = d - try std.math.absInt(yline - sy);
        var i = try allocator.create(Interval);
        i.* = Interval{ .l = sx - l, .r = sx + l };
        try intervals.append(i);
    }
    // sort by left bound
    std.sort.sort(*Interval, intervals.items, {}, lesserThan);
    // Reduce intervals and compute results
    var n: i64 = 0;
    var i: usize = 0;
    while (i < intervals.items.len) : (i += 1) {
        var int = intervals.items[i];
        var j = i + 1;
        while (j < intervals.items.len) {
            const jnt = intervals.items[j];
            if (jnt.l <= int.r) { // jint overlaps
                if (jnt.r > int.r) {
                    int.r = jnt.r;
                }
                _ = intervals.orderedRemove(j);
                continue;
            } else {
                break;
            }
            j += 1;
        }
        n = n + int.r - int.l;
    }
    return n;
}
