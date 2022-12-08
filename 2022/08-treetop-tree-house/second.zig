const std = @import("std");

const example = @embedFile("example");
const input = @embedFile("input");

pub fn main() anyerror!void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    try std.testing.expectEqual(try solve(example, allocator), 8);
    const result = try solve(input, allocator);
    try std.io.getStdOut().writer().print("{}\n", .{result});
}

fn solve(puzzle: []const u8, allocator: std.mem.Allocator) !u64 {
    var it = std.mem.tokenize(u8, puzzle, "\n");
    var heights = std.ArrayList(std.ArrayList(u8)).init(allocator);
    var scenic = std.ArrayList(std.ArrayList(u64)).init(allocator);
    // process input
    var line = it.next() orelse unreachable;
    const width = line.len;
    while (true) {
        var h = try heights.addOne();
        var v = try scenic.addOne();
        h.* = try std.ArrayList(u8).initCapacity(allocator, width);
        v.* = try std.ArrayList(u64).initCapacity(allocator, width);
        try h.*.writer().writeAll(line);
        var i: usize = 0;
        while (i < width) : (i += 1) {
            v.*.appendAssumeCapacity(0);
        }
        if (it.next()) |l| {
            line = l;
        } else {
            break;
        }
    }
    const lastX = width - 1;
    const lastY = heights.items.len - 1;
    // init first and last lines edges
    var x: usize = 1;
    while (x < width - 1) : (x += 1) {
        scenic.items[0].items[x] = 0;
        scenic.items[lastY].items[x] = 0;
    }
    // calculate scenic scores tree by tree
    var tot: usize = 0;
    var y: usize = 1;
    while (y < lastY) : (y += 1) {
        x = 1;
        while (x < lastX) : (x += 1) {
            const max = heights.items[y].items[x];
            // look right
            var sr: usize = 1; // scenic score to the right
            var i: usize = x + 1;
            while (i < lastX) : (i += 1) {
                if (heights.items[y].items[i] < max) {
                    sr += 1;
                } else {
                    break;
                }
            }
            // look left
            var sl: usize = 1;
            i = x - 1;
            while (i > 0) : (i -= 1) {
                if (heights.items[y].items[i] < max) {
                    sl += 1;
                } else {
                    break;
                }
            }
            // look down
            var sd: usize = 1;
            i = y + 1;
            while (i < lastY) : (i += 1) {
                if (heights.items[i].items[x] < max) {
                    sd += 1;
                } else {
                    break;
                }
            }
            // look up
            var su: usize = 1;
            i = y - 1;
            while (i > 0) : (i -= 1) {
                if (heights.items[i].items[x] < max) {
                    su += 1;
                } else {
                    break;
                }
            }
            const score = sr * sl * sd * su;
            if (score > tot) {
                tot = score;
            }
        }
    }
    return tot;
}
