const std = @import("std");

const example = @embedFile("example");
const input = @embedFile("input");

pub fn main() anyerror!void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    try std.testing.expectEqual(try solve(example, allocator), 21);
    const result = try solve(input, allocator);
    try std.io.getStdOut().writer().print("{}\n", .{result});
}

fn solve(puzzle: []const u8, allocator: std.mem.Allocator) !u64 {
    var it = std.mem.tokenize(u8, puzzle, "\n");
    var heights = std.ArrayList(std.ArrayList(u8)).init(allocator);
    var visibility = std.ArrayList(std.ArrayList(bool)).init(allocator);
    // process input
    var line = it.next() orelse unreachable;
    const width = line.len;
    while (true) {
        var h = try heights.addOne();
        var v = try visibility.addOne();
        h.* = try std.ArrayList(u8).initCapacity(allocator, width);
        v.* = try std.ArrayList(bool).initCapacity(allocator, width);
        try h.*.writer().writeAll(line);
        v.*.appendAssumeCapacity(true);
        var i: usize = 1;
        while (i < width - 1) : (i += 1) {
            v.*.appendAssumeCapacity(false);
        }
        v.*.appendAssumeCapacity(true);
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
        visibility.items[0].items[x] = true;
        visibility.items[lastY].items[x] = true;
    }
    // calculate visible trees in lines
    var y: usize = 1;
    while (y < lastY) : (y += 1) {
        const treeline = heights.items[y];
        // look right
        var max = treeline.items[0];
        x = 1;
        while (x < lastX) : (x += 1) {
            if (treeline.items[x] > max) {
                visibility.items[y].items[x] = true;
                max = treeline.items[x];
            }
        }
        // look left
        max = treeline.items[lastX];
        x = lastX - 1;
        while (x > 0) : (x -= 1) {
            if (treeline.items[x] > max) {
                visibility.items[y].items[x] = true;
                max = treeline.items[x];
            }
        }
    }
    // calculate visible trees in columns
    x = 1;
    while (x < lastX) : (x += 1) {
        // look down
        var max = heights.items[0].items[x];
        y = 1;
        while (y < lastY) : (y += 1) {
            if (heights.items[y].items[x] > max) {
                visibility.items[y].items[x] = true;
                max = heights.items[y].items[x];
            }
        }
        // look up
        max = heights.items[lastY].items[x];
        y = lastY - 1;
        while (y > 0) : (y -= 1) {
            if (heights.items[y].items[x] > max) {
                visibility.items[y].items[x] = true;
                max = heights.items[y].items[x];
            }
        }
    }
    // calculate total
    var tot: usize = 0;
    y = 0;
    while (y <= lastY) : (y += 1) {
        x = 0;
        while (x <= lastX) : (x += 1) {
            if (visibility.items[y].items[x]) {
                tot += 1;
            }
        }
    }
    return tot;
}
