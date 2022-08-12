const std = @import("std");

const input = @embedFile("input");

pub fn main() anyerror!void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const result = try solve(input, 12, allocator);
    try std.io.getStdOut().writer().print("{}\n", .{result});
}

fn solve(puzzle: []const u8, width: u8, allocator: std.mem.Allocator) !u64 {
    var it = std.mem.tokenize(u8, puzzle, "\n");
    var bits = std.ArrayList(u64).init(allocator);
    defer bits.deinit();

    if (it.next()) |line| {
        for (line) |c| {
            var v: u8 = 0;
            if (c == '1') {
                v = 1;
            }
            try bits.append(v);
        }
    }
    var len: usize = 1;
    while (it.next()) |line| : (len += 1) {
        for (line) |c, i| {
            if (c == '1') {
                bits.items[i] += 1;
            }
        }
    }

    var gamma: u64 = 0;
    var epsilon: u64 = 0;
    var i: usize = 0;
    while (i < width) : (i += 1) {
        gamma <<= 1;
        epsilon <<= 1;
        if (bits.items[i] > len / 2) {
            gamma += 1;
        } else {
            epsilon += 1;
        }
    }
    return gamma * epsilon;
}

test "solve" {
    const example = @embedFile("example");
    try std.testing.expectEqual(solve(example, 5, std.testing.allocator), 198);
}
