const std = @import("std");

const example = @embedFile("example");
const input = @embedFile("input");

pub fn main() anyerror!void {
    try std.testing.expectEqual(solve(example), 7);
    const result = try solve(input);
    try std.io.getStdOut().writer().print("{}\n", .{result});
}

fn solve(puzzle: []const u8) !u64 {
    var it = std.mem.tokenize(u8, puzzle, " \n");
    var tot: u64 = 0;
    var prev: ?u64 = null;
    while (it.next()) |value| {
        const n = try std.fmt.parseInt(u64, value, 10);
        if (prev) |p| {
            if (p < n) {
                tot += 1;
            }
        }
        prev = n;
    }
    return tot;
}
