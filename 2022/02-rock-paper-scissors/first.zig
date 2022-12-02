const std = @import("std");

const example = @embedFile("example");
const input = @embedFile("input");

pub fn main() anyerror!void {
    try std.testing.expectEqual(solve(example), 15);
    const result = try solve(input);
    try std.io.getStdOut().writer().print("{}\n", .{result});
}

const scores = [3][3]u8{ // X  Y  Z
    [3]u8{ 4, 8, 3 }, // A  4  8  3
    [3]u8{ 1, 5, 9 }, // B  1  5  9
    [3]u8{ 7, 2, 6 }, // C  7  2  6
};

fn solve(puzzle: []const u8) !u64 {
    var it = std.mem.tokenize(u8, puzzle, "\n");
    var tot: u64 = 0;
    while (it.next()) |line| {
        tot += scores[line[0] - 'A'][line[2] - 'X'];
    }
    return tot;
}
