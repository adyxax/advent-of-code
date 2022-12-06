const std = @import("std");

const example = @embedFile("example");
const example2 = @embedFile("example2");
const example3 = @embedFile("example3");
const example4 = @embedFile("example4");
const example5 = @embedFile("example5");
const input = @embedFile("input");

pub fn main() anyerror!void {
    try std.testing.expectEqual(solve(example), 7);
    try std.testing.expectEqual(solve(example2), 5);
    try std.testing.expectEqual(solve(example3), 6);
    try std.testing.expectEqual(solve(example4), 10);
    try std.testing.expectEqual(solve(example5), 11);
    const result = solve(input);
    try std.io.getStdOut().writer().print("{}\n", .{result});
}

fn solve(puzzle: []const u8) u64 {
    var i: u64 = 3;
    while (true) : (i += 1) {
        if (puzzle[i] == puzzle[i - 1] or puzzle[i] == puzzle[i - 2] or puzzle[i] == puzzle[i - 3] or puzzle[i - 1] == puzzle[i - 2] or puzzle[i - 1] == puzzle[i - 3] or puzzle[i - 2] == puzzle[i - 3]) {
            continue;
        }
        return i + 1;
    }
}
