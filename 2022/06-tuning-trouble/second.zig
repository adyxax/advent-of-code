const std = @import("std");

const example = @embedFile("example");
const example2 = @embedFile("example2");
const example3 = @embedFile("example3");
const example4 = @embedFile("example4");
const example5 = @embedFile("example5");
const input = @embedFile("input");

pub fn main() anyerror!void {
    try std.testing.expectEqual(solve(example), 19);
    try std.testing.expectEqual(solve(example2), 23);
    try std.testing.expectEqual(solve(example3), 23);
    try std.testing.expectEqual(solve(example4), 29);
    try std.testing.expectEqual(solve(example5), 26);
    const result = solve(input);
    try std.io.getStdOut().writer().print("{}\n", .{result});
}

fn solve(puzzle: []const u8) u64 {
    var n: u64 = 13;
    outer: while (true) : (n += 1) {
        var i: u64 = n;
        while (i >= n - 13) : (i -= 1) {
            var j: u64 = i - 1;
            while (j >= n - 13) : (j -= 1) {
                if (puzzle[i] == puzzle[j]) {
                    continue :outer;
                }
                if (j == 0) {
                    break;
                }
            }
        }
        return n + 1;
    }
}
