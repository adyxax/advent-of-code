const std = @import("std");

const example = @embedFile("example");
const input = @embedFile("input");

pub fn main() anyerror!void {
    try std.testing.expectEqual(solve(example), 5);
    const result = try solve(input);
    try std.io.getStdOut().writer().print("{}\n", .{result});
}

fn solve(puzzle: []const u8) !u64 {
    var it = std.mem.tokenize(u8, puzzle, " \n");
    var tot: u64 = 0;
    var prev = [_]?u64{null} ** 3;
    while (it.next()) |value| {
        const n = try std.fmt.parseInt(u64, value, 10);
        if (prev[0]) |p| {
            if (p < n) {
                tot += 1;
            }
        }
        var i: usize = 0;
        while (i < prev.len - 1) : (i += 1) {
            prev[i] = prev[i + 1];
        }
        prev[2] = n;
    }
    return tot;
}
