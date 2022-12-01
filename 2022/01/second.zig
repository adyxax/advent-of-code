const std = @import("std");

const example = @embedFile("example");
const input = @embedFile("input");

pub fn main() anyerror!void {
    try std.testing.expectEqual(solve(example), 45000);
    const result = try solve(input);
    try std.io.getStdOut().writer().print("{}\n", .{result});
}

fn solve(puzzle: []const u8) !u64 {
    var it = std.mem.split(u8, puzzle, "\n");
    var top3 = [_]u64{ 0, 0, 0 };
    var tot: u64 = 0;
    while (it.next()) |value| {
        const n = std.fmt.parseInt(u64, value, 10) catch 0;
        if (n == 0) {
            for (top3) |*max| {
                if (tot > max.*) {
                    // this swapping will keep the array sorted
                    const prev = max.*;
                    max.* = tot;
                    tot = prev;
                }
            }
            tot = 0;
        } else {
            tot += n;
        }
    }
    return top3[0] + top3[1] + top3[2];
}
