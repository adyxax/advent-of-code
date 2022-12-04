const std = @import("std");

const example = @embedFile("example");
const input = @embedFile("input");

pub fn main() anyerror!void {
    try std.testing.expectEqual(solve(example), 2);
    const result = try solve(input);
    try std.io.getStdOut().writer().print("{}\n", .{result});
}

fn solve(puzzle: []const u8) !u64 {
    var it = std.mem.tokenize(u8, puzzle, "\n");
    var tot: u64 = 0;
    while (it.next()) |line| {
        var it2 = std.mem.tokenize(u8, line, "-,");
        const a = try std.fmt.parseInt(u64, it2.next() orelse unreachable, 10);
        const b = try std.fmt.parseInt(u64, it2.next() orelse unreachable, 10);
        const c = try std.fmt.parseInt(u64, it2.next() orelse unreachable, 10);
        const d = try std.fmt.parseInt(u64, it2.next() orelse unreachable, 10);
        if (a <= c and b >= d or a >= c and b <= d) {
            tot += 1;
        }
    }
    return tot;
}
