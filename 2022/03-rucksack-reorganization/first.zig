const std = @import("std");

const example = @embedFile("example");
const input = @embedFile("input");

pub fn main() anyerror!void {
    try std.testing.expectEqual(solve(example), 157);
    const result = try solve(input);
    try std.io.getStdOut().writer().print("{}\n", .{result});
}

fn solve(puzzle: []const u8) !u64 {
    var it = std.mem.tokenize(u8, puzzle, "\n");
    var tot: u64 = 0;
    while (it.next()) |line| {
        var i: usize = 0;
        var middle = line.len / 2;
        outer: while (i < middle) : (i += 1) {
            var j = middle;
            while (j < line.len) : (j += 1) {
                if (line[i] == line[j]) {
                    break :outer;
                }
            }
        }
        if (line[i] <= 'Z') {
            tot = tot + line[i] - 'A' + 27;
        } else {
            tot = tot + line[i] - 'a' + 1;
        }
    }
    return tot;
}
