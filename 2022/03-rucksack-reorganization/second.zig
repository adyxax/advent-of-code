const std = @import("std");

const example = @embedFile("example");
const input = @embedFile("input");

pub fn main() anyerror!void {
    try std.testing.expectEqual(solve(example), 70);
    const result = try solve(input);
    try std.io.getStdOut().writer().print("{}\n", .{result});
}

fn solve(puzzle: []const u8) !u64 {
    var it = std.mem.tokenize(u8, puzzle, "\n");
    var tot: u64 = 0;
    while (it.next()) |line1| {
        if (it.next()) |line2| {
            if (it.next()) |line3| {
                var i: usize = 0;
                outer: while (i < line1.len) : (i += 1) {
                    var j: usize = 0;
                    while (j < line2.len) : (j += 1) {
                        if (line1[i] == line2[j]) {
                            var k: usize = 0;
                            while (k < line3.len) : (k += 1) {
                                if (line1[i] == line3[k]) {
                                    break :outer;
                                }
                            }
                            continue :outer; // char on line1 was not found in line3
                        }
                    }
                    continue :outer; // char on line1 was not found in line2
                }
                if (line1[i] <= 'Z') {
                    tot = tot + line1[i] - 'A' + 27;
                } else {
                    tot = tot + line1[i] - 'a' + 1;
                }
            }
        }
    }
    return tot;
}
