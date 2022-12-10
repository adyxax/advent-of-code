const std = @import("std");

const example = @embedFile("example");
const input = @embedFile("input");

pub fn main() anyerror!void {
    try std.testing.expectEqual(solve(example), 13140);
    const result = solve(input);
    try std.io.getStdOut().writer().print("{}\n", .{result});
}

fn solve(puzzle: []const u8) i64 {
    var it = std.mem.tokenize(u8, puzzle, "\n");
    var tot: i64 = 0;
    var cycle: i64 = 1;
    var x: i64 = 1;
    var line: []const u8 = undefined;
    var prev: ?i64 = null;
    // process input
    while (cycle <= 220) : (cycle += 1) {
        if (@mod(cycle - 20, 40) == 0) {
            tot += x * cycle;
        }
        if (prev) |p| { // cpu is busy adding
            x += p;
            prev = null;
        } else { // read another instruction
            line = it.next() orelse break;
            if (line[0] == 'a') {
                var elts = std.mem.split(u8, line, " ");
                _ = elts.next() orelse unreachable;
                prev = std.fmt.parseInt(i64, elts.next() orelse unreachable, 10) catch unreachable;
            }
        }
    }
    return tot;
}
