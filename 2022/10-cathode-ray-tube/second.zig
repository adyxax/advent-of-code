const std = @import("std");

const input = @embedFile("input");

var result: [6 * 40]u8 = undefined;

pub fn main() anyerror!void {
    solve(input);
    var y: usize = 0;
    while (y < 6) : (y += 1) {
        std.debug.print("{s}\n", .{result[y * 40 .. (y + 1) * 40]});
    }
}

fn solve(puzzle: []const u8) void {
    var it = std.mem.tokenize(u8, puzzle, "\n");
    var cycle: usize = 0;
    var x: i64 = 1;
    var line: []const u8 = undefined;
    var prev: ?i64 = null;
    // process input
    while (cycle < 40 * 6) : (cycle += 1) {
        const pos = @mod(cycle, 40);
        const draw = pos == x - 1 or pos == x or pos == x + 1;
        result[cycle] = if (draw) '#' else '.';
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
}
