const std = @import("std");

const example = @embedFile("example");
const input = @embedFile("input");

pub fn main() anyerror!void {
    const result = try solve(input);
    try std.io.getStdOut().writer().print("{}\n", .{result});
}

fn solve(puzzle: []const u8) !u64 {
    var it = std.mem.tokenize(u8, puzzle, "\n");
    var aim: u64 = 0;
    var pos: u64 = 0;
    var depth: u64 = 0;
    while (it.next()) |line| {
        var it2 = std.mem.tokenize(u8, line, " ");
        const step = it2.next() orelse unreachable;
        const n: u64 = std.fmt.parseInt(u64, it2.next().?, 10) catch unreachable;
        switch (step[0]) {
            'f' => {
                pos += n;
                depth += n * aim;
            },
            'd' => aim += n,
            'u' => aim -= n,
            else => unreachable,
        }
    }
    return pos * depth;
}

test "solve" {
    try std.testing.expectEqual(solve(example), 900);
}
