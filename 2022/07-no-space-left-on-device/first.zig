const std = @import("std");

const example = @embedFile("example");
const input = @embedFile("input");

pub fn main() anyerror!void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    try std.testing.expectEqual(try solve(example, allocator), 95437);
    const result = try solve(input, allocator);
    try std.io.getStdOut().writer().print("{}\n", .{result});
}

fn solve(puzzle: []const u8, allocator: std.mem.Allocator) !u64 {
    var it = std.mem.tokenize(u8, puzzle, "\n");
    var tot: usize = 0;
    var n: usize = 0; // depth
    var stack = std.ArrayList(u64).init(allocator);
    _ = it.next() orelse unreachable; // drop the cd /
    try stack.append(0);
    while (it.next()) |line| {
        if (line[2] == 'c') { // a cd command
            if (line[5] == '.') { // going up
                const v = stack.pop();
                n -= 1;
                stack.items[n] += v;
                if (v <= 100000) {
                    tot += v;
                }
            } else { // going down
                n += 1;
                try stack.append(0);
            }
        } else if (line[2] == 'l') { // a ls command
            continue;
        } else if (line[2] == 'r') { // a dir entry in a ls command
            continue;
        } else { // a file entry in a ls command
            var elts = std.mem.split(u8, line, " ");
            const v = try std.fmt.parseInt(u64, elts.next() orelse unreachable, 10);
            stack.items[n] += v;
        }
    }
    while (n > 1) : (n -= 1) {
        const v = stack.pop();
        n -= 1;
        stack.items[n] += v;
        if (v <= 100000) {
            tot += v;
        }
    }
    return tot;
}
