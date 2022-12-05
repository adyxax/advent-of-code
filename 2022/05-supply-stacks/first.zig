const std = @import("std");

const example = @embedFile("example");
const input = @embedFile("input");

pub fn main() anyerror!void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var output = try solve(example, allocator);
    try std.testing.expect(std.mem.eql(u8, output, "CMZ"));

    const result = try solve(input, allocator);
    try std.io.getStdOut().writer().print("{s}\n", .{result});
}

fn solve(puzzle: []const u8, allocator: std.mem.Allocator) ![]u8 {
    var it = std.mem.tokenize(u8, puzzle, "\n");
    var line = it.next() orelse unreachable;
    // Initializations from the first line
    const size = (line.len + 1) / 4;
    var output = try allocator.alloc(u8, size);
    var stacks = try allocator.alloc(std.ArrayList(u8), size);
    for (stacks) |*stack| {
        stack.* = std.ArrayList(u8).init(allocator);
    }
    // parse stacks
    while (true) {
        var i: usize = 0;
        while (i < size) : (i += 1) {
            const v = line[i * 4 + 1];
            if (v != ' ') {
                try stacks[i].insert(0, v);
            }
        }
        line = it.next() orelse unreachable;
        if (line[0] != '[') { // we got the line with numbers
            break;
        }
    }
    // process rules
    while (it.next()) |l| {
        var elts = std.mem.split(u8, l, " ");
        _ = elts.next() orelse unreachable; // the move word
        var n = try std.fmt.parseInt(u8, elts.next() orelse unreachable, 10);
        _ = elts.next() orelse unreachable; // the from word
        const s = try std.fmt.parseInt(u8, elts.next() orelse unreachable, 10) - 1;
        _ = elts.next() orelse unreachable; // the to word
        const d = try std.fmt.parseInt(u8, elts.next() orelse unreachable, 10) - 1;
        while (n > 0) : (n -= 1) {
            const v = stacks[s].pop();
            try stacks[d].append(v);
        }
    }
    // generate output
    var i: usize = 0;
    while (i < size) : (i += 1) {
        output[i] = stacks[i].pop();
    }
    return output;
}
