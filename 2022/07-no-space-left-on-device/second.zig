const std = @import("std");

const example = @embedFile("example");
const input = @embedFile("input");

pub fn main() anyerror!void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    try std.testing.expectEqual(try solve(example, allocator), 24933642);
    const result = try solve(input, allocator);
    try std.io.getStdOut().writer().print("{}\n", .{result});
}

fn solve(puzzle: []const u8, allocator: std.mem.Allocator) !u64 {
    var it = std.mem.tokenize(u8, puzzle, "\n");
    var tot: usize = 0;
    var n: usize = 0; // depth
    var sizes = std.ArrayList(u64).init(allocator);
    var stack = std.ArrayList(u64).init(allocator);
    _ = it.next() orelse unreachable; // drop the cd /
    try stack.append(0);
    while (it.next()) |line| {
        if (line[2] == 'c') { // a cd command
            if (line[5] == '.') { // going up
                const v = stack.pop();
                n -= 1;
                stack.items[n] += v;
                try sizes.append(v);
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
            tot += v;
        }
    }
    while (n > 1) : (n -= 1) { // we unwrap the cd up to /
        const v = stack.pop();
        n -= 1;
        stack.items[n] += v;
        try sizes.append(v);
    }
    try sizes.append(stack.items[n]); // we do not forget the last one
    n = 30000000 - (70000000 - tot); // the amount of space we need to free
    // let's run a dychotomy on the array
    std.sort.sort(u64, sizes.items, {}, std.sort.asc(u64));
    var a: usize = 0;
    var b = sizes.items.len - 1;
    while (b - a > 2) {
        const m = a + (b - a) / 2;
        if (sizes.items[m] < n) {
            a = m;
        } else {
            b = m;
        }
    }
    return sizes.items[b];
}
