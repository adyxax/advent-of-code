const std = @import("std");

const example = @embedFile("example");
const input = @embedFile("input");

pub fn main() anyerror!void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    try std.testing.expectEqual(try solve(example, allocator), 31);
    const result = try solve(input, allocator);
    try std.io.getStdOut().writer().print("{}\n", .{result});
}

const Node = struct {
    x: u8,
    y: u8,
    cost: ?u64,
    value: u8,
    fn evalAndAdd(e: *Node, n: *Node, nq: *std.PriorityQueue(*Node, void, lesserThan)) !void {
        if (e.cost == null and e.value <= n.value + 1) {
            e.cost = n.cost.? + 1;
            try nq.add(e);
        }
    }
};

fn lesserThan(context: void, a: *Node, b: *Node) std.math.Order {
    _ = context;
    return std.math.order(a.cost.?, b.cost.?);
}

fn solve(puzzle: []const u8, allocator: std.mem.Allocator) !u64 {
    var it = std.mem.tokenize(u8, puzzle, "\n");
    var grid = std.ArrayList(std.ArrayList(Node)).init(allocator);
    var Sx: u8 = undefined;
    var Sy: u8 = undefined;
    var Ex: u8 = undefined;
    var Ey: u8 = undefined;
    // process input
    var y: u8 = 0;
    var line = it.next() orelse unreachable;
    const width = line.len;
    while (true) : (line = it.next() orelse break) {
        var l = try grid.addOne();
        l.* = try std.ArrayList(Node).initCapacity(allocator, width);
        var x: u8 = 0;
        while (x < line.len) : (x += 1) {
            var n = l.*.addOneAssumeCapacity();
            n.*.x = x;
            n.*.y = y;
            n.*.cost = null;
            n.*.value = switch (line[x]) {
                'S' => blk: {
                    Sx = x;
                    Sy = y;
                    n.*.cost = 0;
                    break :blk 'a';
                },
                'E' => blk: {
                    Ex = x;
                    Ey = y;
                    break :blk 'z';
                },
                else => line[x],
            };
        }
        y += 1;
    }
    const height = grid.items.len;
    // find shortest path
    var nq = std.PriorityQueue(*Node, void, lesserThan).init(allocator, {});
    try nq.add(&grid.items[Sy].items[Sx]);
    while (true) {
        var n = nq.remove();
        if (n.x == Ex and n.y == Ey) {
            return n.cost.?;
        }
        if (n.x > 0) {
            try grid.items[n.y].items[n.x - 1].evalAndAdd(n, &nq);
        }
        if (n.x < width - 1) {
            try grid.items[n.y].items[n.x + 1].evalAndAdd(n, &nq);
        }
        if (n.y > 0) {
            try grid.items[n.y - 1].items[n.x].evalAndAdd(n, &nq);
        }
        if (n.y < height - 1) {
            try grid.items[n.y + 1].items[n.x].evalAndAdd(n, &nq);
        }
    }
}
