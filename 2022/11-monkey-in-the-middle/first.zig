const std = @import("std");

const example = @embedFile("example");
const input = @embedFile("input");

pub fn main() anyerror!void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    try std.testing.expectEqual(try solve(example, allocator), 10605);
    const result = try solve(input, allocator);
    try std.io.getStdOut().writer().print("{}\n", .{result});
}

const Operation = struct {
    first: ?u64,
    operand: u8,
    second: ?u64,
    fn init(line: []const u8, allocator: std.mem.Allocator) !*Operation {
        var o = try allocator.create(Operation);
        var it = std.mem.tokenize(u8, line, " ");
        _ = it.next() orelse unreachable; // Operation:
        _ = it.next() orelse unreachable; // new
        _ = it.next() orelse unreachable; // =
        o.first = std.fmt.parseInt(u64, it.next() orelse unreachable, 10) catch null;
        const tmp = it.next() orelse unreachable;
        o.operand = tmp[0];
        o.second = std.fmt.parseInt(u64, it.next() orelse unreachable, 10) catch null;
        return o;
    }
    fn run(o: Operation, v: u64) u64 {
        const first = if (o.first) |f| f else v;
        const second = if (o.second) |f| f else v;
        switch (o.operand) {
            '+' => return first + second,
            '-' => return first - second,
            '*' => return first * second,
            '/' => return first / second,
            else => unreachable,
        }
    }
};

const Monkey = struct {
    inspections: u64,
    items: std.ArrayList(u64),
    operation: *Operation,
    divisible: u64, // divisible by
    testTrue: u64,
    testFalse: u64,
    fn init(it: *std.mem.TokenIterator(u8), allocator: std.mem.Allocator) !*Monkey {
        var m = try allocator.create(Monkey);
        m.inspections = 0;
        m.items = std.ArrayList(u64).init(allocator);
        var items = std.mem.tokenize(u8, it.next() orelse unreachable, " ,");
        _ = items.next() orelse unreachable; // starting
        _ = items.next() orelse unreachable; // items:
        while (items.next()) |item| {
            try m.items.append(std.fmt.parseInt(u64, item, 10) catch unreachable);
        }
        m.operation = try Operation.init(it.next() orelse unreachable, allocator);
        var divisible = std.mem.tokenize(u8, it.next() orelse unreachable, " ");
        _ = divisible.next() orelse unreachable; // test:
        _ = divisible.next() orelse unreachable; // divisible
        _ = divisible.next() orelse unreachable; // by
        m.divisible = std.fmt.parseInt(u64, divisible.next() orelse unreachable, 10) catch unreachable;
        var testTrue = std.mem.tokenize(u8, it.next() orelse unreachable, " ");
        _ = testTrue.next() orelse unreachable; // if
        _ = testTrue.next() orelse unreachable; // true:
        _ = testTrue.next() orelse unreachable; // throw
        _ = testTrue.next() orelse unreachable; // to
        _ = testTrue.next() orelse unreachable; // monkey
        m.testTrue = std.fmt.parseInt(u64, testTrue.next() orelse unreachable, 10) catch unreachable;
        var testFalse = std.mem.tokenize(u8, it.next() orelse unreachable, " ");
        _ = testFalse.next() orelse unreachable; // if
        _ = testFalse.next() orelse unreachable; // true:
        _ = testFalse.next() orelse unreachable; // throw
        _ = testFalse.next() orelse unreachable; // to
        _ = testFalse.next() orelse unreachable; // monkey
        m.testFalse = std.fmt.parseInt(u64, testFalse.next() orelse unreachable, 10) catch unreachable;
        return m;
    }
    fn step(m: *Monkey, monkeys: []*Monkey) !void {
        m.inspections += m.items.items.len;
        for (m.items.items) |item| {
            const v = m.operation.run(item) / 3;
            if (@mod(v, m.divisible) == 0) {
                try monkeys[m.testTrue].items.append(v);
            } else {
                try monkeys[m.testFalse].items.append(v);
            }
        }
        m.items.items.len = 0;
    }
};

fn solve(puzzle: []const u8, allocator: std.mem.Allocator) !u64 {
    var it = std.mem.tokenize(u8, puzzle, "\n");
    var monkeys = std.ArrayList(*Monkey).init(allocator);
    // process input
    while (it.next()) |_| {
        try monkeys.append(try Monkey.init(&it, allocator));
    }
    // run 20 rounds
    var rounds: usize = 0;
    while (rounds < 20) : (rounds += 1) {
        for (monkeys.items) |m| {
            try m.step(monkeys.items);
        }
    }
    // find answer
    var m1: u64 = 0;
    var m2: u64 = 0;
    for (monkeys.items) |m| {
        if (m.inspections > m1) {
            m2 = m1;
            m1 = m.inspections;
            continue;
        }
        if (m.inspections > m2) {
            m2 = m.inspections;
        }
    }
    return m1 * m2;
}
