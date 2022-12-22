const std = @import("std");

const example = @embedFile("example");
const input = @embedFile("input");

var allocator: std.mem.Allocator = undefined;

pub fn main() anyerror!void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    allocator = arena.allocator();

    try std.testing.expectEqual(try solve(example), 24);
    const result = try solve(input);
    try std.io.getStdOut().writer().print("{}\n", .{result});
}

const Line = struct {
    data: std.ArrayList(u8),
    x: u64,
    fn get(l: *Line, x: u64) u8 {
        if (x >= l.x and x < l.x + @intCast(u64, l.len())) return l.data.items[@intCast(usize, x - @intCast(u64, l.x))];
        return ' ';
    }
    fn init() !*Line {
        var l = try allocator.create(Line);
        l.data = std.ArrayList(u8).init(allocator);
        return l;
    }
    inline fn len(l: Line) usize {
        return l.data.items.len;
    }
    fn set(l: *Line, x: u64, v: u8) !void {
        if (l.len() == 0) { // this is en empty line
            l.x = x;
            try l.data.append(v);
            return;
        }
        const lx = @intCast(u64, l.len());
        if (x >= l.x) {
            if (x < l.x + lx) { // just set the value
                l.data.items[@intCast(usize, x - l.x)] = v;
            } else { // we need to add trailing spaces
                var i: usize = l.len();
                while (i < x - l.x) : (i += 1) {
                    try l.data.append(' ');
                }
                try l.data.append(v);
            }
        } else { // we need to shift right and add leading spaces
            const oldLen = l.len();
            l.data.items.len += @intCast(usize, l.x - x);
            try l.data.ensureUnusedCapacity(l.len());
            std.mem.copyBackwards(u8, l.data.items[@intCast(usize, l.x - x)..], l.data.items[0..oldLen]);
            l.data.items[0] = v;
            var i: usize = 1;
            while (i < @intCast(usize, l.x - x)) : (i += 1) {
                l.data.items[i] = ' ';
            }
            l.x = x;
        }
    }
};

pub const Field = struct {
    x: u64 = 0,
    y: u64 = 0,
    lines: std.ArrayList(*Line),
    lx: usize = 0,
    pub fn get(f: *Field, x: u64, y: u64) u8 {
        if (y >= f.y and y < f.y + @intCast(u64, f.lines.items.len)) return f.lines.items[@intCast(usize, y - @intCast(u64, f.y))].get(x);
        return ' ';
    }
    fn init() !*Field {
        var f = try allocator.create(Field);
        f.x = undefined;
        f.y = 0;
        f.lines = std.ArrayList(*Line).init(allocator);
        var l = try f.lines.addOne();
        l.* = try Line.init();
        f.lx = 0;
        return f;
    }
    inline fn isIn(f: *Field, x: u64, y: u64) bool {
        return x >= f.x and y >= f.y and x < f.x + @intCast(u64, f.lx) and y < f.y + @intCast(u64, f.lines.items.len);
    }
    inline fn len(f: Field) usize {
        return f.lines.items.len;
    }
    pub fn set(f: *Field, x: u64, y: u64, v: u8) !void {
        if (y >= f.y) {
            if (y < f.y + @intCast(u64, f.lines.items.len)) { // the line exists
                try f.lines.items[@intCast(usize, y - f.y)].set(x, v);
            } else { // append lines
                var i: usize = f.lines.items.len;
                while (i < y - f.y) : (i += 1) {
                    try f.lines.append(try Line.init());
                }
                var l = try Line.init();
                try l.set(x, v);
                try f.lines.append(l);
            }
        } else { // preprend lines
            const oldLen = f.lines.items.len;
            f.lines.items.len += @intCast(usize, f.y - y);
            try f.lines.ensureUnusedCapacity(f.lines.items.len);
            std.mem.copyBackwards(*Line, f.lines.items[@intCast(usize, f.y - y)..], f.lines.items[0..oldLen]);
            var l = try Line.init();
            try l.set(x, v);
            f.lines.items[0] = l;
            var i: usize = 1;
            while (i < @intCast(usize, f.y - y)) : (i += 1) {
                f.lines.items[i] = try Line.init();
            }
            f.y = y;
        }
        if (x < f.x or x >= f.x + @intCast(u64, f.lx)) { // recalculate boundaries
            f.x = std.math.maxInt(u64);
            var x2: u64 = std.math.minInt(u64);
            for (f.lines.items) |line| {
                if (line.len() == 0) continue;
                if (f.x > line.x) f.x = line.x;
                if (x2 < line.x + @intCast(u64, line.len())) x2 = line.x + @intCast(u64, line.len());
            }
            f.lx = @intCast(usize, x2 - f.x);
        }
        return;
    }
};

fn solve(puzzle: []const u8) !u64 {
    var it = std.mem.tokenize(u8, puzzle, "\n");
    var field = try Field.init();
    // process input
    while (it.next()) |line| {
        var coords = std.mem.tokenize(u8, line, " ->,");
        var x = try std.fmt.parseInt(u64, coords.next() orelse continue, 10);
        var y = try std.fmt.parseInt(u64, coords.next() orelse unreachable, 10);
        try field.set(x, y, '#');
        while (true) {
            var a = try std.fmt.parseInt(u64, coords.next() orelse break, 10);
            var b = try std.fmt.parseInt(u64, coords.next() orelse unreachable, 10);

            while (x != a or y != b) {
                if (x < a) {
                    x += 1;
                } else if (x > a) {
                    x -= 1;
                }
                if (y < b) {
                    y += 1;
                } else if (y > b) {
                    y -= 1;
                }
                try field.set(x, y, '#');
            }
        }
    }
    // run simulation
    var i: usize = 0;
    outer: while (true) : (i += 1) { // add sand forever
        var x: u64 = 500;
        var y: u64 = field.y;
        while (true) { // let the sand fall
            if (!field.isIn(x, y)) {
                break :outer;
            }
            switch (field.get(x, y + 1)) {
                ' ' => y += 1, // we can fall straight down
                else => {
                    switch (field.get(x - 1, y + 1)) {
                        ' ' => { // we can fall down to the left
                            x -= 1;
                            y += 1;
                        },
                        else => {
                            switch (field.get(x + 1, y + 1)) {
                                ' ' => { // we can fall down to the right
                                    x += 1;
                                    y += 1;
                                },
                                else => { // we stop falling
                                    try field.set(x, y, 'o');
                                    break;
                                },
                            }
                        },
                    }
                },
            }
        }
    }
    return i;
}
