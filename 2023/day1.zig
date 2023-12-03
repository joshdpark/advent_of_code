const std = @import("std");
const print = std.debug.print;
const expect = std.testing.expect;

/// returns a u8 because we're not going to advance more than 256 bytes
fn match(str: []const u8, pattern: []const u8) bool {
    var i: u8 = 0;
    if (pattern.len > str.len)
        return false; // the patten is longer than the input string
    while (str[i] == pattern[i]) : (i += 1) {
        if (i + 1 == pattern.len)
            return true; // the bytes needed to advance
    }
    return false;
}

const Parser = struct {
    str: []const u8,
    current: usize = 0,

    const Digit = enum { One, Two, Three, Four, Five, Six, Seven, Eight, Nine };

    fn parse(self: *Parser) ?Digit {
        const ahead = self.str[self.current + 1 ..];
        const c = self.str[self.current];
        return switch (c) {
            '1' => .One,
            '2' => .Two,
            '3' => .Three,
            '4' => .Four,
            '5' => .Five,
            '6' => .Six,
            '7' => .Seven,
            '8' => .Eight,
            '9' => .Nine,
            'o' => if (match(ahead, "ne")) .One else null,
            't' => if (match(ahead, "wo")) .Two else if (match(ahead, "hree")) .Three else null,
            'f' => if (match(ahead, "our")) .Four else if (match(ahead, "ive")) .Five else null,
            's' => if (match(ahead, "ix")) .Six else if (match(ahead, "even")) .Seven else null,
            'e' => if (match(ahead, "ight")) .Eight else null,
            'n' => if (match(ahead, "ine")) .Nine else null,
            else => null,
        };
    }

    fn parseForward(self: *Parser) !Digit {
        self.current = 0;
        while (self.current < self.str.len) {
            if (self.parse()) |token|
                return token
            else
                self.current += 1;
        }
        return error.parseError;
    }

    fn parseBackward(self: *Parser) !Digit {
        self.current = self.str.len - 1;
        while (self.current >= 0) {
            if (self.parse()) |token|
                return token
            else
                self.current -= 1;
        }
        return error.parseError;
    }

    fn digitToInt(t: Digit) u32 {
        return switch (t) {
            .One => 1,
            .Two => 2,
            .Three => 3,
            .Four => 4,
            .Five => 5,
            .Six => 6,
            .Seven => 7,
            .Eight => 8,
            .Nine => 9,
        };
    }

    fn eval(self: *Parser) !u32 {
        const left = try self.parseForward();
        const right = try self.parseBackward();
        return digitToInt(left) * 10 + digitToInt(right);
    }
};

pub fn main() !void {

    // file setup
    const filename = "day1.txt";
    const file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();
    var buf_reader = std.io.bufferedReader(file.reader());
    const stream = buf_reader.reader();

    // memory setup
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    var list = std.ArrayList(u8).init(allocator);
    defer list.deinit();

    var result: u32 = 0;
    while (stream.streamUntilDelimiter(list.writer(), '\n', null)) {
        // std.debug.print("{s}\n", .{list.items});
        var p = Parser{ .str = list.items };
        if (p.eval()) |digit| {
            result += digit;
        } else |err| {
            return err;
        }
        list.clearRetainingCapacity();
    } else |_| {
        print("{d}", .{result});
        return;
    }
}

test parse_tens {
    try expect(parse_tens(.{ '7', '4' }) == 74);
    try expect(parse_tens(.{ '1', '0' }) == 10);
}

test match {
    try expect(match("helloworld!", "hello"));
    try expect(match("eleven", "eleven"));
    try expect(!match("two", "twi"));
    try expect(!match("sev", "seven"));
}

test swap {
    var a: u8 = 'a';
    var b: u8 = 'z';
    swap(&a, &b);
    try expect(a == 'z' and b == 'a');
}

test reverse {
    var str = "reverse this".*;
    reverse(str[0..]);
    print("{s}\n", .{str});
}

test Parser {
    var str = "two";
    var parser = Parser{ .str = str[0..] };
    try expect(parser.parse() == Parser.Digit.Two);
}

test "parsing forward" {
    var p = Parser{ .str = "t9963onefourthree6oneightq" };
    print("{any}\n", .{p.eval()});
}
