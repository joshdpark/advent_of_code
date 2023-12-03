const std = @import("std");
const print = std.debug.print;
const expect = std.testing.expect;

fn parse_tens(str: [2]u8) u32 {
    // print("strlen: {d}\n", .{str.len});
    var a: u32 = 0;
    for (str, 0..) |c, i| {
        a += c - '0';
        if (i + 1 == str.len)
            break;
        a *= 10;
    }
    return a;
}

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

inline fn swap(x: *u8, y: *u8) void {
    var t = x.*;
    x.* = y.*;
    y.* = t;
}

fn reverse(str: []u8) void {
    // swap the first and last element
    var i: u32 = 0;
    while (i < str.len / 2) : (i += 1) {
        swap(&str[i], &str[str.len - 1 - i]);
    }
}

const Parser = struct {
    str: []u8,
    current: usize,

    const Digit = enum {
        One,
        Two,
        Three,
        Four,
        Five,
        Six,
        Seven,
        Eight,
        Nine
    }
};

fn digits(str: []u8) u32 {
    // make ints a struct with data
    // const parsed = struct {
    //     data: u8,
    //     end: u8,
    // };
    // var ints = [_]u8{0} ** 64; // unlikely that there are more than 64 ints in a line
    // var ints_end: u8 = 0;
    var current: u32 = 0;
    while (current < str.len) {
        var c = str[current];
        // print("{c}\n", .{c});
        if (c <= '9' and c >= '0') {
            // ints[ints_end] = c;
            // ints_end += 1;
            // current += 1;
            continue;
        }
        var ahead = str[current + 1 ..];
        // if match, increment the current index and the ints index
        switch (c) {
            'o' => if (match(ahead, "ne")) {
                ints[ints_end] = '1';
                current += 3;
                ints_end += 1;
                continue;
            },
            't' => if (match(ahead, "wo")) {
                ints[ints_end] = '2';
                current += 3;
                ints_end += 1;
                continue;
            } else if (match(ahead, "hree")) {
                ints[ints_end] = '3';
                current += 5;
                ints_end += 1;
                continue;
            },
            'f' => if (match(ahead, "our")) {
                ints[ints_end] = '4';
                current += 4;
                ints_end += 1;
                continue;
            } else if (match(ahead, "ive")) {
                ints[ints_end] = '5';
                current += 4;
                ints_end += 1;
                continue;
            },
            's' => if (match(ahead, "ix")) {
                ints[ints_end] = '6';
                current += 3;
                ints_end += 1;
                continue;
            } else if (match(ahead, "even")) {
                ints[ints_end] = '7';
                current += 5;
                ints_end += 1;
                continue;
            },
            'e' => if (match(ahead, "ight")) {
                ints[ints_end] = '8';
                current += 5;
                ints_end += 1;
                continue;
            },
            'n' => if (match(ahead, "ine")) {
                ints[ints_end] = '9';
                current += 4;
                ints_end += 1;
                continue;
            },
            else => {},
        }
        current += 1;
    }
    var result: [2]u8 = undefined;
    result[0] = ints[0];
    result[1] = if (ints[0..ints_end].len > 1) ints[ints_end - 1] else ints[0];
    print("{c}\n", .{result});
    return parse_tens(result);
}

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
        result += digits(list.items);
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

test digits {
    try expect(digits("9963onefourthree6oneightq") == 98);
    try expect(digits("4nineeightseven2") == 42);
    try expect(digits("3four") == 34);
    try expect(digits("pqr3stu8vwx") == 38);
    try expect(digits("treb7uchet") == 77);
    try expect(digits("a1b2c3d4e5f") == 15);
    try expect(digits("7pqrstsixteen") == 76);
    try expect(digits("two1nine") == 29);
    try expect(digits("eightwothree") == 83);
    try expect(digits("abcone2threexyz") == 13);
    try expect(digits("xgfrrnrlkgdqfxdtwo9fvthree") == 23);
}
