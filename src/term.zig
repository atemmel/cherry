const std = @import("std");

const fs = std.fs;
const posix = std.posix;

pub const Color = struct {
    pub const black = "\x1b[0;30m";
    pub const red = "\x1b[0;31m";
    pub const green = "\x1b[0;32m";
    pub const yellow = "\x1b[0;33m";
    pub const blue = "\x1b[0;34m";
    pub const purple = "\x1b[0;35m";
    pub const cyan = "\x1b[0;36m";
    pub const white = "\x1b[0;37m";
    pub const gray = "\x1b[0;38m";
};

pub const Hi = struct {
    pub const black = "\x1b[0;90m";
    pub const red = "\x1b[0;91m";
    pub const green = "\x1b[0;92m";
    pub const yellow = "\x1b[0;93m";
    pub const blue = "\x1b[0;94m";
    pub const purple = "\x1b[0;95m";
    pub const cyan = "\x1b[0;96m";
    pub const white = "\x1b[0;97m";
    pub const gray = "\x1b[0;98m";
};

pub const BoldHi = struct {
    pub const black = "\x1b[1;90m";
    pub const red = "\x1b[1;91m";
    pub const green = "\x1b[1;92m";
    pub const yellow = "\x1b[1;93m";
    pub const blue = "\x1b[1;94m";
    pub const purple = "\x1b[1;95m";
    pub const cyan = "\x1b[1;96m";
    pub const white = "\x1b[1;97m";
    pub const gray = "\x1b[1;98m";
};

// shoutout to big man over at:
// https://zig.news/lhp/want-to-create-a-tui-application-the-basics-of-uncooked-terminal-io-17gm

pub const Vec2 = struct {
    x: u16,
    y: u16,
};

pub const Term = struct {
    tty: fs.File,
    original_termios: posix.termios,
    state: posix.termios,

    pub const Event = union(enum) {
        key: u8,
        ctrl: u8,
        alt: u8,
        escape: void,
        arrow_down: void,
        arrow_left: void,
        arrow_right: void,
        arrow_up: void,
        backspace: void,
        delete: void,
        end: void,
        home: void,
        insert: void,
        page_down: void,
        page_up: void,
        unknown: void,
        tab: void,
    };

    pub fn init() !Term {
        var tty = try fs.cwd().openFile("/dev/tty", .{ .mode = .read_write });
        errdefer tty.close();
        const original_termios = try posix.tcgetattr(tty.handle);
        var state = original_termios;

        //   ECHO: Stop the terminal from displaying pressed keys.
        // ICANON: Disable canonical ("cooked") input mode. Allows us to read inputs
        //         byte-wise instead of line-wise.
        //   ISIG: Disable signals for Ctrl-C (SIGINT) and Ctrl-Z (SIGTSTP), so we
        //         can handle them as "normal" escape sequences.
        // IEXTEN: Disable input preprocessing. This allows us to handle Ctrl-V,
        //         which would otherwise be intercepted by some terminals.

        state.lflag = .{
            .ECHO = false,
            .ICANON = false,
            .ISIG = false,
            .IEXTEN = false,
        };

        //   IXON: Disable software control flow. This allows us to handle Ctrl-S
        //         and Ctrl-Q.
        //  ICRNL: Disable converting carriage returns to newlines. Allows us to
        //         handle Ctrl-J and Ctrl-M.
        // BRKINT: Disable converting sending SIGINT on break conditions. Likely has
        //         no effect on anything remotely modern.
        //  INPCK: Disable parity checking. Likely has no effect on anything
        //         remotely modern.
        // ISTRIP: Disable stripping the 8th bit of characters. Likely has no effect
        //         on anything remotely modern.

        state.iflag = .{
            .IXON = false,
            .ICRNL = false,
            .BRKINT = false,
            .INPCK = false,
            .ISTRIP = false,
        };

        // Disable output processing. Common output processing includes prefixing
        // newline with a carriage return.

        state.oflag = .{
            .OPOST = false,
        };

        // Set the character size to 8 bits per byte. Likely has no efffect on
        // anything remotely modern.
        state.cflag = .{
            .CSIZE = .CS8,
        };

        state.cc[@intFromEnum(posix.system.V.TIME)] = 0;
        state.cc[@intFromEnum(posix.system.V.MIN)] = 1;

        try posix.tcsetattr(tty.handle, .FLUSH, state);

        return Term{
            .tty = tty,
            .original_termios = original_termios,
            .state = state,
        };
    }

    pub fn restore(self: *Term) !void {
        try posix.tcsetattr(self.tty.handle, .FLUSH, self.original_termios);
        self.tty.close();
        self.tty = fs.File{ .handle = 0 };
    }

    pub fn readByte(self: Term) !u8 {
        var buffer: [1]u8 = undefined;
        _ = try self.tty.read(&buffer);
        return buffer[0];
    }

    pub fn getCursor(self: Term) !Vec2 {
        try self.tty.writeAll("\x1b[6n");
        var buf: ["\x1b[256;256R".len]u8 = undefined;
        const output = try self.tty.reader().readUntilDelimiter(&buf, 'R');
        var splitter = std.mem.splitAny(u8, output, ";[");
        _ = splitter.next().?;
        const row_half = splitter.next() orelse return error.UnexpectedEnd;
        const column_half = splitter.next() orelse return error.UnexpectedEnd;
        const row = try std.fmt.parseUnsigned(u16, row_half, 10);
        const column = try std.fmt.parseUnsigned(u16, column_half, 10);
        return .{
            .x = column - 1, // it's one-based
            .y = row - 1,
        };
    }

    pub fn size(self: Term) Vec2 {
        const linux = std.os.linux;
        var l_size = std.mem.zeroes(std.posix.winsize);
        const err = linux.ioctl(self.tty.handle, linux.T.IOCGWINSZ, @intFromPtr(&l_size));
        if (posix.errno(err) != .SUCCESS) {
            unreachable;
        }
        return .{
            .x = l_size.row,
            .y = l_size.col,
        };
    }

    pub fn readEvent(self: *Term) !Event {
        const byte = try self.readByte();
        return switch (byte) {
            '\x1b' => res: {
                self.state.cc[@intFromEnum(posix.system.V.TIME)] = 1;
                self.state.cc[@intFromEnum(posix.system.V.MIN)] = 0;
                try posix.tcsetattr(self.tty.handle, .NOW, self.state);

                var esc_buffer: [8]u8 = undefined;
                const esc_read = try self.tty.read(&esc_buffer);

                self.state.cc[@intFromEnum(posix.system.V.TIME)] = 0;
                self.state.cc[@intFromEnum(posix.system.V.MIN)] = 1;
                try posix.tcsetattr(self.tty.handle, .NOW, self.state);

                if (esc_read == 0) {
                    break :res .escape;
                }
                break :res escape_key_codes.get(esc_buffer[0..esc_read]) orelse .unknown;
            },
            'a' & '\x1F' => Event{ .ctrl = 'a' },
            'b' & '\x1F' => Event{ .ctrl = 'b' },
            'c' & '\x1F' => Event{ .ctrl = 'c' },
            'd' & '\x1F' => Event{ .ctrl = 'd' },
            'e' & '\x1F' => Event{ .ctrl = 'e' },
            'f' & '\x1F' => Event{ .ctrl = 'f' },
            'g' & '\x1F' => Event{ .ctrl = 'g' },
            'h' & '\x1F' => Event{ .ctrl = 'h' },
            // Duplicate
            //'i' & '\x1F' => Event{ .ctrl = 'i' },
            // Duplicate
            //'j' & '\x1F' => Event{ .ctrl = 'j' },
            'k' & '\x1F' => Event{ .ctrl = 'k' },
            'l' & '\x1F' => Event{ .ctrl = 'l' },
            // Duplicate
            //'m' & '\x1F' => Event{ .ctrl = 'm' },
            'n' & '\x1F' => Event{ .ctrl = 'n' },
            'o' & '\x1F' => Event{ .ctrl = 'o' },
            'p' & '\x1F' => Event{ .ctrl = 'p' },
            'q' & '\x1F' => Event{ .ctrl = 'q' },
            'r' & '\x1F' => Event{ .ctrl = 'r' },
            's' & '\x1F' => Event{ .ctrl = 's' },
            't' & '\x1F' => Event{ .ctrl = 't' },
            'u' & '\x1F' => Event{ .ctrl = 'u' },
            'v' & '\x1F' => Event{ .ctrl = 'v' },
            'w' & '\x1F' => Event{ .ctrl = 'w' },
            'x' & '\x1F' => Event{ .ctrl = 'x' },
            'y' & '\x1F' => Event{ .ctrl = 'y' },
            'z' & '\x1F' => Event{ .ctrl = 'z' },
            9 => .tab,
            127 => .backspace,
            else => Event{ .key = byte },
        };
    }
};

/// The various escape sequences that represent special keys.
const escape_key_codes = blk: {
    @setEvalBranchQuota(5000);
    break :blk std.StaticStringMap(Term.Event).initComptime(
        .{
            // Legacy
            .{ "[A", .arrow_up },
            .{ "OA", .arrow_up },
            .{ "[B", .arrow_down },
            .{ "OB", .arrow_down },
            .{ "[C", .arrow_right },
            .{ "OC", .arrow_right },
            .{ "[D", .arrow_left },
            .{ "OD", .arrow_left },
            .{ "[2~", .insert },
            .{ "[3~", .delete },
            .{ "[5~", .page_up },
            .{ "[6~", .page_down },
            .{ "[F", .end },
            .{ "OF", .end },
            .{ "[4~", .home },
            .{ "[8~", .home },
            .{ "[H", .home },
            .{ "[1~", .home },
            .{ "[7~", .home },
            .{ "[H~", .home },
            //.{ "OP", .{ .function = 1 } },
            //.{ "OQ", .{ .function = 2 } },
            //.{ "OR", .{ .function = 3 } },
            //.{ "OS", .{ .function = 4 } },
            //.{ "[15~", .{ .function = 5 } },
            //.{ "[17~", .{ .function = 6 } },
            //.{ "[18~", .{ .function = 7 } },
            //.{ "[19~", .{ .function = 8 } },
            //.{ "[20~", .{ .function = 9 } },
            //.{ "[21~", .{ .function = 10 } },
            //.{ "[23~", .{ .function = 11 } },
            //.{ "[24~", .{ .function = 12 } },
            //.{ "a", .{ .alt = 'a' } },
            .{ "b", Term.Event{ .alt = 'b' } },
            //.{ "c", .{ .alt = 'c' } },
            //.{ "d", .{ .alt = 'd' } },
            //.{ "e", .{ .alt = 'e' } },
            .{ "f", Term.Event{ .alt = 'f' } },
            //.{ "g", .{ .alt = 'g' } },
            //.{ "h", .{ .alt = 'h' } },
            //.{ "i", .{ .alt = 'i' } },
            //.{ "j", .{ .alt = 'j' } },
            //.{ "k", .{ .alt = 'k' } },
            //.{ "l", .{ .alt = 'l' } },
            //.{ "m", .{ .alt = 'm' } },
            //.{ "n", .{ .alt = 'n' } },
            //.{ "o", .{ .alt = 'o' } },
            //.{ "p", .{ .alt = 'p' } },
            //.{ "q", .{ .alt = 'q' } },
            //.{ "r", .{ .alt = 'r' } },
            //.{ "s", .{ .alt = 's' } },
            //.{ "t", .{ .alt = 't' } },
            //.{ "u", .{ .alt = 'u' } },
            //.{ "v", .{ .alt = 'v' } },
            //.{ "w", .{ .alt = 'w' } },
            //.{ "x", .{ .alt = 'x' } },
            //.{ "y", .{ .alt = 'y' } },
            //.{ "z", .{ .alt = 'z' } },

            // Kitty
            .{ "[27u", .escape },
            .{ "[97;5u", Term.Event{ .ctrl = 'a' } },
            .{ "[98;5u", Term.Event{ .ctrl = 'b' } },
            .{ "[99;5u", Term.Event{ .ctrl = 'c' } },
            .{ "[100;5u", Term.Event{ .ctrl = 'd' } },
            .{ "[101;5u", Term.Event{ .ctrl = 'e' } },
            .{ "[102;5u", Term.Event{ .ctrl = 'f' } },
            .{ "[103;5u", Term.Event{ .ctrl = 'g' } },
            .{ "[104;5u", Term.Event{ .ctrl = 'h' } },
            .{ "[105;5u", Term.Event{ .ctrl = 'i' } },
            .{ "[106;5u", Term.Event{ .ctrl = 'j' } },
            .{ "[107;5u", Term.Event{ .ctrl = 'k' } },
            .{ "[108;5u", Term.Event{ .ctrl = 'l' } },
            .{ "[109;5u", Term.Event{ .ctrl = 'm' } },
            .{ "[110;5u", Term.Event{ .ctrl = 'n' } },
            .{ "[111;5u", Term.Event{ .ctrl = 'o' } },
            .{ "[112;5u", Term.Event{ .ctrl = 'p' } },
            .{ "[113;5u", Term.Event{ .ctrl = 'q' } },
            .{ "[114;5u", Term.Event{ .ctrl = 'r' } },
            .{ "[115;5u", Term.Event{ .ctrl = 's' } },
            .{ "[116;5u", Term.Event{ .ctrl = 't' } },
            .{ "[117;5u", Term.Event{ .ctrl = 'u' } },
            .{ "[118;5u", Term.Event{ .ctrl = 'v' } },
            .{ "[119;5u", Term.Event{ .ctrl = 'w' } },
            .{ "[120;5u", Term.Event{ .ctrl = 'x' } },
            .{ "[121;5u", Term.Event{ .ctrl = 'y' } },
            .{ "[122;5u", Term.Event{ .ctrl = 'z' } },
            .{ "[97;3u", Term.Event{ .alt = 'a' } },
            .{ "[98;3u", Term.Event{ .alt = 'b' } },
            .{ "[99;3u", Term.Event{ .alt = 'c' } },
            .{ "[100;3u", Term.Event{ .alt = 'd' } },
            .{ "[101;3u", Term.Event{ .alt = 'e' } },
            .{ "[102;3u", Term.Event{ .alt = 'f' } },
            .{ "[103;3u", Term.Event{ .alt = 'g' } },
            .{ "[104;3u", Term.Event{ .alt = 'h' } },
            .{ "[105;3u", Term.Event{ .alt = 'i' } },
            .{ "[106;3u", Term.Event{ .alt = 'j' } },
            .{ "[107;3u", Term.Event{ .alt = 'k' } },
            .{ "[108;3u", Term.Event{ .alt = 'l' } },
            .{ "[109;3u", Term.Event{ .alt = 'm' } },
            .{ "[110;3u", Term.Event{ .alt = 'n' } },
            .{ "[111;3u", Term.Event{ .alt = 'o' } },
            .{ "[112;3u", Term.Event{ .alt = 'p' } },
            .{ "[113;3u", Term.Event{ .alt = 'q' } },
            .{ "[114;3u", Term.Event{ .alt = 'r' } },
            .{ "[115;3u", Term.Event{ .alt = 's' } },
            .{ "[116;3u", Term.Event{ .alt = 't' } },
            .{ "[117;3u", Term.Event{ .alt = 'u' } },
            .{ "[118;3u", Term.Event{ .alt = 'v' } },
            .{ "[119;3u", Term.Event{ .alt = 'w' } },
            .{ "[120;3u", Term.Event{ .alt = 'x' } },
            .{ "[121;3u", Term.Event{ .alt = 'y' } },
            .{ "[122;3u", Term.Event{ .alt = 'z' } },

            // Other
            .{ "[1;5C", Term.Event{ .ctrl = 0 } },
            .{ "[1;5D", Term.Event{ .ctrl = 1 } },
        },
    );
};

pub fn clearLine(writer: anytype, len: usize) void {
    _ = writer.print("\r", .{}) catch unreachable;
    for (0..len) |_| {
        _ = writer.print(" ", .{}) catch unreachable;
    }
    _ = writer.print("\r", .{}) catch unreachable;
}

pub fn moveCursor(writer: anytype, row: usize, col: usize) void {
    _ = writer.print("\x1B[{};{}H", .{ row + 1, col + 1 }) catch unreachable;
}

pub fn moveUp(writer: anytype, steps: usize) void {
    _ = writer.print("\x1B[{}A", .{steps}) catch unreachable;
}

pub fn moveDown(writer: anytype, steps: usize) void {
    _ = writer.print("\x1B[{}B", .{steps}) catch unreachable;
}

pub fn moveRight(writer: anytype, steps: usize) void {
    _ = writer.print("\x1B[{}C", .{steps}) catch unreachable;
}

pub fn moveLeft(writer: anytype, steps: usize) void {
    _ = writer.print("\x1B[{}D", .{steps}) catch unreachable;
}

pub fn clear(writer: anytype) void {
    _ = writer.print("\x1B[3J\x1B[2J", .{}) catch unreachable;
}

pub fn clearRect(writer: anytype, pos: Vec2, rect: Vec2) void {
    for (0..rect.y) |y| {
        moveCursor(writer, pos.x, pos.y + y);
        for (0..rect.x) |_| {
            writer.writeByte(' ');
        }
    }
}
