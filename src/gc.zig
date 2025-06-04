const std = @import("std");
const builtin = @import("builtin");
const ast = @import("ast.zig");
const values = @import("value.zig");
const tokens = @import("tokens.zig");
const pipeline = @import("pipeline.zig");
const InterpreterError = @import("interpreter.zig").InterpreterError;
const Value = @import("value.zig").Value;

pub const Roots = std.ArrayList(*Value);
pub const Symtable = std.StringHashMap(*Value);

const Environment = union(enum) {
    symtable: Symtable,
    closure: *values.Closure,
};

const Frame = struct {
    root_values: Roots,
    //envronment: Environment, TODO: rework this
    symtable: Symtable,

    pub fn dumpSymtable(self: Frame) void {
        var it = self.symtable.iterator();
        while (it.next()) |entry| {
            std.debug.print("{s} - {any} ({*})\n", .{ entry.key_ptr.*, entry.value_ptr.*, entry.value_ptr.* });
        }
    }
};

// bookkeeping code for user-defined aliases lives here so as to be reachable by the interpreter
// in reality, this would perhaps be more appropriate to be 'owned' by the repl
pub const Alias = struct {
    from: []const u8,
    to: []const u8,
};

const GcList = std.ArrayList(*values.Value);
const ProgramStack = std.ArrayList(Frame);
const Aliases = std.ArrayList(Alias);

var gc_list: GcList = undefined;
var program_stack: ProgramStack = undefined;
var persistent_allocator: std.mem.Allocator = undefined;
var n_allocs: usize = 0;
var allocs_until_collect: usize = 800;
var total_collections: usize = 0;
pub var always_collect = false; // used to stress the gc, if enabled will perform a collection for every requested allocation

// by allocating two booleans on startup, the gc can skip allocating future booleans by
// always refering to either of them whenever necessary
// the booleans are never gc:ed, instead manually deleted on deinit
// however, using fast booleans prevents the gc from tracing the origins of boolean values
const fast_booleans = builtin.mode != .Debug;
var true_value: *Value = undefined;
var false_value: *Value = undefined;

pub var aliases: Aliases = undefined; // exposed for convenience

pub fn init(ally: std.mem.Allocator) !void {
    gc_list = try GcList.initCapacity(ally, 128);
    program_stack = try ProgramStack.initCapacity(ally, 16);
    aliases = try Aliases.initCapacity(ally, 16);

    persistent_allocator = ally;

    if (fast_booleans) {
        true_value = try persistent_allocator.create(values.Value);
        false_value = try persistent_allocator.create(values.Value);

        true_value.* = .{
            .origin_module = undefined,
            .origin = undefined,
            .as = .{
                .boolean = true,
            },
            .marked = false,
        };

        false_value.* = .{
            .origin_module = undefined,
            .origin = undefined,
            .as = .{
                .boolean = false,
            },
            .marked = false,
        };
    }
}

pub fn deinit() void {
    pipeline.state.env_map.deinit();
    defer aliases.deinit();
    defer gc_list.deinit();
    defer program_stack.deinit();
    for (program_stack.items) |*frame| {
        deinitFrame(frame);
    }
    for (gc_list.items) |ptr| {
        ptr.deinit(persistent_allocator);
        persistent_allocator.destroy(ptr);
    }

    if (fast_booleans) {
        persistent_allocator.destroy(false_value);
        persistent_allocator.destroy(true_value);
    }
}

fn deinitFrame(frame: *Frame) void {
    var it = frame.symtable.keyIterator();
    while (it.next()) |key| {
        persistent_allocator.free(key.*);
    }
    frame.symtable.deinit();
    frame.root_values.deinit();
}

pub fn allocator() std.mem.Allocator {
    return persistent_allocator;
}

pub fn dump() void {
    switch (builtin.mode) {
        .Debug => {
            std.debug.print("Begin GC dump\n", .{});
            for (gc_list.items) |value| {
                std.debug.print("{*} {any} {s}\n", .{ value, value.origin.kind, value.origin.value });
                pipeline.state.dumpSourceAtToken(value.origin, value.origin_module);
            }
        },
        else => {
            std.debug.print("GC dumps are disabled in release builds, as no garbage tracing is done.", .{});
        },
    }
}

fn get(idx: usize) *values.Value {
    return gc_list.items[idx];
}

fn sweep() void {
    var i: usize = 0;
    while (i < gc_list.items.len) {
        const ptr = get(i);
        i += 1;

        if (ptr.marked) {
            ptr.unmark();
        } else { // collect
            if (pipeline.state.verboseGc) {
                std.debug.print("collecting: {*} {any} {s}\n", .{ ptr, ptr.origin.kind, ptr.origin.value });
                pipeline.state.dumpSourceAtToken(ptr.origin, ptr.origin_module);
            }
            ptr.deinit(persistent_allocator);
            persistent_allocator.destroy(ptr);
            _ = gc_list.swapRemove(i - 1);
            // retry index
            i -= 1;
        }
    }
    // lose all allocations
    n_allocs = 0;
}

pub fn maybeCollect() void {
    if (shouldCollect()) {
        total_collections += 1;
        mark();
        sweep();
    }
}

pub fn shouldCollect() bool {
    return always_collect or n_allocs >= allocs_until_collect;
}

fn push(value: values.Value) !*values.Value {
    n_allocs += 1;
    maybeCollect();
    const ptr = try persistent_allocator.create(values.Value);

    try gc_list.append(ptr);
    ptr.* = value;
    return ptr;
}

pub const ValueOptions = struct {
    origin: *const tokens.Token,
    origin_module: []const u8,
};

pub fn integer(i: i64, opt: ValueOptions) !*values.Value {
    return push(.{
        .as = .{
            .integer = i,
        },
        .origin = opt.origin,
        .origin_module = opt.origin_module,
    });
}

pub fn floating(f: f64, opt: ValueOptions) !*values.Value {
    return push(.{
        .as = .{
            .float = f,
        },
        .origin = opt.origin,
        .origin_module = opt.origin_module,
    });
}

pub fn string(s: []const u8, opt: ValueOptions) !*values.Value {
    return push(.{
        .as = .{
            .string = try persistent_allocator.dupe(u8, s),
        },
        .origin = opt.origin,
        .origin_module = opt.origin_module,
    });
}

pub fn allocedString(s: []const u8, opt: ValueOptions) !*values.Value {
    return push(.{
        .as = .{
            .string = s,
        },
        .origin = opt.origin,
        .origin_module = opt.origin_module,
    });
}

pub fn boolean(b: bool, opt: ValueOptions) !*values.Value {
    if (fast_booleans) {
        return if (b) true_value else false_value;
    }
    return push(.{
        .as = .{
            .boolean = b,
        },
        .origin = opt.origin,
        .origin_module = opt.origin_module,
    });
}

pub fn list(l: values.List, opt: ValueOptions) !*values.Value {
    return push(.{
        .as = .{
            .list = l,
        },
        .origin = opt.origin,
        .origin_module = opt.origin_module,
    });
}

pub fn emptyList(opt: ValueOptions) !*values.Value {
    return list(values.List.init(persistent_allocator), opt);
}

pub fn record(r: values.Record, opt: ValueOptions) !*values.Value {
    return push(.{
        .as = .{
            .record = r,
        },
        .origin = opt.origin,
        .origin_module = opt.origin_module,
    });
}

pub fn closure(c: ast.Closure, opt: ValueOptions) !*values.Value {
    var cl: values.Closure = .{
        .ast = c,
        .upvalues = values.Closure.Upvalues.init(allocator()),
    };
    const value: Value = .{
        .as = .{
            .closure = cl,
        },
        .origin = opt.origin,
        .origin_module = opt.origin_module,
    };
    try collectClosureParentUsages(&cl);
    return push(value);
}

pub fn emptyRecord(opt: ValueOptions) !*values.Value {
    return record(values.Record.init(persistent_allocator), opt);
}

pub fn cloneOrReference(origin_value: *values.Value) !*values.Value {
    const opt = ValueOptions{
        .origin_module = origin_value.origin_module,
        .origin = origin_value.origin,
    };
    return appendRootV(switch (origin_value.as) {
        // clone
        .string => |s| try string(s, opt),
        .integer => |i| blk: {
            const in = try integer(i, opt);
            break :blk in;
        },
        .float => |f| try floating(f, opt),
        .boolean => |b| try boolean(b, opt),
        // reference
        .list, .record, .closure => origin_value,
    });
}

pub fn stackDepth() usize {
    return program_stack.items.len;
}

fn topFrame() *Frame {
    return &program_stack.items[program_stack.items.len - 1];
}

pub fn pushFrame() !void {
    try program_stack.append(.{
        .symtable = Symtable.init(persistent_allocator),
        .root_values = Roots.init(persistent_allocator),
    });
}

pub fn popFrame() void {
    var old_frame = program_stack.pop().?;
    deinitFrame(&old_frame);
}

pub fn getSymbol(key: []const u8) ?*Value {
    for (program_stack.items) |frame| {
        if (frame.symtable.get(key)) |symbol| {
            return symbol;
        }
    }
    return null;
}

pub const SymbolAndDepth = struct {
    value: *Value,
    depth: usize,
};

pub fn getSymbolAndStackDepth(key: []const u8) ?SymbolAndDepth {
    for (0.., program_stack.items) |i, frame| {
        if (frame.symtable.get(key)) |symbol| {
            return SymbolAndDepth{
                .value = symbol,
                .depth = i,
            };
        }
    }
    return null;
}

pub fn getEntry(key: []const u8) ?Symtable.Entry {
    for (program_stack.items) |frame| {
        if (frame.symtable.getEntry(key)) |entry| {
            return entry;
        }
    }
    return null;
}

pub fn appendRoot(value: *Value) !void {
    try topFrame().root_values.append(value);
}

pub fn appendRootV(value: *Value) !*Value {
    try appendRoot(value);
    return value;
}

pub fn appendParentRoot(value: *Value) !void {
    std.debug.assert(stackDepth() > 1); // must have parent
    const parent_idx = stackDepth() - 2;
    try program_stack.items[parent_idx].root_values.append(value);
}

pub fn appendToFrameRoot(root_idx: usize, value: *Value) !void {
    std.debug.assert(program_stack.items.len > root_idx);
    try program_stack.items[root_idx].root_values.append(value);
}

pub fn varDump(dump_root_values: bool) void {
    std.debug.print("+--------------+\n|   var dump   |\n+--------------+\n", .{});
    for (program_stack.items, 0..) |frame, idx| {
        std.debug.print("| frame {}:\n", .{idx});
        if (dump_root_values) {
            for (frame.root_values.items) |v| {
                std.debug.print("root value {s} ({*})\n", .{ v, v });
            }
        }
        var it = frame.symtable.iterator();
        while (it.next()) |entry| {
            std.debug.print("variable: {s}, is: {s} ({*})\n", .{ entry.key_ptr.*, entry.value_ptr.*, entry.value_ptr.* });
        }
    }
    std.debug.print("+--------------+\n| end var dump |\n+--------------+\n", .{});
}

fn mark() void {
    for (program_stack.items) |*frame| {
        markFrame(frame);
    }
}

fn markFrame(frame: *Frame) void {
    for (frame.root_values.items) |v| {
        v.mark();
    }
    var it = frame.symtable.valueIterator();
    while (it.next()) |entry| {
        entry.*.mark();
    }
}

pub fn insertSymbol(key: []const u8, value: *Value) !void {
    const frame = topFrame();
    if (frame.symtable.get(key) != null) {
        pipeline.state.dumpSourceAtToken(value.origin, value.origin_module);
        return InterpreterError.VariableAlreadyDeclared;
    }
    return try frame.symtable.put(try persistent_allocator.dupe(u8, key), value);
}

fn collectClosureParentUsages(cl: *values.Closure) !void {
    switch (cl.ast.as) {
        .body => |b| {
            _ = b;
        },
        .expression => |e| try collectClosureExpressionUsage(cl, e),
    }
}

fn collectClosureScopeUsage(cl: *values.Closure, scope: ast.Scope) !void {
    for (scope) |stmnt| {
        switch (stmnt) {
            .assignment => |a| {
                collectClosureExpressionUsage(cl, &a.expression);
            },
        }
    }
}

fn collectClosureExpressionUsage(cl: *values.Closure, expr: *const ast.Expression) !void {
    switch (expr.as) {
        .variable => |v| {
            const lookup = getSymbolAndStackDepth(v.token.value) orelse unreachable; //TODO
            std.debug.print("Variable {s} has depth {}, current depth {}\n", .{ v.token.value, lookup.depth, stackDepth() });
        },
        .binary_operator => |b| {
            try collectClosureExpressionUsage(cl, b.lhs);
            try collectClosureExpressionUsage(cl, b.rhs);
        },
        .closure => {
            //TODO: man
        },
        .bareword,
        .integer_literal,
        .string_literal,
        .record_literal,
        .bool_literal,
        .list_literal,
        .capturing_call,
        .unary_operator,
        => {},
    }
}
