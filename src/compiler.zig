const std = @import("std");
const pipeline = @import("pipeline.zig");
const ast = @import("ast.zig");
const vm = @import("vm.zig");
const gc = @import("gc.zig");

const PipelineState = pipeline.State;

const Context = struct {
    arena: std.mem.Allocator,
    state: *PipelineState,
    root_module: *const pipeline.Module,

    vm: vm.VM,
    chunk: vm.Chunk,

    pub fn makeConstant(self: *Context, value: vm.Value) !usize {
        self.vm.push(value);
        defer _ = self.vm.pop();
        return self.chunk.addConstant(value);
    }
};

pub const InterpreterOptions = struct {
    root_module_name: []const u8,
    root_scope_already_exists: bool,
};

pub fn compile(state: *PipelineState, opt: InterpreterOptions) !void {
    const arena = state.scratch_arena.allocator();
    var ctx = Context{
        .arena = state.scratch_arena,
        .root_module = state.modules.getPtr(opt.root_module_name).?,
        .chunk = vm.Chunk.init(arena),
        .vm = vm.VM.init(arena, gc.allocator()),
    };
    try compileRoot(&ctx);
}

fn compileRoot(ctx: *Context) !void {
    for (ctx.root_module.ast.statements) |stmnt| {
        try compileStatement(ctx, stmnt);
    }
}

fn compileStatement(ctx: *Context, stmnt: ast.Statement) !void {
    switch (stmnt) {
        .var_decl => |v| try compileVarDeclaration(ctx, v),
        else => unreachable,
    }
}

fn compileVarDeclaration(ctx: *Context, decl: ast.VarDecl) !void {
    for (decl.tokens) |tok| {
        const name = ctx.makeConstant(try vm.allcateString(tok.value));
        try defineVariable(ctx, name);
    }
}

fn defineVariable(ctx: *Context, global: usize) !void {
    try emitUnaryOp(ctx, .define_global, global);
}

fn emitUnaryOp(ctx: *Context, op: vm.Op, addr: usize) !void {
    try ctx.chunk.addInstruction(op);
    try ctx.chunk.addAdress(addr);
}
