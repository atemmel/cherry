const std = @import("std");
const main = @import("main.zig");
const eql = std.mem.eql;

pub const ParseError = error{} || std.Io.Reader.DelimiterError || std.mem.Allocator.Error;

pub const ResponseError = error{} || std.Io.Writer.Error || std.mem.Allocator.Error;

pub const Request = struct {
    content_length: usize,
    raw_body: []const u8,
    body: RequestBody,
};

pub const RequestBody = struct {
    id: usize,
    method: []const u8,
    jsonrpc: []const u8,
};

const ServerCapabilities = struct {
    //positionEncoding: ?[]const u8,
    //textDocumentSync: ?TextDocumentSyncOptions | TextDocumentSyncKind;
    //notebookDocumentSync?: NotebookDocumentSyncOptions | NotebookDocumentSyncRegistrationOptions;
    //completionProvider: ?CompletionOptions;
    //hoverProvider?: boolean | HoverOptions;
    //signatureHelpProvider?: SignatureHelpOptions;
    //declarationProvider?: boolean | DeclarationOptions | DeclarationRegistrationOptions;
    //definitionProvider?: boolean | DefinitionOptions;
    //typeDefinitionProvider?: boolean | TypeDefinitionOptions | TypeDefinitionRegistrationOptions;
    //implementationProvider?: boolean | ImplementationOptions | ImplementationRegistrationOptions;
    //referencesProvider?: boolean | ReferenceOptions;
    //documentHighlightProvider?: boolean | DocumentHighlightOptions;
    //documentSymbolProvider?: boolean | DocumentSymbolOptions;
    //codeActionProvider?: boolean | CodeActionOptions;
    //codeLensProvider?: CodeLensOptions;
    //documentLinkProvider?: DocumentLinkOptions;
    //colorProvider?: boolean | DocumentColorOptions | DocumentColorRegistrationOptions;
    //documentFormattingProvider?: boolean | DocumentFormattingOptions;
    //documentRangeFormattingProvider?: boolean | DocumentRangeFormattingOptions;
    //documentOnTypeFormattingProvider?: DocumentOnTypeFormattingOptions;
    //renameProvider?: boolean | RenameOptions;
    //foldingRangeProvider?: boolean | FoldingRangeOptions | FoldingRangeRegistrationOptions;
    //executeCommandProvider?: ExecuteCommandOptions;
    //selectionRangeProvider?: boolean | SelectionRangeOptions | SelectionRangeRegistrationOptions;
    //linkedEditingRangeProvider?: boolean | LinkedEditingRangeOptions | LinkedEditingRangeRegistrationOptions;
    //callHierarchyProvider?: boolean | CallHierarchyOptions | CallHierarchyRegistrationOptions;
    //semanticTokensProvider?: SemanticTokensOptions | SemanticTokensRegistrationOptions;
    //monikerProvider?: boolean | MonikerOptions | MonikerRegistrationOptions;
    //typeHierarchyProvider?: boolean | TypeHierarchyOptions | TypeHierarchyRegistrationOptions;
    //inlineValueProvider?: boolean | InlineValueOptions | InlineValueRegistrationOptions;
    //inlayHintProvider?: boolean | InlayHintOptions | InlayHintRegistrationOptions;
    //diagnosticProvider?: DiagnosticOptions | DiagnosticRegistrationOptions;
    //workspaceSymbolProvider?: boolean | WorkspaceSymbolOptions;
    //workspace?: {
    //workspaceFolders?: WorkspaceFoldersServerCapabilities;
    //fileOperations?: {
    //didCreate?: FileOperationRegistrationOptions;
    //willCreate?: FileOperationRegistrationOptions;
    //didRename?: FileOperationRegistrationOptions;
    //willRename?: FileOperationRegistrationOptions;
    //didDelete?: FileOperationRegistrationOptions;
    //willDelete?: FileOperationRegistrationOptions;
    //};
    //};
};

const InitializeResult = struct {
    capabilities: ServerCapabilities,
    serverInfo: ?struct {
        name: []const u8,
        version: ?[]const u8,
    },
};

pub const Response = struct {
    id: usize,
    result: InitializeResult,
    @"error": ?struct {
        code: usize,
        message: []const u8,
    },
};

pub const Context = struct {
    source: *std.Io.Reader,
    sink: *std.Io.Writer,
    arena: std.mem.Allocator,
    arena_impl: std.heap.ArenaAllocator,
};

const Method = fn (ctx: *Context, req: Request) ResponseError!?Response;

const methods_table = std.StaticStringMap(*const Method).initComptime(&.{
    .{ "initialize", writeInitializedResponse },
    .{ "shutdown", handleShutdown },
});

pub fn run(gpa: std.mem.Allocator) !void {
    var source_buffer: [2048]u8 = .{0} ** 2048;
    var sink_buffer: [2048]u8 = .{0} ** 2048;

    var source = std.fs.File.stdin().reader(&source_buffer);
    var sink = std.fs.File.stdout().writer(&sink_buffer);

    var arena_impl = std.heap.ArenaAllocator.init(gpa);

    var ctx: Context = .{
        .source = &source.interface,
        .sink = &sink.interface,
        .arena_impl = arena_impl,
        .arena = arena_impl.allocator(),
    };

    while (true) {
        const req = try parseRequest(&ctx);

        if (methods_table.get(req.body.method)) |method| {
            const response = try method(&ctx, req) orelse break;
            try writeResponse(&ctx, response);
        } else {
            std.debug.print("unrecognized method: '{s}'\n'{s}'\n", .{ req.body.method, req.raw_body });
        }
    }
}

fn parseRequest(ctx: *Context) ParseError!Request {
    var req: Request = undefined;
    try parseHeaders(ctx, &req);
    try parseBody(ctx, &req);
    return req;
}

fn parseHeaders(ctx: *Context, req: *Request) ParseError!void {
    while (true) {
        const maybe_end = try ctx.source.peek(2);
        if (eql(u8, "\r\n", maybe_end)) {
            _ = try ctx.source.take(2);
            break;
        }

        const header_name = try ctx.source.takeDelimiterExclusive(':');
        _ = try ctx.source.take(2);
        const header_value = try ctx.source.takeDelimiterExclusive('\r');
        _ = try ctx.source.take(2);

        if (eql(u8, "Content-Length", header_name)) {
            req.content_length = std.fmt.parseUnsigned(usize, header_value, 10) catch @panic("TODO");
        } else if (eql(u8, "Content-Type", header_name)) {}
    }
}

fn parseBody(ctx: *Context, req: *Request) ParseError!void {
    req.raw_body = try ctx.source.readAlloc(ctx.arena, req.content_length);
    const parsed = std.json.parseFromSlice(RequestBody, ctx.arena, req.raw_body, .{ .ignore_unknown_fields = true }) catch |err| {
        std.debug.print("Something was wrong with the response body: {}\n", .{err});
        return;
    };
    req.body = parsed.value;
}

fn writeInitializedResponse(_: *Context, req: Request) ResponseError!?Response {
    const response: Response = .{
        .id = req.body.id,
        .result = .{
            .capabilities = .{},
            .serverInfo = .{
                .name = "cherry",
                .version = main.git_latest_commit_hash,
            },
        },
        .@"error" = null,
    };
    return response;
}

fn handleShutdown(_: *Context, _: Request) ResponseError!?Response {
    return null;
}

fn writeResponse(ctx: *Context, response: Response) ResponseError!void {
    const body = try std.json.Stringify.valueAlloc(ctx.arena, response, .{});
    try ctx.sink.print("Content-Length: {}\r\n\r\n", .{body.len});
    try ctx.sink.flush();
    _ = try ctx.sink.write(body);
    try ctx.sink.flush();
}
