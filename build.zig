const std = @import("std");

fn dateStr(b: *std.Build) []const u8 {
    const now: u64 = @intCast(std.time.timestamp());
    const epoch = std.time.epoch.EpochSeconds{ .secs = now };
    const epoch_day = epoch.getEpochDay();
    const year_day = epoch_day.calculateYearDay();
    const month_day = year_day.calculateMonthDay();
    const day_seconds = epoch.getDaySeconds();

    const year = year_day.year;
    const month = month_day.month.numeric();
    const day = month_day.day_index + 1;
    const hour = day_seconds.getHoursIntoDay();
    const minute = day_seconds.getMinutesIntoHour();

    return std.fmt.allocPrint(b.allocator, "{}-{}-{} {:0>2}:{:0>2}", .{ year, month, day, hour, minute }) catch unreachable;
}

// Although this function looks imperative, note that its job is to
// declaratively construct a build graph that will be executed by an external
// runner.
pub fn build(b: *std.Build) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard optimization options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall. Here we do not
    // set a preferred release mode, allowing the user to decide how to optimize.
    const optimize = b.standardOptimizeOption(.{});

    const main_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const exe = b.addExecutable(.{
        .name = "cherry",
        .root_module = main_mod,
        .use_llvm = optimize != .Debug,
        .use_lld = optimize != .Debug,
    });

    const options = b.addOptions();
    options.addOption([]const u8, "build_date", dateStr(b));
    exe.root_module.addOptions("build_options", options);

    //git log --format="%H" -n 1
    const git = b.addSystemCommand(&.{"git"});
    git.addArgs(&.{
        "log",
        \\--format=%H
        ,
        "-n",
        "1",
    });
    const git_latest_commit_hash = git.captureStdOut();
    exe.root_module.addAnonymousImport("git_latest_commit_hash", .{ .root_source_file = git_latest_commit_hash });

    const clap = b.dependency("clap", .{});
    exe.root_module.addImport("clap", clap.module("clap"));

    // This declares intent for the executable to be installed into the
    // standard location when the user invokes the "install" step (the default
    // step when running `zig build`).
    b.installArtifact(exe);

    // This *creates* a Run step in the build graph, to be executed when another
    // step is evaluated that depends on it. The next line below will establish
    // such a dependency.
    const run_cmd = b.addRunArtifact(exe);

    const exe_check = b.addExecutable(.{
        .name = "cherry",
        .root_module = main_mod,
    });

    const check = b.step("check", "Check if cherry compiles");
    check.dependOn(&exe_check.step);

    // By making the run step depend on the install step, it will be run from the
    // installation directory rather than directly from within the cache directory.
    // This is not necessary, however, if the application depends on other installed
    // files, this ensures they will be present and in the expected location.
    run_cmd.step.dependOn(b.getInstallStep());

    // This allows the user to pass arguments to the application in the build
    // command itself, like this: `zig build run -- arg1 arg2 etc`
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    // This creates a build step. It will be visible in the `zig build --help` menu,
    // and can be selected like this: `zig build run`
    // This will evaluate the `run` step rather than the default, which is "install".
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const exe_unit_tests = b.addTest(.{
        .root_module = main_mod,
        .use_llvm = false,
        .use_lld = false,
    });

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    // Similar to creating the run step earlier, this exposes a `test` step to
    // the `zig build --help` menu, providing a way for the user to request
    // running the unit tests.
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);
}
