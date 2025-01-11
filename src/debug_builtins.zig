const std = @import("std");
const Value = @import("compiler/value.zig").Value;
const Vm = @import("vm/Vm.zig");

const HONEY_RELEASE = false;

/// Dumps information about the stack to stderr
pub fn dump_stack(vm: *Vm, args: []const Value) !?Value {
    if (args.len != 0) {
        return error.InvalidNumberOfArguments;
    }
    vm.stack.dump();
    return null;
}

/// Sets the VM's stack logging state
pub fn set_stack_logging(vm: *Vm, args: []const Value) !?Value {
    if (args.len != 1) {
        return error.InvalidNumberOfArguments;
    }
    const value: bool = if (args[0] == .boolean)
        args[0].boolean
    else
        return error.UnexpectedType;

    vm.options.print_stack_state = value;
    return null;
}

/// Dumps information about a variadic number of arguments to stderr
pub fn dump_var(vm: *Vm, args: []const Value) !?Value {
    if (args.len == 0) {
        return error.InvalidNumberOfArguments;
    }
    for (args) |arg| {
        try vm.writer.print("{s}({s})\n", .{ arg.typeName(), arg });
    }
    return null;
}
