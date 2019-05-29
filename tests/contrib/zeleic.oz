    call proc_main
    halt
proc_f:
# prologue
    push_stack_frame 1
# parameter passing
    store 0, r0
# x := x * (3 + x);
    load r0, 0
    int_const r1, 3
    load r2, 0
    add_int r1, r1, r2
    mul_int r0, r0, r1
    store 0, r0
# write x;
    load r0, 0
    call_builtin print_int
# epilogue
    pop_stack_frame 1
    return
proc_main:
# prologue
    push_stack_frame 1
# initialise int val y
    int_const r0, 0
    store 0, r0
# y := 5;
    int_const r0, 5
    store 0, r0
# call f(y);
    load r0, 0
    call proc_f
# write y;
    load r0, 0
    call_builtin print_int
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# epilogue
    pop_stack_frame 1
    return
