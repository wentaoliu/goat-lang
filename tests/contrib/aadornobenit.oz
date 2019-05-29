    call proc_main
    halt
proc_main:
# prologue
    push_stack_frame 1
# initialise int val a
    int_const r0, 0
    store 0, r0
# call a(1, 3, 4 + 3);
    int_const r0, 1
    int_const r1, 3
    int_const r2, 4
    int_const r3, 3
    add_int r2, r2, r3
    call proc_a
# epilogue
    pop_stack_frame 1
    return
proc_a:
# prologue
    push_stack_frame 3
# parameter passing
    store 0, r0
    store 1, r1
    store 2, r2
# write (x + y) + x;
    load r0, 0
    load r1, 1
    add_int r0, r0, r1
    load r1, 0
    add_int r0, r0, r1
    call_builtin print_int
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# epilogue
    pop_stack_frame 3
    return
