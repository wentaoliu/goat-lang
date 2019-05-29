    call proc_main
    halt
proc_main:
# prologue
    push_stack_frame 3
# initialise int val hundred
    int_const r0, 0
    store 0, r0
# initialise int val ninety
    int_const r0, 0
    store 1, r0
# initialise int val ten
    int_const r0, 0
    store 2, r0
# ten := 10;
    int_const r0, 10
    store 2, r0
# ninety := 90;
    int_const r0, 90
    store 1, r0
# hundred := ten + ninety;
    load r0, 2
    load r1, 1
    add_int r0, r0, r1
    store 0, r0
# write "Result is ";
    string_const r0, "Result is "
    call_builtin print_string
# write hundred;
    load r0, 0
    call_builtin print_int
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# epilogue
    pop_stack_frame 3
    return
