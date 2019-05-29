    call proc_main
    halt
proc_f:
# prologue
    push_stack_frame 1
# parameter passing
    store 0, r0
# x := x + 1;
    load r0, 0
    int_const r1, 1
    int_to_real r1, r1
    add_real r0, r0, r1
    store 0, r0
# epilogue
    pop_stack_frame 1
    return
proc_main:
# prologue
    push_stack_frame 1
# initialise int val n
    int_const r0, 0
    store 0, r0
# n := 41;
    int_const r0, 41
    store 0, r0
# call f(n);
    load r0, 0
    int_to_real r0, r0
    call proc_f
# epilogue
    pop_stack_frame 1
    return
