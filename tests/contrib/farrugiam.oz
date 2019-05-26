    call proc_main
    halt
proc_main:
# prologue
    push_stack_frame 1
# initialise int val x
    int_const r0, 0
    store 0, r0
# call close_call();
    call proc_close_call
# x := x / x;
    load r0, 0
    load r1, 0
    div_int r0, r0, r1
    store 0, r0
# epilogue
    pop_stack_frame 1
    return
proc_close_call:
# prologue
    push_stack_frame 1
# initialise int val savior[1]
    int_const r0, 0
    store 0, r0
# savior[2] := savior[5];
    int_const r0, 5
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    int_const r1, 2
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# epilogue
    pop_stack_frame 1
    return
