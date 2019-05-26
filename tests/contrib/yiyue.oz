    call proc_main
    halt
proc_main:
# prologue
    push_stack_frame 5
# initialise int val a[1,2]
    int_const r0, 0
    store 0, r0
    store 1, r0
# initialise int val b
    int_const r0, 0
    store 2, r0
# initialise int val c[1,2]
    int_const r0, 0
    store 3, r0
    store 4, r0
# a[1, 0] := 1;
    int_const r0, 1
    int_const r1, 1
    int_const r2, 0
    int_const r3, 2
    mul_int r1, r3, r1
    add_int r1, r1, r2
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# b := 3;
    int_const r0, 3
    store 2, r0
# while b > 0
label_0:
    load r0, 2
    int_const r1, 0
    cmp_gt_int r0, r0, r1
    branch_on_true r0, label_1
    branch_uncond label_2
label_1:
# do
# write b;
    load r0, 2
    call_builtin print_int
# write " ";
    string_const r0, " "
    call_builtin print_string
# b := b - 1;
    load r0, 2
    int_const r1, 1
    sub_int r0, r0, r1
    store 2, r0
    branch_uncond label_0
# od
label_2:
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# epilogue
    pop_stack_frame 5
    return
