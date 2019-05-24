    call proc_main
    halt
proc_main:
# prologue
    push_stack_frame 1
# initialise int val a
    int_const r0, 0
    store 0, r0
# write "This is a test file for everyone\n";
    string_const r0, "This is a test file for everyone\n"
    call_builtin print_string
# a := 100;
    int_const r0, 100
    store 0, r0
# if (3 > 4) || (10 != 1)
    int_const r0, 3
    int_const r1, 4
    cmp_gt_int r0, r0, r1
    branch_on_true r0, label_0
    branch_uncond label_2
label_2:
    int_const r0, 10
    int_const r1, 1
    cmp_ne_int r0, r0, r1
    branch_on_true r0, label_0
    branch_uncond label_1
label_0:
# then
# call sum(a);
    load r0, 0
    call proc_sum
# fi
label_1:
# epilogue
    pop_stack_frame 1
    return
proc_sum:
# prologue
    push_stack_frame 2
# parameter passing
    store 1, r0
# initialise int val result
    int_const r0, 0
    store 0, r0
# while x >= 0
label_3:
    load r0, 1
    int_const r1, 0
    cmp_ge_int r0, r0, r1
    branch_on_true r0, label_4
    branch_uncond label_5
label_4:
# do
# result := result + x;
    load r0, 0
    load r1, 1
    add_int r0, r0, r1
    store 0, r0
# x := x - 1;
    load r0, 1
    int_const r1, 1
    sub_int r0, r0, r1
    store 1, r0
    branch_uncond label_3
# od
label_5:
# write x;
    load r0, 1
    call_builtin print_int
# epilogue
    pop_stack_frame 2
    return
