    call proc_main
    halt
proc_main:
# prologue
    push_stack_frame 6
# initialise float val a
    real_const r0, 0.0
    store 0, r0
# initialise float val b
    real_const r0, 0.0
    store 1, r0
# initialise float val delta
    real_const r0, 0.0
    store 2, r0
# initialise int val iter
    int_const r0, 0
    store 3, r0
# initialise int val x
    int_const r0, 0
    store 4, r0
# initialise float val y
    real_const r0, 0.0
    store 5, r0
# x := 7;
    int_const r0, 7
    store 4, r0
# a := 0;
    int_const r0, 0
    int_to_real r0, r0
    store 0, r0
# b := x;
    load r0, 4
    int_to_real r0, r0
    store 1, r0
# y := (a + b) / 2.0;
    load r0, 0
    load r1, 1
    add_real r0, r0, r1
    real_const r1, 2.0
    div_real r0, r0, r1
    store 5, r0
# iter := 0;
    int_const r0, 0
    store 3, r0
# delta := (y * y) - x;
    load r0, 5
    load r1, 5
    mul_real r0, r0, r1
    load r1, 4
    int_to_real r1, r1
    sub_real r0, r0, r1
    store 2, r0
# while ((delta > 0.1) || (delta < -0.1)) && (iter < 100)
label_0:
    load r0, 2
    real_const r1, 0.1
    cmp_gt_real r0, r0, r1
    branch_on_true r0, label_3
    branch_uncond label_4
label_4:
    load r0, 2
    real_const r1, 0.1
    neg_real r1, r1
    cmp_lt_real r0, r0, r1
    branch_on_true r0, label_3
    branch_uncond label_2
label_3:
    load r0, 3
    int_const r1, 100
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_1
    branch_uncond label_2
label_1:
# do
# iter := iter + 1;
    load r0, 3
    int_const r1, 1
    add_int r0, r0, r1
    store 3, r0
# if delta > 0
    load r0, 2
    int_const r1, 0
    int_to_real r1, r1
    cmp_gt_real r0, r0, r1
    branch_on_true r0, label_5
    branch_uncond label_6
label_5:
# then
# b := y;
    load r0, 5
    store 1, r0
    branch_uncond label_7
label_6:
# else
# a := y;
    load r0, 5
    store 0, r0
# fi
label_7:
# y := (a + b) / 2.0;
    load r0, 0
    load r1, 1
    add_real r0, r0, r1
    real_const r1, 2.0
    div_real r0, r0, r1
    store 5, r0
# delta := (y * y) - x;
    load r0, 5
    load r1, 5
    mul_real r0, r0, r1
    load r1, 4
    int_to_real r1, r1
    sub_real r0, r0, r1
    store 2, r0
    branch_uncond label_0
# od
label_2:
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# write "The square root of ";
    string_const r0, "The square root of "
    call_builtin print_string
# write x;
    load r0, 4
    call_builtin print_int
# write "is ";
    string_const r0, "is "
    call_builtin print_string
# write y;
    load r0, 5
    call_builtin print_real
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# epilogue
    pop_stack_frame 6
    return
