    call proc_main
    halt
proc_p:
# prologue
    push_stack_frame 1
# parameter passing
    store 0, r0
# i := i - 1;
    load r0, 0
    load_indirect r0, r0
    int_const r1, 1
    sub_int r0, r0, r1
    load r1, 0
    store_indirect r1, r0
# epilogue
    pop_stack_frame 1
    return
proc_main:
# prologue
    push_stack_frame 2
# initialise int val a
    int_const r0, 0
    store 0, r0
# initialise int val b
    int_const r0, 0
    store 1, r0
# a := 100;
    int_const r0, 100
    store 0, r0
# if a > 0
    load r0, 0
    int_const r1, 0
    cmp_gt_int r0, r0, r1
    branch_on_true r0, label_0
    branch_uncond label_1
label_0:
# then
# a := a + 1;
    load r0, 0
    int_const r1, 1
    add_int r0, r0, r1
    store 0, r0
# b := 2 * a;
    int_const r0, 2
    load r1, 0
    mul_int r0, r0, r1
    store 1, r0
    branch_uncond label_2
label_1:
# else
# a := 2 * a;
    int_const r0, 2
    load r1, 0
    mul_int r0, r0, r1
    store 0, r0
# b := 1 - a;
    int_const r0, 1
    load r1, 0
    sub_int r0, r0, r1
    store 1, r0
# fi
label_2:
# while a < b
label_3:
    load r0, 0
    load r1, 1
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_4
    branch_uncond label_5
label_4:
# do
# a := a + 1;
    load r0, 0
    int_const r1, 1
    add_int r0, r0, r1
    store 0, r0
# b := a + 30;
    load r0, 0
    int_const r1, 30
    add_int r0, r0, r1
    store 1, r0
# while b > 30
label_6:
    load r0, 1
    int_const r1, 30
    cmp_gt_int r0, r0, r1
    branch_on_true r0, label_7
    branch_uncond label_8
label_7:
# do
# call p(b);
    load_address r0, 1
    call proc_p
    branch_uncond label_6
# od
label_8:
    branch_uncond label_3
# od
label_5:
# epilogue
    pop_stack_frame 2
    return
