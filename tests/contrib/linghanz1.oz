    call proc_main
    halt
proc_main:
# prologue
    push_stack_frame 5
# initialise int val n
    int_const r0, 0
    store 0, r0
# initialise int val sum
    int_const r0, 0
    store 1, r0
# initialise int val sum_temp
    int_const r0, 0
    store 2, r0
# initialise int val temp
    int_const r0, 0
    store 3, r0
# initialise int val x
    int_const r0, 0
    store 4, r0
# x := 15;
    int_const r0, 15
    store 4, r0
# n := 14;
    int_const r0, 14
    store 0, r0
# sum := 0;
    int_const r0, 0
    store 1, r0
# sum_temp := 0;
    int_const r0, 0
    store 2, r0
# while n > 0
label_0:
    load r0, 0
    int_const r1, 0
    cmp_gt_int r0, r0, r1
    branch_on_true r0, label_1
    branch_uncond label_2
label_1:
# do
# temp := x;
    load r0, 4
    store 3, r0
# while temp > 0
label_3:
    load r0, 3
    int_const r1, 0
    cmp_gt_int r0, r0, r1
    branch_on_true r0, label_4
    branch_uncond label_5
label_4:
# do
# sum_temp := sum_temp + temp;
    load r0, 2
    load r1, 3
    add_int r0, r0, r1
    store 2, r0
# temp := temp - 1;
    load r0, 3
    int_const r1, 1
    sub_int r0, r0, r1
    store 3, r0
# if sum > 100
    load r0, 1
    int_const r1, 100
    cmp_gt_int r0, r0, r1
    branch_on_true r0, label_6
    branch_uncond label_7
label_6:
# then
# sum := sum - 100;
    load r0, 1
    int_const r1, 100
    sub_int r0, r0, r1
    store 1, r0
# fi
label_7:
    branch_uncond label_3
# od
label_5:
# sum := sum + sum_temp;
    load r0, 1
    load r1, 2
    add_int r0, r0, r1
    store 1, r0
# n := n - 1;
    load r0, 0
    int_const r1, 1
    sub_int r0, r0, r1
    store 0, r0
    branch_uncond label_0
# od
label_2:
# write sum;
    load r0, 1
    call_builtin print_int
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# epilogue
    pop_stack_frame 5
    return
