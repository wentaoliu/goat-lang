    call proc_main
    halt
proc_main:
# prologue
    push_stack_frame 2
# initialise int val result
    int_const r0, 0
    store 0, r0
# initialise int val x
    int_const r0, 0
    store 1, r0
# read x;
    call_builtin read_int
    store 1, r0
# call factorial(x, result);
    load r0, 1
    load_address r1, 0
    call proc_factorial
# write result;
    load r0, 0
    call_builtin print_int
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# epilogue
    pop_stack_frame 2
    return
proc_factorial:
# prologue
    push_stack_frame 2
# parameter passing
    store 1, r0
    store 0, r1
# f := 1;
    int_const r0, 1
    load r1, 0
    store_indirect r1, r0
# if n < 0
    load r0, 1
    int_const r1, 0
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_0
    branch_uncond label_1
label_0:
# then
# write "Less than zero!";
    string_const r0, "Less than zero!"
    call_builtin print_string
    branch_uncond label_2
label_1:
# else
# while n > 1
label_3:
    load r0, 1
    int_const r1, 1
    cmp_gt_int r0, r0, r1
    branch_on_true r0, label_4
    branch_uncond label_5
label_4:
# do
# f := f * n;
    load r0, 0
    load_indirect r0, r0
    load r1, 1
    mul_int r0, r0, r1
    load r1, 0
    store_indirect r1, r0
# n := n - 1;
    load r0, 1
    int_const r1, 1
    sub_int r0, r0, r1
    store 1, r0
    branch_uncond label_3
# od
label_5:
# fi
label_2:
# epilogue
    pop_stack_frame 2
    return
