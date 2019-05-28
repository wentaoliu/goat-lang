    call proc_main
    halt
proc_main:
# prologue
    push_stack_frame 3
# initialise int val m
    int_const r0, 0
    store 0, r0
# initialise int val n
    int_const r0, 0
    store 1, r0
# initialise int val result
    int_const r0, 0
    store 2, r0
# write "I will compute Ackermann(m, n) for you!\n";
    string_const r0, "I will compute Ackermann(m, n) for you!\n"
    call_builtin print_string
# write "Type m (>= 0): ";
    string_const r0, "Type m (>= 0): "
    call_builtin print_string
# read m;
    call_builtin read_int
    store 0, r0
# write "Type n (>= 0): ";
    string_const r0, "Type n (>= 0): "
    call_builtin print_string
# read n;
    call_builtin read_int
    store 1, r0
# if (m < 0) || (n < 0)
    load r0, 0
    int_const r1, 0
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_0
    branch_uncond label_3
label_3:
    load r0, 1
    int_const r1, 0
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_0
    branch_uncond label_1
label_0:
# then
# write "Bad input. m and n must be non-negative.\n";
    string_const r0, "Bad input. m and n must be non-negative.\n"
    call_builtin print_string
    branch_uncond label_2
label_1:
# else
# call ackermann(m, n, result);
    load r0, 0
    load r1, 1
    load_address r2, 2
    call proc_ackermann
# write result;
    load r0, 2
    call_builtin print_int
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# fi
label_2:
# epilogue
    pop_stack_frame 3
    return
proc_ackermann:
# prologue
    push_stack_frame 4
# parameter passing
    store 0, r0
    store 1, r1
    store 2, r2
# initialise int val temp
    int_const r0, 0
    store 3, r0
# if m = 0
    load r0, 0
    int_const r1, 0
    cmp_eq_int r0, r0, r1
    branch_on_true r0, label_4
    branch_uncond label_5
label_4:
# then
# result := n + 1;
    load r0, 1
    int_const r1, 1
    add_int r0, r0, r1
    load r1, 2
    store_indirect r1, r0
    branch_uncond label_6
label_5:
# else
# if (m > 0) && (n = 0)
    load r0, 0
    int_const r1, 0
    cmp_gt_int r0, r0, r1
    branch_on_true r0, label_10
    branch_uncond label_8
label_10:
    load r0, 1
    int_const r1, 0
    cmp_eq_int r0, r0, r1
    branch_on_true r0, label_7
    branch_uncond label_8
label_7:
# then
# call ackermann(m - 1, 1, result);
    load r0, 0
    int_const r1, 1
    sub_int r0, r0, r1
    int_const r1, 1
    load r2, 2
    call proc_ackermann
    branch_uncond label_9
label_8:
# else
# call ackermann(m, n - 1, temp);
    load r0, 0
    load r1, 1
    int_const r2, 1
    sub_int r1, r1, r2
    load_address r2, 3
    call proc_ackermann
# call ackermann(m - 1, temp, result);
    load r0, 0
    int_const r1, 1
    sub_int r0, r0, r1
    load r1, 3
    load r2, 2
    call proc_ackermann
# fi
label_9:
# fi
label_6:
# epilogue
    pop_stack_frame 4
    return
