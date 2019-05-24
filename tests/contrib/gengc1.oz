    call proc_main
    halt
proc_main:
# prologue
    push_stack_frame 5
# initialise int val limit
    int_const r0, 0
    store 0, r0
# initialise int val lower
    int_const r0, 0
    store 1, r0
# initialise int val result
    int_const r0, 0
    store 2, r0
# initialise int val target
    int_const r0, 0
    store 3, r0
# initialise int val upper
    int_const r0, 0
    store 4, r0
# call get_limit(limit);
    load_address r0, 0
    call proc_get_limit
# call get_target(target, limit);
    load_address r0, 3
    load r1, 0
    call proc_get_target
# result := -1;
    int_const r0, 1
    neg_int r0, r0
    store 2, r0
# lower := 1;
    int_const r0, 1
    store 1, r0
# upper := limit;
    load r0, 0
    store 4, r0
# write "Game start!";
    string_const r0, "Game start!"
    call_builtin print_string
# while result != 0
label_0:
    load r0, 2
    int_const r1, 0
    cmp_ne_int r0, r0, r1
    branch_on_true r0, label_1
    branch_uncond label_2
label_1:
# do
# call make_guess(result, upper, lower, target);
    load_address r0, 2
    load_address r1, 4
    load_address r2, 1
    load r3, 3
    call proc_make_guess
    branch_uncond label_0
# od
label_2:
# write "Thank you for playing!";
    string_const r0, "Thank you for playing!"
    call_builtin print_string
# epilogue
    pop_stack_frame 5
    return
proc_get_limit:
# prologue
    push_stack_frame 3
# parameter passing
    store 1, r0
# initialise bool val is_valid
    int_const r0, 0
    store 0, r0
# initialise int val my_limit
    int_const r0, 0
    store 2, r0
# while !is_valid
label_3:
    load r0, 0
    branch_on_true r0, label_5
    branch_uncond label_4
label_4:
# do
# write "Enter guess limit:";
    string_const r0, "Enter guess limit:"
    call_builtin print_string
# read my_limit;
    call_builtin read_int
    store 2, r0
# if my_limit <= 0
    load r0, 2
    int_const r1, 0
    cmp_le_int r0, r0, r1
    branch_on_true r0, label_6
    branch_uncond label_7
label_6:
# then
# write "limit should be positive";
    string_const r0, "limit should be positive"
    call_builtin print_string
    branch_uncond label_8
label_7:
# else
# if my_limit >= 100000000
    load r0, 2
    int_const r1, 100000000
    cmp_ge_int r0, r0, r1
    branch_on_true r0, label_9
    branch_uncond label_10
label_9:
# then
# write "limit should not exceed 99999999";
    string_const r0, "limit should not exceed 99999999"
    call_builtin print_string
    branch_uncond label_11
label_10:
# else
# limit := my_limit;
    load r0, 2
    load r1, 1
    store_indirect r1, r0
    branch_uncond label_12
label_12:
    int_const r0, 1
    store 0, r0
    branch_uncond label_14
label_13:
    int_const r0, 0
    store 0, r0
label_14:
# fi
label_11:
# fi
label_8:
    branch_uncond label_3
# od
label_5:
# epilogue
    pop_stack_frame 3
    return
proc_get_target:
# prologue
    push_stack_frame 4
# parameter passing
    store 3, r0
    store 1, r1
# initialise bool val is_valid
    int_const r0, 0
    store 0, r0
# initialise int val my_target
    int_const r0, 0
    store 2, r0
# while !is_valid
label_15:
    load r0, 0
    branch_on_true r0, label_17
    branch_uncond label_16
label_16:
# do
# write "Enter guess target:";
    string_const r0, "Enter guess target:"
    call_builtin print_string
# read my_target;
    call_builtin read_int
    store 2, r0
# if my_target <= 0
    load r0, 2
    int_const r1, 0
    cmp_le_int r0, r0, r1
    branch_on_true r0, label_18
    branch_uncond label_19
label_18:
# then
# write "target should be positive";
    string_const r0, "target should be positive"
    call_builtin print_string
    branch_uncond label_20
label_19:
# else
# if my_target > limit
    load r0, 2
    load r1, 1
    cmp_gt_int r0, r0, r1
    branch_on_true r0, label_21
    branch_uncond label_22
label_21:
# then
# write "target should not exceed limit";
    string_const r0, "target should not exceed limit"
    call_builtin print_string
    branch_uncond label_23
label_22:
# else
# target := my_target;
    load r0, 2
    load r1, 3
    store_indirect r1, r0
    branch_uncond label_24
label_24:
    int_const r0, 1
    store 0, r0
    branch_uncond label_26
label_25:
    int_const r0, 0
    store 0, r0
label_26:
# fi
label_23:
# fi
label_20:
    branch_uncond label_15
# od
label_17:
# epilogue
    pop_stack_frame 4
    return
proc_make_guess:
# prologue
    push_stack_frame 5
# parameter passing
    store 2, r0
    store 4, r1
    store 1, r2
    store 3, r3
# initialise int val average
    int_const r0, 0
    store 0, r0
# average := (upper + lower) / 2;
    load r0, 4
    load_indirect r0, r0
    load r1, 1
    load_indirect r1, r1
    add_int r0, r0, r1
    int_const r1, 2
    div_int r0, r0, r1
    store 0, r0
# write "Guessing:";
    string_const r0, "Guessing:"
    call_builtin print_string
# write average;
    load r0, 0
    call_builtin print_int
# if average = result
    load r0, 0
    load r1, 2
    load_indirect r1, r1
    cmp_eq_int r0, r0, r1
    branch_on_true r0, label_27
    branch_uncond label_28
label_27:
# then
# write "Guess correct!";
    string_const r0, "Guess correct!"
    call_builtin print_string
# result := 0;
    int_const r0, 0
    load r1, 2
    store_indirect r1, r0
    branch_uncond label_29
label_28:
# else
# if average < target
    load r0, 0
    load r1, 3
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_30
    branch_uncond label_31
label_30:
# then
# write "Too low";
    string_const r0, "Too low"
    call_builtin print_string
# lower := average + 1;
    load r0, 0
    int_const r1, 1
    add_int r0, r0, r1
    load r1, 1
    store_indirect r1, r0
# result := -1;
    int_const r0, 1
    neg_int r0, r0
    load r1, 2
    store_indirect r1, r0
    branch_uncond label_32
label_31:
# else
# write "Too high";
    string_const r0, "Too high"
    call_builtin print_string
# upper := average - 1;
    load r0, 0
    int_const r1, 1
    sub_int r0, r0, r1
    load r1, 4
    store_indirect r1, r0
# result := 1;
    int_const r0, 1
    load r1, 2
    store_indirect r1, r0
# fi
label_32:
# fi
label_29:
# epilogue
    pop_stack_frame 5
    return
