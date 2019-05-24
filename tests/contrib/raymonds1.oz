    call proc_main
    halt
proc_main:
# prologue
    push_stack_frame 3
# initialise bool val even
    int_const r0, 0
    store 0, r0
# initialise int val num
    int_const r0, 0
    store 1, r0
# initialise bool val success
    int_const r0, 0
    store 2, r0
# num := 12;
    int_const r0, 12
    store 1, r0
# call is_even(even, num);
    load_address r0, 0
    load r1, 1
    call proc_is_even
# if even
    load r0, 0
    branch_on_true r0, label_0
    branch_uncond label_1
label_0:
# then
# call reduce(success, num);
    load_address r0, 2
    load r1, 1
    call proc_reduce
    branch_uncond label_2
label_1:
# else
# call expand(success, num);
    load_address r0, 2
    load r1, 1
    call proc_expand
# fi
label_2:
# write "For the number ";
    string_const r0, "For the number "
    call_builtin print_string
# write num;
    load r0, 1
    call_builtin print_int
# write "; sequence termination: ";
    string_const r0, "; sequence termination: "
    call_builtin print_string
# write success;
    load r0, 2
    call_builtin print_bool
# write "!\n";
    string_const r0, "!\n"
    call_builtin print_string
# epilogue
    pop_stack_frame 3
    return
proc_reduce:
# prologue
    push_stack_frame 3
# parameter passing
    store 2, r0
    store 1, r1
# initialise bool val even
    int_const r0, 0
    store 0, r0
# if num = 1
    load r0, 1
    int_const r1, 1
    cmp_eq_int r0, r0, r1
    branch_on_true r0, label_3
    branch_uncond label_4
label_3:
# then
# write num;
    load r0, 1
    call_builtin print_int
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
    branch_uncond label_6
label_6:
    int_const r0, 1
    load r1, 2
    store_indirect r1, r0
    branch_uncond label_8
label_7:
    int_const r0, 0
    load r1, 2
    store_indirect r1, r0
label_8:
    branch_uncond label_5
label_4:
# else
# write num;
    load r0, 1
    call_builtin print_int
# write " -> ";
    string_const r0, " -> "
    call_builtin print_string
# num := num / 2;
    load r0, 1
    int_const r1, 2
    div_int r0, r0, r1
    store 1, r0
# call is_even(even, num);
    load_address r0, 0
    load r1, 1
    call proc_is_even
# if even
    load r0, 0
    branch_on_true r0, label_9
    branch_uncond label_10
label_9:
# then
# call reduce(successful, num);
    load r0, 2
    load r1, 1
    call proc_reduce
    branch_uncond label_11
label_10:
# else
# call expand(successful, num);
    load r0, 2
    load r1, 1
    call proc_expand
# fi
label_11:
# fi
label_5:
# epilogue
    pop_stack_frame 3
    return
proc_expand:
# prologue
    push_stack_frame 3
# parameter passing
    store 2, r0
    store 1, r1
# initialise bool val even
    int_const r0, 0
    store 0, r0
# if num = 1
    load r0, 1
    int_const r1, 1
    cmp_eq_int r0, r0, r1
    branch_on_true r0, label_12
    branch_uncond label_13
label_12:
# then
# write num;
    load r0, 1
    call_builtin print_int
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
    branch_uncond label_15
label_15:
    int_const r0, 1
    load r1, 2
    store_indirect r1, r0
    branch_uncond label_17
label_16:
    int_const r0, 0
    load r1, 2
    store_indirect r1, r0
label_17:
    branch_uncond label_14
label_13:
# else
# write num;
    load r0, 1
    call_builtin print_int
# write " -> ";
    string_const r0, " -> "
    call_builtin print_string
# num := (3 * num) + 1;
    int_const r0, 3
    load r1, 1
    mul_int r0, r0, r1
    int_const r1, 1
    add_int r0, r0, r1
    store 1, r0
# call is_even(even, num);
    load_address r0, 0
    load r1, 1
    call proc_is_even
# if even
    load r0, 0
    branch_on_true r0, label_18
    branch_uncond label_19
label_18:
# then
# call reduce(successful, num);
    load r0, 2
    load r1, 1
    call proc_reduce
    branch_uncond label_20
label_19:
# else
# call expand(successful, num);
    load r0, 2
    load r1, 1
    call proc_expand
# fi
label_20:
# fi
label_14:
# epilogue
    pop_stack_frame 3
    return
proc_is_even:
# prologue
    push_stack_frame 2
# parameter passing
    store 0, r0
    store 1, r1
    branch_uncond label_22
label_21:
    int_const r0, 1
    load r1, 0
    store_indirect r1, r0
    branch_uncond label_23
label_22:
    int_const r0, 0
    load r1, 0
    store_indirect r1, r0
label_23:
# while num >= 0
label_24:
    load r0, 1
    int_const r1, 0
    cmp_ge_int r0, r0, r1
    branch_on_true r0, label_25
    branch_uncond label_26
label_25:
# do
# if num = 0
    load r0, 1
    int_const r1, 0
    cmp_eq_int r0, r0, r1
    branch_on_true r0, label_27
    branch_uncond label_28
label_27:
# then
    branch_uncond label_29
label_29:
    int_const r0, 1
    load r1, 0
    store_indirect r1, r0
    branch_uncond label_31
label_30:
    int_const r0, 0
    load r1, 0
    store_indirect r1, r0
label_31:
# fi
label_28:
# num := num - 2;
    load r0, 1
    int_const r1, 2
    sub_int r0, r0, r1
    store 1, r0
    branch_uncond label_24
# od
label_26:
# epilogue
    pop_stack_frame 2
    return
