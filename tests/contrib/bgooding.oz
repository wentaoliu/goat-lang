    call proc_main
    halt
proc_main:
# prologue
    push_stack_frame 10
# initialise bool val f
    int_const r0, 0
    store 0, r0
# initialise int val n[2,4]
    int_const r0, 0
    store 1, r0
    int_const r1, 0
    store 2, r1
    int_const r2, 0
    store 3, r2
    int_const r3, 0
    store 4, r3
    int_const r4, 0
    store 5, r4
    int_const r5, 0
    store 6, r5
    int_const r6, 0
    store 7, r6
    int_const r7, 0
    store 8, r7
# initialise bool val t
    int_const r0, 0
    store 9, r0
    branch_uncond label_0
label_0:
    int_const r0, 1
    store 9, r0
    branch_uncond label_2
label_1:
    int_const r0, 0
    store 9, r0
label_2:
# write "Boolean Testing: ";
    string_const r0, "Boolean Testing: "
    call_builtin print_string
# if ((f < t) && !(!f != t)) && (f >= (f != (f = t)))
    load r0, 0
    load r1, 9
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_7
    branch_uncond label_4
label_7:
    load r0, 0
    branch_on_true r0, label_9
    branch_uncond label_8
label_8:
    int_const r0, 1
    branch_uncond label_10
label_9:
    int_const r0, 0
label_10:
    load r1, 9
    cmp_ne_int r0, r0, r1
    branch_on_true r0, label_4
    branch_uncond label_6
label_6:
    load r0, 0
    load r1, 0
    load r2, 0
    load r3, 9
    cmp_eq_int r2, r2, r3
    branch_on_true r2, label_14
    branch_uncond label_15
label_14:
    int_const r2, 1
    branch_uncond label_16
label_15:
    int_const r2, 0
label_16:
    cmp_ne_int r1, r1, r2
    branch_on_true r1, label_11
    branch_uncond label_12
label_11:
    int_const r1, 1
    branch_uncond label_13
label_12:
    int_const r1, 0
label_13:
    cmp_ge_int r0, r0, r1
    branch_on_true r0, label_3
    branch_uncond label_4
label_3:
# then
# write "Passed\n";
    string_const r0, "Passed\n"
    call_builtin print_string
    branch_uncond label_5
label_4:
# else
# write "Failed\n";
    string_const r0, "Failed\n"
    call_builtin print_string
# fi
label_5:
# write "Reference Testing: ";
    string_const r0, "Reference Testing: "
    call_builtin print_string
# n[1, 2] := 5;
    int_const r0, 5
    int_const r1, 1
    int_const r2, 2
    int_const r3, 4
    mul_int r1, r3, r1
    add_int r1, r1, r2
    load_address r2, 1
    sub_offset r1, r2, r1
    store_indirect r1, r0
# n[0, 3] := n[1, 2];
    int_const r0, 1
    int_const r1, 2
    int_const r2, 4
    mul_int r0, r2, r0
    add_int r0, r0, r1
    load_address r1, 1
    sub_offset r0, r1, r0
    load_indirect r0, r0
    int_const r1, 0
    int_const r2, 3
    int_const r3, 4
    mul_int r1, r3, r1
    add_int r1, r1, r2
    load_address r2, 1
    sub_offset r1, r2, r1
    store_indirect r1, r0
# call ref_test(n[1, 2]);
    int_const r0, 1
    int_const r1, 2
    int_const r2, 4
    mul_int r0, r2, r0
    add_int r0, r1, r0
    load_address r1, 1
    sub_offset r0, r1, r0
    call proc_ref_test
# if n[1, 2] = (2 * (n[0, 3] + 1))
    int_const r0, 1
    int_const r1, 2
    int_const r2, 4
    mul_int r0, r2, r0
    add_int r0, r0, r1
    load_address r1, 1
    sub_offset r0, r1, r0
    load_indirect r0, r0
    int_const r1, 2
    int_const r2, 0
    int_const r3, 3
    int_const r4, 4
    mul_int r2, r4, r2
    add_int r2, r2, r3
    load_address r3, 1
    sub_offset r2, r3, r2
    load_indirect r2, r2
    int_const r3, 1
    add_int r2, r2, r3
    mul_int r1, r1, r2
    cmp_eq_int r0, r0, r1
    branch_on_true r0, label_17
    branch_uncond label_18
label_17:
# then
# write "Passed\n";
    string_const r0, "Passed\n"
    call_builtin print_string
    branch_uncond label_19
label_18:
# else
# write "Failed\n";
    string_const r0, "Failed\n"
    call_builtin print_string
# fi
label_19:
# epilogue
    pop_stack_frame 10
    return
proc_ref_test:
# prologue
    push_stack_frame 1
# parameter passing
    store 0, r0
# call ref_test01'(n);
    load r0, 0
    call proc_ref_test01'
# n := n + n;
    load r0, 0
    load_indirect r0, r0
    load r1, 0
    load_indirect r1, r1
    add_int r0, r0, r1
    load r1, 0
    store_indirect r1, r0
# epilogue
    pop_stack_frame 1
    return
proc_ref_test01':
# prologue
    push_stack_frame 1
# parameter passing
    store 0, r0
# n := n + 1;
    load r0, 0
    load_indirect r0, r0
    int_const r1, 1
    add_int r0, r0, r1
    load r1, 0
    store_indirect r1, r0
# epilogue
    pop_stack_frame 1
    return
