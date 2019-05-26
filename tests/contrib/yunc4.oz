    call proc_main
    halt
proc_p:
# prologue
    push_stack_frame 1
# parameter passing
    store 0, r0
# write n;
    load r0, 0
    load_indirect r0, r0
    call_builtin print_int
# n := n * n;
    load r0, 0
    load_indirect r0, r0
    load r1, 0
    load_indirect r1, r1
    mul_int r0, r0, r1
    load r1, 0
    store_indirect r1, r0
# epilogue
    pop_stack_frame 1
    return
proc_main:
# prologue
    push_stack_frame 54
# initialise float val a[12,4]
    real_const r0, 0.0
    store 0, r0
    real_const r1, 0.0
    store 1, r1
    real_const r2, 0.0
    store 2, r2
    real_const r3, 0.0
    store 3, r3
    real_const r4, 0.0
    store 4, r4
    real_const r5, 0.0
    store 5, r5
    real_const r6, 0.0
    store 6, r6
    real_const r7, 0.0
    store 7, r7
    real_const r8, 0.0
    store 8, r8
    real_const r9, 0.0
    store 9, r9
    real_const r10, 0.0
    store 10, r10
    real_const r11, 0.0
    store 11, r11
    real_const r12, 0.0
    store 12, r12
    real_const r13, 0.0
    store 13, r13
    real_const r14, 0.0
    store 14, r14
    real_const r15, 0.0
    store 15, r15
    real_const r16, 0.0
    store 16, r16
    real_const r17, 0.0
    store 17, r17
    real_const r18, 0.0
    store 18, r18
    real_const r19, 0.0
    store 19, r19
    real_const r20, 0.0
    store 20, r20
    real_const r21, 0.0
    store 21, r21
    real_const r22, 0.0
    store 22, r22
    real_const r23, 0.0
    store 23, r23
    real_const r24, 0.0
    store 24, r24
    real_const r25, 0.0
    store 25, r25
    real_const r26, 0.0
    store 26, r26
    real_const r27, 0.0
    store 27, r27
    real_const r28, 0.0
    store 28, r28
    real_const r29, 0.0
    store 29, r29
    real_const r30, 0.0
    store 30, r30
    real_const r31, 0.0
    store 31, r31
    real_const r32, 0.0
    store 32, r32
    real_const r33, 0.0
    store 33, r33
    real_const r34, 0.0
    store 34, r34
    real_const r35, 0.0
    store 35, r35
    real_const r36, 0.0
    store 36, r36
    real_const r37, 0.0
    store 37, r37
    real_const r38, 0.0
    store 38, r38
    real_const r39, 0.0
    store 39, r39
    real_const r40, 0.0
    store 40, r40
    real_const r41, 0.0
    store 41, r41
    real_const r42, 0.0
    store 42, r42
    real_const r43, 0.0
    store 43, r43
    real_const r44, 0.0
    store 44, r44
    real_const r45, 0.0
    store 45, r45
    real_const r46, 0.0
    store 46, r46
    real_const r47, 0.0
    store 47, r47
# initialise bool val aa
    int_const r0, 0
    store 48, r0
# initialise int val b
    int_const r0, 0
    store 49, r0
# initialise bool val bb
    int_const r0, 0
    store 50, r0
# initialise bool val cc
    int_const r0, 0
    store 51, r0
# initialise bool val dd
    int_const r0, 0
    store 52, r0
# initialise bool val ee
    int_const r0, 0
    store 53, r0
# a[5, 1] := (((-2 * (1 - 10)) + 2) + (2 / 2)) + (14 * 5.4);
    int_const r0, 2
    neg_int r0, r0
    int_const r1, 1
    int_const r2, 10
    sub_int r1, r1, r2
    mul_int r0, r0, r1
    int_const r1, 2
    add_int r0, r0, r1
    int_const r1, 2
    int_const r2, 2
    div_int r1, r1, r2
    add_int r0, r0, r1
    int_to_real r0, r0
    int_const r1, 14
    int_to_real r1, r1
    real_const r2, 5.4
    mul_real r1, r1, r2
    add_real r0, r0, r1
    int_const r1, 5
    int_const r2, 1
    int_const r3, 4
    mul_int r1, r3, r1
    add_int r1, r1, r2
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# b := 2;
    int_const r0, 2
    store 49, r0
# call p(b);
    load_address r0, 49
    call proc_p
# a[b, 2] := a[5, 1];
    int_const r0, 5
    int_const r1, 1
    int_const r2, 4
    mul_int r0, r2, r0
    add_int r0, r0, r1
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    load r1, 49
    int_const r2, 2
    int_const r3, 4
    mul_int r1, r3, r1
    add_int r1, r1, r2
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# while a[b, 2] > -11
label_0:
    load r0, 49
    int_const r1, 2
    int_const r2, 4
    mul_int r0, r2, r0
    add_int r0, r0, r1
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    int_const r1, 11
    neg_int r1, r1
    int_to_real r1, r1
    cmp_gt_real r0, r0, r1
    branch_on_true r0, label_1
    branch_uncond label_2
label_1:
# do
# write a[b, 2];
    load r0, 49
    int_const r1, 2
    int_const r2, 4
    mul_int r0, r2, r0
    add_int r0, r0, r1
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    call_builtin print_real
    branch_uncond label_0
# od
label_2:
# write b;
    load r0, 49
    call_builtin print_int
    load r0, 48
    branch_on_true r0, label_3
    branch_uncond label_7
label_7:
    load r0, 50
    branch_on_true r0, label_9
    branch_uncond label_6
label_9:
    load r0, 51
    branch_on_true r0, label_6
    branch_uncond label_8
label_8:
    load r0, 52
    branch_on_true r0, label_6
    branch_uncond label_3
label_6:
    load r0, 53
    load r1, 51
    cmp_eq_int r0, r0, r1
    branch_on_true r0, label_4
    branch_uncond label_3
label_3:
    int_const r0, 1
    store 48, r0
    branch_uncond label_5
label_4:
    int_const r0, 0
    store 48, r0
label_5:
# write aa;
    load r0, 48
    call_builtin print_bool
# write aa && bb;
    load r0, 48
    branch_on_true r0, label_13
    branch_uncond label_11
label_13:
    load r0, 50
    branch_on_true r0, label_10
    branch_uncond label_11
label_10:
    int_const r0, 1
    branch_uncond label_12
label_11:
    int_const r0, 0
label_12:
    call_builtin print_bool
# write aa && cc;
    load r0, 48
    branch_on_true r0, label_17
    branch_uncond label_15
label_17:
    load r0, 51
    branch_on_true r0, label_14
    branch_uncond label_15
label_14:
    int_const r0, 1
    branch_uncond label_16
label_15:
    int_const r0, 0
label_16:
    call_builtin print_bool
# epilogue
    pop_stack_frame 54
    return
