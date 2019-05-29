    call proc_main
    halt
proc_main:
# prologue
    push_stack_frame 1
# initialise float val ans
    real_const r0, 0.0
    store 0, r0
# call calculation1(ans);
    load r0, 0
    call proc_calculation1
# write ans;
    load r0, 0
    call_builtin print_real
# call calculation2(ans);
    load_address r0, 0
    call proc_calculation2
# write ans;
    load r0, 0
    call_builtin print_real
# call loopMatrix();
    call proc_loopMatrix
# epilogue
    pop_stack_frame 1
    return
proc_calculation1:
# prologue
    push_stack_frame 2
# parameter passing
    store 1, r0
# initialise float val a
    real_const r0, 0.0
    store 0, r0
# a := 10.1;
    real_const r0, 10.1
    store 0, r0
# ans := (((a * (2 + 1)) / 3) - 1.1) + (10 / 6);
    load r0, 0
    int_const r1, 2
    int_const r2, 1
    add_int r1, r1, r2
    int_to_real r1, r1
    mul_real r0, r0, r1
    int_const r1, 3
    int_to_real r1, r1
    div_real r0, r0, r1
    real_const r1, 1.1
    sub_real r0, r0, r1
    int_const r1, 10
    int_const r2, 6
    div_int r1, r1, r2
    int_to_real r1, r1
    add_real r0, r0, r1
    store 1, r0
# write ans;
    load r0, 1
    call_builtin print_real
# epilogue
    pop_stack_frame 2
    return
proc_calculation2:
# prologue
    push_stack_frame 2
# parameter passing
    store 1, r0
# initialise float val a
    real_const r0, 0.0
    store 0, r0
# a := 10.1;
    real_const r0, 10.1
    store 0, r0
# ans := (((a * (2 + 1)) / 3) - 1.1) + (10 / 6);
    load r0, 0
    int_const r1, 2
    int_const r2, 1
    add_int r1, r1, r2
    int_to_real r1, r1
    mul_real r0, r0, r1
    int_const r1, 3
    int_to_real r1, r1
    div_real r0, r0, r1
    real_const r1, 1.1
    sub_real r0, r0, r1
    int_const r1, 10
    int_const r2, 6
    div_int r1, r1, r2
    int_to_real r1, r1
    add_real r0, r0, r1
    load r1, 1
    store_indirect r1, r0
# write ans;
    load r0, 1
    load_indirect r0, r0
    call_builtin print_real
# epilogue
    pop_stack_frame 2
    return
proc_loopMatrix:
# prologue
    push_stack_frame 30
# initialise float val element
    real_const r0, 0.0
    store 0, r0
# initialise int val hi
    int_const r0, 0
    store 1, r0
# initialise int val hi2
    int_const r0, 0
    store 2, r0
# initialise int val lo
    int_const r0, 0
    store 3, r0
# initialise int val lo2
    int_const r0, 0
    store 4, r0
# initialise float val matrix[5,5]
    real_const r0, 0.0
    store 5, r0
    real_const r1, 0.0
    store 6, r1
    real_const r2, 0.0
    store 7, r2
    real_const r3, 0.0
    store 8, r3
    real_const r4, 0.0
    store 9, r4
    real_const r5, 0.0
    store 10, r5
    real_const r6, 0.0
    store 11, r6
    real_const r7, 0.0
    store 12, r7
    real_const r8, 0.0
    store 13, r8
    real_const r9, 0.0
    store 14, r9
    real_const r10, 0.0
    store 15, r10
    real_const r11, 0.0
    store 16, r11
    real_const r12, 0.0
    store 17, r12
    real_const r13, 0.0
    store 18, r13
    real_const r14, 0.0
    store 19, r14
    real_const r15, 0.0
    store 20, r15
    real_const r16, 0.0
    store 21, r16
    real_const r17, 0.0
    store 22, r17
    real_const r18, 0.0
    store 23, r18
    real_const r19, 0.0
    store 24, r19
    real_const r20, 0.0
    store 25, r20
    real_const r21, 0.0
    store 26, r21
    real_const r22, 0.0
    store 27, r22
    real_const r23, 0.0
    store 28, r23
    real_const r24, 0.0
    store 29, r24
# lo := 0;
    int_const r0, 0
    store 3, r0
# hi := 5;
    int_const r0, 5
    store 1, r0
# lo2 := 0;
    int_const r0, 0
    store 4, r0
# hi2 := 5;
    int_const r0, 5
    store 2, r0
# element := 1.1;
    real_const r0, 1.1
    store 0, r0
# while lo < hi
label_0:
    load r0, 3
    load r1, 1
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_1
    branch_uncond label_2
label_1:
# do
# while lo2 < hi2
label_3:
    load r0, 4
    load r1, 2
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_4
    branch_uncond label_5
label_4:
# do
# matrix[lo, lo2] := element;
    load r0, 0
    load r1, 3
    load r2, 4
    int_const r3, 5
    mul_int r1, r3, r1
    add_int r1, r1, r2
    load_address r2, 5
    sub_offset r1, r2, r1
    store_indirect r1, r0
# lo2 := lo2 + 1;
    load r0, 4
    int_const r1, 1
    add_int r0, r0, r1
    store 4, r0
# element := element * 2;
    load r0, 0
    int_const r1, 2
    int_to_real r1, r1
    mul_real r0, r0, r1
    store 0, r0
    branch_uncond label_3
# od
label_5:
# lo := lo + 1;
    load r0, 3
    int_const r1, 1
    add_int r0, r0, r1
    store 3, r0
    branch_uncond label_0
# od
label_2:
# lo := 0;
    int_const r0, 0
    store 3, r0
# hi := 5;
    int_const r0, 5
    store 1, r0
# lo2 := 0;
    int_const r0, 0
    store 4, r0
# hi2 := 5;
    int_const r0, 5
    store 2, r0
# while lo < hi
label_6:
    load r0, 3
    load r1, 1
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_7
    branch_uncond label_8
label_7:
# do
# while lo2 < hi2
label_9:
    load r0, 4
    load r1, 2
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_10
    branch_uncond label_11
label_10:
# do
# write matrix[lo, lo2];
    load r0, 3
    load r1, 4
    int_const r2, 5
    mul_int r0, r2, r0
    add_int r0, r0, r1
    load_address r1, 5
    sub_offset r0, r1, r0
    load_indirect r0, r0
    call_builtin print_real
# lo2 := lo2 + 1;
    load r0, 4
    int_const r1, 1
    add_int r0, r0, r1
    store 4, r0
    branch_uncond label_9
# od
label_11:
# lo := lo + 1;
    load r0, 3
    int_const r1, 1
    add_int r0, r0, r1
    store 3, r0
    branch_uncond label_6
# od
label_8:
# epilogue
    pop_stack_frame 30
    return
