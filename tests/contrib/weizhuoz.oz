    call proc_main
    halt
proc_initFloatValue:
# prologue
    push_stack_frame 1
# parameter passing
    store 0, r0
# value := 0.0;
    real_const r0, 0.0
    load r1, 0
    store_indirect r1, r0
# epilogue
    pop_stack_frame 1
    return
proc_initIntValue:
# prologue
    push_stack_frame 1
# parameter passing
    store 0, r0
# value := 0;
    int_const r0, 0
    load r1, 0
    store_indirect r1, r0
# epilogue
    pop_stack_frame 1
    return
proc_main:
# prologue
    push_stack_frame 14
# initialise int val count
    int_const r0, 0
    store 0, r0
# initialise int val count2
    int_const r0, 0
    store 1, r0
# initialise float val floatMat[2,3]
    real_const r0, 0.0
    store 2, r0
    real_const r1, 0.0
    store 3, r1
    real_const r2, 0.0
    store 4, r2
    real_const r3, 0.0
    store 5, r3
    real_const r4, 0.0
    store 6, r4
    real_const r5, 0.0
    store 7, r5
# initialise int val intList[3]
    int_const r0, 0
    store 8, r0
    int_const r1, 0
    store 9, r1
    int_const r2, 0
    store 10, r2
# initialise float val x
    real_const r0, 0.0
    store 11, r0
# initialise int val y
    int_const r0, 0
    store 12, r0
# initialise bool val z
    int_const r0, 0
    store 13, r0
# write "let's test\n";
    string_const r0, "let's test\n"
    call_builtin print_string
# count := 0;
    int_const r0, 0
    store 0, r0
# while count < 3
label_0:
    load r0, 0
    int_const r1, 3
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_1
    branch_uncond label_2
label_1:
# do
# call initIntValue(intList[count]);
    load r0, 0
    load_address r1, 8
    sub_offset r0, r1, r0
    call proc_initIntValue
# count := count + 1;
    load r0, 0
    int_const r1, 1
    add_int r0, r0, r1
    store 0, r0
    branch_uncond label_0
# od
label_2:
# count := 0;
    int_const r0, 0
    store 0, r0
# count2 := 0;
    int_const r0, 0
    store 1, r0
# while count < 2
label_3:
    load r0, 0
    int_const r1, 2
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_4
    branch_uncond label_5
label_4:
# do
# while count2 < 3
label_6:
    load r0, 1
    int_const r1, 3
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_7
    branch_uncond label_8
label_7:
# do
# call initFloatValue(floatMat[count, count2]);
    load r0, 0
    load r1, 1
    int_const r2, 3
    mul_int r0, r2, r0
    add_int r0, r1, r0
    load_address r1, 2
    sub_offset r0, r1, r0
    call proc_initFloatValue
# count2 := count2 + 1;
    load r0, 1
    int_const r1, 1
    add_int r0, r0, r1
    store 1, r0
    branch_uncond label_6
# od
label_8:
# count := count + 1;
    load r0, 0
    int_const r1, 1
    add_int r0, r0, r1
    store 0, r0
    branch_uncond label_3
# od
label_5:
# x := 3.5;
    real_const r0, 3.5
    store 11, r0
# write "please input y: ";
    string_const r0, "please input y: "
    call_builtin print_string
# read y;
    call_builtin read_int
    store 12, r0
    load r0, 12
    int_const r1, 10
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_9
    branch_uncond label_10
label_9:
    int_const r0, 1
    store 13, r0
    branch_uncond label_11
label_10:
    int_const r0, 0
    store 13, r0
label_11:
# while z
label_12:
    load r0, 13
    branch_on_true r0, label_13
    branch_uncond label_14
label_13:
# do
# y := y + 1;
    load r0, 12
    int_const r1, 1
    add_int r0, r0, r1
    store 12, r0
    load r0, 12
    int_const r1, 10
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_15
    branch_uncond label_16
label_15:
    int_const r0, 1
    store 13, r0
    branch_uncond label_17
label_16:
    int_const r0, 0
    store 13, r0
label_17:
    branch_uncond label_12
# od
label_14:
# epilogue
    pop_stack_frame 14
    return
