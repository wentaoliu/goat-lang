    call proc_main
    halt
proc_main:
# prologue
    push_stack_frame 16
# initialise int val a1
    int_const r0, 0
    store 0, r0
# initialise float val a2
    real_const r0, 0.0
    store 1, r0
# initialise int val arra[3]
    int_const r0, 0
    store 2, r0
    int_const r1, 0
    store 3, r1
    int_const r2, 0
    store 4, r2
# initialise int val b1
    int_const r0, 0
    store 5, r0
# initialise float val b2
    real_const r0, 0.0
    store 6, r0
# initialise int val c1
    int_const r0, 0
    store 7, r0
# initialise int val input_var
    int_const r0, 0
    store 8, r0
# initialise int val mart[2,3]
    int_const r0, 0
    store 9, r0
    int_const r1, 0
    store 10, r1
    int_const r2, 0
    store 11, r2
    int_const r3, 0
    store 12, r3
    int_const r4, 0
    store 13, r4
    int_const r5, 0
    store 14, r5
# initialise bool val result
    int_const r0, 0
    store 15, r0
# read input_var;
    call_builtin read_int
    store 8, r0
# a1 := 1;
    int_const r0, 1
    store 0, r0
# b1 := 2;
    int_const r0, 2
    store 5, r0
# if a1 < b1
    load r0, 0
    load r1, 5
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_0
    branch_uncond label_1
label_0:
# then
    branch_uncond label_3
label_3:
    int_const r0, 1
    store 15, r0
    branch_uncond label_5
label_4:
    int_const r0, 0
    store 15, r0
label_5:
    branch_uncond label_2
label_1:
# else
    branch_uncond label_7
label_6:
    int_const r0, 1
    store 15, r0
    branch_uncond label_8
label_7:
    int_const r0, 0
    store 15, r0
label_8:
# fi
label_2:
# a2 := 1.1;
    real_const r0, 1.1
    store 1, r0
# b2 := 2.2;
    real_const r0, 2.2
    store 6, r0
# c1 := 0;
    int_const r0, 0
    store 7, r0
# while c1 < 3
label_9:
    load r0, 7
    int_const r1, 3
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_10
    branch_uncond label_11
label_10:
# do
# if a1 != b1
    load r0, 0
    load r1, 5
    cmp_ne_int r0, r0, r1
    branch_on_true r0, label_12
    branch_uncond label_13
label_12:
# then
# c1 := c1 + 1;
    load r0, 7
    int_const r1, 1
    add_int r0, r0, r1
    store 7, r0
# fi
label_13:
    branch_uncond label_9
# od
label_11:
# call func1(a1, b1);
    load r0, 0
    load r1, 5
    call proc_func1
# write c1;
    load r0, 7
    call_builtin print_int
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# epilogue
    pop_stack_frame 16
    return
proc_func1:
# prologue
    push_stack_frame 3
# parameter passing
    store 0, r0
    store 1, r1
# initialise int val c
    int_const r0, 0
    store 2, r0
# c := a1 + a2;
    load r0, 0
    load r1, 1
    add_int r0, r0, r1
    store 2, r0
# epilogue
    pop_stack_frame 3
    return
