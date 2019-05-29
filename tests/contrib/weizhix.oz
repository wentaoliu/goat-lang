    call proc_main
    halt
proc_main:
# prologue
    push_stack_frame 39
# initialise int val a[6,6]
    int_const r0, 0
    store 0, r0
    store 1, r0
    store 2, r0
    store 3, r0
    store 4, r0
    store 5, r0
    store 6, r0
    store 7, r0
    store 8, r0
    store 9, r0
    store 10, r0
    store 11, r0
    store 12, r0
    store 13, r0
    store 14, r0
    store 15, r0
    store 16, r0
    store 17, r0
    store 18, r0
    store 19, r0
    store 20, r0
    store 21, r0
    store 22, r0
    store 23, r0
    store 24, r0
    store 25, r0
    store 26, r0
    store 27, r0
    store 28, r0
    store 29, r0
    store 30, r0
    store 31, r0
    store 32, r0
    store 33, r0
    store 34, r0
    store 35, r0
# initialise int val i
    int_const r0, 0
    store 36, r0
# initialise int val j
    int_const r0, 0
    store 37, r0
# initialise int val n
    int_const r0, 0
    store 38, r0
# read n;
    call_builtin read_int
    store 38, r0
# if (n > 5) || (n <= 10)
    load r0, 38
    int_const r1, 5
    cmp_gt_int r0, r0, r1
    branch_on_true r0, label_0
    branch_uncond label_3
label_3:
    load r0, 38
    int_const r1, 10
    cmp_le_int r0, r0, r1
    branch_on_true r0, label_0
    branch_uncond label_1
label_0:
# then
# write "Result:\n";
    string_const r0, "Result:\n"
    call_builtin print_string
# i := 0;
    int_const r0, 0
    store 36, r0
# while i < n
label_4:
    load r0, 36
    load r1, 38
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_5
    branch_uncond label_6
label_5:
# do
# if i = 0
    load r0, 36
    int_const r1, 0
    cmp_eq_int r0, r0, r1
    branch_on_true r0, label_7
    branch_uncond label_8
label_7:
# then
# a[0, 0] := 1;
    int_const r0, 1
    int_const r1, 0
    int_const r2, 0
    int_const r3, 6
    mul_int r1, r3, r1
    add_int r1, r1, r2
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# write "1\n";
    string_const r0, "1\n"
    call_builtin print_string
    branch_uncond label_9
label_8:
# else
# if i = 1
    load r0, 36
    int_const r1, 1
    cmp_eq_int r0, r0, r1
    branch_on_true r0, label_10
    branch_uncond label_11
label_10:
# then
# a[1, 0] := 1;
    int_const r0, 1
    int_const r1, 1
    int_const r2, 0
    int_const r3, 6
    mul_int r1, r3, r1
    add_int r1, r1, r2
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# a[1, 1] := 1;
    int_const r0, 1
    int_const r1, 1
    int_const r2, 1
    int_const r3, 6
    mul_int r1, r3, r1
    add_int r1, r1, r2
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# write "1 1\n";
    string_const r0, "1 1\n"
    call_builtin print_string
    branch_uncond label_12
label_11:
# else
# a[i, 0] := 1;
    int_const r0, 1
    load r1, 36
    int_const r2, 0
    int_const r3, 6
    mul_int r1, r3, r1
    add_int r1, r1, r2
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# a[i, i] := 1;
    int_const r0, 1
    load r1, 36
    load r2, 36
    int_const r3, 6
    mul_int r1, r3, r1
    add_int r1, r1, r2
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# j := 1;
    int_const r0, 1
    store 37, r0
# while j <= (i - 1)
label_13:
    load r0, 37
    load r1, 36
    int_const r2, 1
    sub_int r1, r1, r2
    cmp_le_int r0, r0, r1
    branch_on_true r0, label_14
    branch_uncond label_15
label_14:
# do
# a[i, j] := a[i - 1, j - 1] + a[i - 1, j];
    load r0, 36
    int_const r1, 1
    sub_int r0, r0, r1
    load r1, 37
    int_const r2, 1
    sub_int r1, r1, r2
    int_const r2, 6
    mul_int r0, r2, r0
    add_int r0, r0, r1
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    load r1, 36
    int_const r2, 1
    sub_int r1, r1, r2
    load r2, 37
    int_const r3, 6
    mul_int r1, r3, r1
    add_int r1, r1, r2
    load_address r2, 0
    sub_offset r1, r2, r1
    load_indirect r1, r1
    add_int r0, r0, r1
    load r1, 36
    load r2, 37
    int_const r3, 6
    mul_int r1, r3, r1
    add_int r1, r1, r2
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# j := j + 1;
    load r0, 37
    int_const r1, 1
    add_int r0, r0, r1
    store 37, r0
    branch_uncond label_13
# od
label_15:
# j := 0;
    int_const r0, 0
    store 37, r0
# while j <= i
label_16:
    load r0, 37
    load r1, 36
    cmp_le_int r0, r0, r1
    branch_on_true r0, label_17
    branch_uncond label_18
label_17:
# do
# write a[i, j];
    load r0, 36
    load r1, 37
    int_const r2, 6
    mul_int r0, r2, r0
    add_int r0, r0, r1
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    call_builtin print_int
# write " ";
    string_const r0, " "
    call_builtin print_string
# j := j + 1;
    load r0, 37
    int_const r1, 1
    add_int r0, r0, r1
    store 37, r0
    branch_uncond label_16
# od
label_18:
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# fi
label_12:
# fi
label_9:
# i := i + 1;
    load r0, 36
    int_const r1, 1
    add_int r0, r0, r1
    store 36, r0
    branch_uncond label_4
# od
label_6:
    branch_uncond label_2
label_1:
# else
# write "Invalid input\n";
    string_const r0, "Invalid input\n"
    call_builtin print_string
# fi
label_2:
# epilogue
    pop_stack_frame 39
    return
