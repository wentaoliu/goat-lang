    call proc_main
    halt
proc_main:
# prologue
    push_stack_frame 8
# initialise int val a[5]
    int_const r0, 0
    store 0, r0
    int_const r1, 0
    store 1, r1
    int_const r2, 0
    store 2, r2
    int_const r3, 0
    store 3, r3
    int_const r4, 0
    store 4, r4
# initialise int val i
    int_const r0, 0
    store 5, r0
# initialise int val j
    int_const r0, 0
    store 6, r0
# initialise int val len
    int_const r0, 0
    store 7, r0
# len := 5;
    int_const r0, 5
    store 7, r0
# a[0] := 5;
    int_const r0, 5
    int_const r1, 0
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# a[1] := 7;
    int_const r0, 7
    int_const r1, 1
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# a[2] := 9;
    int_const r0, 9
    int_const r1, 2
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# a[3] := -4;
    int_const r0, 4
    neg_int r0, r0
    int_const r1, 3
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# a[4] := 1;
    int_const r0, 1
    int_const r1, 4
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# write "Before Sort: ";
    string_const r0, "Before Sort: "
    call_builtin print_string
# i := 0;
    int_const r0, 0
    store 5, r0
# while i < len
label_0:
    load r0, 5
    load r1, 7
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_1
    branch_uncond label_2
label_1:
# do
# write a[i];
    load r0, 5
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    call_builtin print_int
# if i < (len - 1)
    load r0, 5
    load r1, 7
    int_const r2, 1
    sub_int r1, r1, r2
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_3
    branch_uncond label_4
label_3:
# then
# write ", ";
    string_const r0, ", "
    call_builtin print_string
    branch_uncond label_5
label_4:
# else
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# fi
label_5:
# i := i + 1;
    load r0, 5
    int_const r1, 1
    add_int r0, r0, r1
    store 5, r0
    branch_uncond label_0
# od
label_2:
# i := 0;
    int_const r0, 0
    store 5, r0
# while i < (len - 1)
label_6:
    load r0, 5
    load r1, 7
    int_const r2, 1
    sub_int r1, r1, r2
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_7
    branch_uncond label_8
label_7:
# do
# j := 0;
    int_const r0, 0
    store 6, r0
# while j < ((len - 1) - i)
label_9:
    load r0, 6
    load r1, 7
    int_const r2, 1
    sub_int r1, r1, r2
    load r2, 5
    sub_int r1, r1, r2
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_10
    branch_uncond label_11
label_10:
# do
# if a[j] > a[j + 1]
    load r0, 6
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    load r1, 6
    int_const r2, 1
    add_int r1, r1, r2
    load_address r2, 0
    sub_offset r1, r2, r1
    load_indirect r1, r1
    cmp_gt_int r0, r0, r1
    branch_on_true r0, label_12
    branch_uncond label_13
label_12:
# then
# call exchange(a[j], a[j + 1]);
    load r0, 6
    load_address r1, 0
    sub_offset r0, r1, r0
    load r1, 6
    int_const r2, 1
    add_int r1, r1, r2
    load_address r2, 0
    sub_offset r1, r2, r1
    call proc_exchange
# fi
label_13:
# j := j + 1;
    load r0, 6
    int_const r1, 1
    add_int r0, r0, r1
    store 6, r0
    branch_uncond label_9
# od
label_11:
# i := i + 1;
    load r0, 5
    int_const r1, 1
    add_int r0, r0, r1
    store 5, r0
    branch_uncond label_6
# od
label_8:
# write "After Sort: ";
    string_const r0, "After Sort: "
    call_builtin print_string
# i := 0;
    int_const r0, 0
    store 5, r0
# while i < len
label_14:
    load r0, 5
    load r1, 7
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_15
    branch_uncond label_16
label_15:
# do
# write a[i];
    load r0, 5
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    call_builtin print_int
# if i < (len - 1)
    load r0, 5
    load r1, 7
    int_const r2, 1
    sub_int r1, r1, r2
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_17
    branch_uncond label_18
label_17:
# then
# write ", ";
    string_const r0, ", "
    call_builtin print_string
    branch_uncond label_19
label_18:
# else
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# fi
label_19:
# i := i + 1;
    load r0, 5
    int_const r1, 1
    add_int r0, r0, r1
    store 5, r0
    branch_uncond label_14
# od
label_16:
# epilogue
    pop_stack_frame 8
    return
proc_exchange:
# prologue
    push_stack_frame 3
# parameter passing
    store 1, r0
    store 2, r1
# initialise int val temp
    int_const r0, 0
    store 0, r0
# temp := x;
    load r0, 1
    load_indirect r0, r0
    store 0, r0
# x := y;
    load r0, 2
    load_indirect r0, r0
    load r1, 1
    store_indirect r1, r0
# y := x;
    load r0, 1
    load_indirect r0, r0
    load r1, 2
    store_indirect r1, r0
# epilogue
    pop_stack_frame 3
    return
