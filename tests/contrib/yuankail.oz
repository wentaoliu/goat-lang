    call proc_main
    halt
proc_main:
# prologue
    push_stack_frame 6
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
# a[0] := 5;
    int_const r0, 5
    int_const r1, 0
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# a[1] := 4;
    int_const r0, 4
    int_const r1, 1
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# a[2] := 3;
    int_const r0, 3
    int_const r1, 2
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# a[3] := 2;
    int_const r0, 2
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
# i := 0;
    int_const r0, 0
    store 5, r0
# call sort(a[0], a[1], a[2], a[3], a[4], 0, 4);
    int_const r0, 0
    load_address r1, 0
    sub_offset r0, r1, r0
    int_const r1, 1
    load_address r2, 0
    sub_offset r1, r2, r1
    int_const r2, 2
    load_address r3, 0
    sub_offset r2, r3, r2
    int_const r3, 3
    load_address r4, 0
    sub_offset r3, r4, r3
    int_const r4, 4
    load_address r5, 0
    sub_offset r4, r5, r4
    int_const r5, 0
    int_const r6, 4
    call proc_sort
# while i < 5
label_0:
    load r0, 5
    int_const r1, 5
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
# i := i + 1;
    load r0, 5
    int_const r1, 1
    add_int r0, r0, r1
    store 5, r0
    branch_uncond label_0
# od
label_2:
# epilogue
    pop_stack_frame 6
    return
proc_sort:
# prologue
    push_stack_frame 15
# parameter passing
    store 0, r0
    store 6, r1
    store 7, r2
    store 8, r3
    store 9, r4
    store 13, r5
    store 10, r6
# initialise int val arr[5]
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
# initialise int val i
    int_const r0, 0
    store 11, r0
# initialise int val j
    int_const r0, 0
    store 12, r0
# initialise int val p
    int_const r0, 0
    store 14, r0
# if lo < hi
    load r0, 13
    load r1, 10
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_3
    branch_uncond label_4
label_3:
# then
# arr[0] := a;
    load r0, 0
    load_indirect r0, r0
    int_const r1, 0
    load_address r2, 1
    sub_offset r1, r2, r1
    store_indirect r1, r0
# arr[1] := b;
    load r0, 6
    load_indirect r0, r0
    int_const r1, 1
    load_address r2, 1
    sub_offset r1, r2, r1
    store_indirect r1, r0
# arr[2] := c;
    load r0, 7
    load_indirect r0, r0
    int_const r1, 2
    load_address r2, 1
    sub_offset r1, r2, r1
    store_indirect r1, r0
# arr[3] := d;
    load r0, 8
    load_indirect r0, r0
    int_const r1, 3
    load_address r2, 1
    sub_offset r1, r2, r1
    store_indirect r1, r0
# arr[4] := e;
    load r0, 9
    load_indirect r0, r0
    int_const r1, 4
    load_address r2, 1
    sub_offset r1, r2, r1
    store_indirect r1, r0
# p := arr[lo];
    load r0, 13
    load_address r1, 1
    sub_offset r0, r1, r0
    load_indirect r0, r0
    store 14, r0
# i := lo;
    load r0, 13
    store 11, r0
# j := hi;
    load r0, 10
    store 12, r0
# while i < j
label_5:
    load r0, 11
    load r1, 12
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_6
    branch_uncond label_7
label_6:
# do
# while (i < hi) && (arr[i] <= p)
label_8:
    load r0, 11
    load r1, 10
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_11
    branch_uncond label_10
label_11:
    load r0, 11
    load_address r1, 1
    sub_offset r0, r1, r0
    load_indirect r0, r0
    load r1, 14
    cmp_le_int r0, r0, r1
    branch_on_true r0, label_9
    branch_uncond label_10
label_9:
# do
# i := i + 1;
    load r0, 11
    int_const r1, 1
    add_int r0, r0, r1
    store 11, r0
    branch_uncond label_8
# od
label_10:
# while (j >= lo) && (arr[j] > p)
label_12:
    load r0, 12
    load r1, 13
    cmp_ge_int r0, r0, r1
    branch_on_true r0, label_15
    branch_uncond label_14
label_15:
    load r0, 12
    load_address r1, 1
    sub_offset r0, r1, r0
    load_indirect r0, r0
    load r1, 14
    cmp_gt_int r0, r0, r1
    branch_on_true r0, label_13
    branch_uncond label_14
label_13:
# do
# j := j - 1;
    load r0, 12
    int_const r1, 1
    sub_int r0, r0, r1
    store 12, r0
    branch_uncond label_12
# od
label_14:
# call swap(arr[i], arr[j]);
    load r0, 11
    load_address r1, 1
    sub_offset r0, r1, r0
    load r1, 12
    load_address r2, 1
    sub_offset r1, r2, r1
    call proc_swap
    branch_uncond label_5
# od
label_7:
# call swap(arr[i], arr[j]);
    load r0, 11
    load_address r1, 1
    sub_offset r0, r1, r0
    load r1, 12
    load_address r2, 1
    sub_offset r1, r2, r1
    call proc_swap
# call swap(arr[lo], arr[j]);
    load r0, 13
    load_address r1, 1
    sub_offset r0, r1, r0
    load r1, 12
    load_address r2, 1
    sub_offset r1, r2, r1
    call proc_swap
# a := arr[0];
    int_const r0, 0
    load_address r1, 1
    sub_offset r0, r1, r0
    load_indirect r0, r0
    load r1, 0
    store_indirect r1, r0
# b := arr[1];
    int_const r0, 1
    load_address r1, 1
    sub_offset r0, r1, r0
    load_indirect r0, r0
    load r1, 6
    store_indirect r1, r0
# c := arr[2];
    int_const r0, 2
    load_address r1, 1
    sub_offset r0, r1, r0
    load_indirect r0, r0
    load r1, 7
    store_indirect r1, r0
# d := arr[3];
    int_const r0, 3
    load_address r1, 1
    sub_offset r0, r1, r0
    load_indirect r0, r0
    load r1, 8
    store_indirect r1, r0
# e := arr[4];
    int_const r0, 4
    load_address r1, 1
    sub_offset r0, r1, r0
    load_indirect r0, r0
    load r1, 9
    store_indirect r1, r0
# call sort(a, b, c, d, e, lo, j - 1);
    load r0, 0
    load r1, 6
    load r2, 7
    load r3, 8
    load r4, 9
    load r5, 13
    load r6, 12
    int_const r7, 1
    sub_int r6, r6, r7
    call proc_sort
# call sort(a, b, c, d, e, j + 1, hi);
    load r0, 0
    load r1, 6
    load r2, 7
    load r3, 8
    load r4, 9
    load r5, 12
    int_const r6, 1
    add_int r5, r5, r6
    load r6, 10
    call proc_sort
# fi
label_4:
# epilogue
    pop_stack_frame 15
    return
proc_swap:
# prologue
    push_stack_frame 3
# parameter passing
    store 0, r0
    store 1, r1
# initialise int val temp
    int_const r0, 0
    store 2, r0
# temp := a;
    load r0, 0
    load_indirect r0, r0
    store 2, r0
# a := b;
    load r0, 1
    load_indirect r0, r0
    load r1, 0
    store_indirect r1, r0
# b := temp;
    load r0, 2
    load r1, 1
    store_indirect r1, r0
# epilogue
    pop_stack_frame 3
    return
