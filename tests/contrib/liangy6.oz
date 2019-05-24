    call proc_main
    halt
proc_f:
# prologue
    push_stack_frame 1
# parameter passing
    store 0, r0
# x := x * (x + 1);
    load r0, 0
    load_indirect r0, r0
    load r1, 0
    load_indirect r1, r1
    int_const r2, 1
    int_to_real r2, r2
    add_real r1, r1, r2
    mul_real r0, r0, r1
    load r1, 0
    store_indirect r1, r0
# if x > 100
    load r0, 0
    load_indirect r0, r0
    int_const r1, 100
    int_to_real r1, r1
    cmp_gt_real r0, r0, r1
    branch_on_true r0, label_0
    branch_uncond label_1
label_0:
# then
# write "more than 100\n";
    string_const r0, "more than 100\n"
    call_builtin print_string
# fi
label_1:
# write x;
    load r0, 0
    load_indirect r0, r0
    call_builtin print_real
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# epilogue
    pop_stack_frame 1
    return
proc_g:
# prologue
    push_stack_frame 1
# parameter passing
    store 0, r0
# y := y - 1;
    load r0, 0
    load_indirect r0, r0
    int_const r1, 1
    int_to_real r1, r1
    sub_real r0, r0, r1
    load r1, 0
    store_indirect r1, r0
# write y;
    load r0, 0
    load_indirect r0, r0
    call_builtin print_real
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# epilogue
    pop_stack_frame 1
    return
proc_main:
# prologue
    push_stack_frame 3
# initialise int val a[1]
    int_const r0, 0
    store 0, r0
# initialise float val m
    real_const r0, 0.0
    store 1, r0
# initialise float val n
    real_const r0, 0.0
    store 2, r0
# n := 0;
    int_const r0, 0
    int_to_real r0, r0
    store 2, r0
# m := 15;
    int_const r0, 15
    int_to_real r0, r0
    store 1, r0
# a[0] := 0;
    int_const r0, 0
    int_const r1, 0
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# while m > n
label_2:
    load r0, 1
    load r1, 2
    cmp_gt_real r0, r0, r1
    branch_on_true r0, label_3
    branch_uncond label_4
label_3:
# do
# call g(m);
    load_address r0, 1
    call proc_g
# call f(n);
    load_address r0, 2
    call proc_f
# a[0] := a[0] + 1;
    int_const r0, 0
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    int_const r1, 1
    add_int r0, r0, r1
    int_const r1, 0
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# write m;
    load r0, 1
    call_builtin print_real
# write ", ";
    string_const r0, ", "
    call_builtin print_string
# write n;
    load r0, 2
    call_builtin print_real
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
    branch_uncond label_2
# od
label_4:
# if a[0] < 4
    int_const r0, 0
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    int_const r1, 4
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_5
    branch_uncond label_6
label_5:
# then
# write "Player_2 wins!\n";
    string_const r0, "Player_2 wins!\n"
    call_builtin print_string
    branch_uncond label_7
label_6:
# else
# write "Player_1 win!\n";
    string_const r0, "Player_1 win!\n"
    call_builtin print_string
# fi
label_7:
# epilogue
    pop_stack_frame 3
    return
