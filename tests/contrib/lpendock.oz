    call proc_main
    halt
proc_modulo:
# prologue
    push_stack_frame 4
# parameter passing
    store 3, r0
    store 1, r1
    store 2, r2
# initialise int val i
    int_const r0, 0
    store 0, r0
# i := 0;
    int_const r0, 0
    store 0, r0
# while i < x
label_0:
    load r0, 0
    load r1, 3
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_1
    branch_uncond label_2
label_1:
# do
# i := i + n;
    load r0, 0
    load r1, 1
    add_int r0, r0, r1
    store 0, r0
    branch_uncond label_0
# od
label_2:
# out := (n - i) + x;
    load r0, 1
    load r1, 0
    sub_int r0, r0, r1
    load r1, 3
    add_int r0, r0, r1
    load r1, 2
    store_indirect r1, r0
# epilogue
    pop_stack_frame 4
    return
proc_mod_pow:
# prologue
    push_stack_frame 5
# parameter passing
    store 4, r0
    store 0, r1
    store 2, r2
    store 3, r3
# initialise int val i
    int_const r0, 0
    store 1, r0
# r := 1;
    int_const r0, 1
    load r1, 3
    store_indirect r1, r0
# i := 0;
    int_const r0, 0
    store 1, r0
# while i < e
label_3:
    load r0, 1
    load r1, 0
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_4
    branch_uncond label_5
label_4:
# do
# r := r * x;
    load r0, 3
    load_indirect r0, r0
    load r1, 4
    mul_int r0, r0, r1
    load r1, 3
    store_indirect r1, r0
# call modulo(r, n, r);
    load r0, 3
    load_indirect r0, r0
    load r1, 2
    load r2, 3
    call proc_modulo
# i := i + 1;
    load r0, 1
    int_const r1, 1
    add_int r0, r0, r1
    store 1, r0
    branch_uncond label_3
# od
label_5:
# epilogue
    pop_stack_frame 5
    return
proc_modular_inverse:
# prologue
    push_stack_frame 8
# parameter passing
    store 0, r0
    store 1, r1
    store 3, r2
# initialise int val b0
    int_const r0, 0
    store 2, r0
# initialise int val q
    int_const r0, 0
    store 4, r0
# initialise int val t
    int_const r0, 0
    store 5, r0
# initialise int val x0
    int_const r0, 0
    store 6, r0
# initialise int val x1
    int_const r0, 0
    store 7, r0
# x0 := 0;
    int_const r0, 0
    store 6, r0
# x1 := 1;
    int_const r0, 1
    store 7, r0
# if b = 0
    load r0, 1
    int_const r1, 0
    cmp_eq_int r0, r0, r1
    branch_on_true r0, label_6
    branch_uncond label_7
label_6:
# then
# out := 1;
    int_const r0, 1
    load r1, 3
    store_indirect r1, r0
    branch_uncond label_8
label_7:
# else
# while a > 1
label_9:
    load r0, 0
    int_const r1, 1
    cmp_gt_int r0, r0, r1
    branch_on_true r0, label_10
    branch_uncond label_11
label_10:
# do
# q := a / b;
    load r0, 0
    load r1, 1
    div_int r0, r0, r1
    store 4, r0
# t := b;
    load r0, 1
    store 5, r0
# call modulo(a, b, b);
    load r0, 0
    load r1, 1
    load_address r2, 1
    call proc_modulo
# a := t;
    load r0, 5
    store 0, r0
# t := x0;
    load r0, 6
    store 5, r0
# x0 := x1 - (q * x0);
    load r0, 7
    load r1, 4
    load r2, 6
    mul_int r1, r1, r2
    sub_int r0, r0, r1
    store 6, r0
# x1 := t;
    load r0, 5
    store 7, r0
    branch_uncond label_9
# od
label_11:
# if x1 < 0
    load r0, 7
    int_const r1, 0
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_12
    branch_uncond label_13
label_12:
# then
# x1 := x1 + b0;
    load r0, 7
    load r1, 2
    add_int r0, r0, r1
    store 7, r0
# fi
label_13:
# out := x1;
    load r0, 7
    load r1, 3
    store_indirect r1, r0
# fi
label_8:
# epilogue
    pop_stack_frame 8
    return
proc_sum_digits:
# prologue
    push_stack_frame 4
# parameter passing
    store 0, r0
    store 1, r1
# initialise int val sum
    int_const r0, 0
    store 2, r0
# initialise int val t
    int_const r0, 0
    store 3, r0
# sum := 0;
    int_const r0, 0
    store 2, r0
# while n > 0
label_14:
    load r0, 0
    int_const r1, 0
    cmp_gt_int r0, r0, r1
    branch_on_true r0, label_15
    branch_uncond label_16
label_15:
# do
# call modulo(n, 10, t);
    load r0, 0
    int_const r1, 10
    load_address r2, 3
    call proc_modulo
# sum := sum + t;
    load r0, 2
    load r1, 3
    add_int r0, r0, r1
    store 2, r0
# n := (n - t) / 10;
    load r0, 0
    load r1, 3
    sub_int r0, r0, r1
    int_const r1, 10
    div_int r0, r0, r1
    store 0, r0
    branch_uncond label_14
# od
label_16:
# out := sum;
    load r0, 2
    load r1, 1
    store_indirect r1, r0
# epilogue
    pop_stack_frame 4
    return
proc_main:
# prologue
    push_stack_frame 1
# initialise int val m
    int_const r0, 0
    store 0, r0
# call modulo(34, 7, m);
    int_const r0, 34
    int_const r1, 7
    load_address r2, 0
    call proc_modulo
# write m;
    load r0, 0
    call_builtin print_int
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# call mod_pow(43, 13, 3, m);
    int_const r0, 43
    int_const r1, 13
    int_const r2, 3
    load_address r3, 0
    call proc_mod_pow
# write m;
    load r0, 0
    call_builtin print_int
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# call modular_inverse(476, 1453, m);
    int_const r0, 476
    int_const r1, 1453
    load_address r2, 0
    call proc_modular_inverse
# write m;
    load r0, 0
    call_builtin print_int
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# call sum_digits(1794675, m);
    int_const r0, 1794675
    load_address r1, 0
    call proc_sum_digits
# write m;
    load r0, 0
    call_builtin print_int
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# epilogue
    pop_stack_frame 1
    return
