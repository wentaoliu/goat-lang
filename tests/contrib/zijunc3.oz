    call proc_main
    halt
proc_main:
# prologue
    push_stack_frame 3
# initialise int val a1
    int_const r0, 0
    store 0, r0
# initialise int val a2
    int_const r0, 0
    store 1, r0
# initialise int val n
    int_const r0, 0
    store 2, r0
# read n;
    call_builtin read_int
    store 2, r0
# call fibonacci_recursion(n, a1);
    load r0, 2
    load_address r1, 0
    call proc_fibonacci_recursion
# call fibonacci_iteration(n, a2);
    load r0, 2
    load_address r1, 1
    call proc_fibonacci_iteration
# write "a1 = ";
    string_const r0, "a1 = "
    call_builtin print_string
# write a1;
    load r0, 0
    call_builtin print_int
# write ", a2 = ";
    string_const r0, ", a2 = "
    call_builtin print_string
# write a2;
    load r0, 1
    call_builtin print_int
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# epilogue
    pop_stack_frame 3
    return
proc_fibonacci_recursion:
# prologue
    push_stack_frame 4
# parameter passing
    store 1, r0
    store 0, r1
# initialise int val tmp[2]
    int_const r0, 0
    store 2, r0
    int_const r1, 0
    store 3, r1
# if n < 0
    load r0, 1
    int_const r1, 0
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_0
    branch_uncond label_1
label_0:
# then
# write "Incorrect input\n";
    string_const r0, "Incorrect input\n"
    call_builtin print_string
    branch_uncond label_2
label_1:
# else
# if n <= 2
    load r0, 1
    int_const r1, 2
    cmp_le_int r0, r0, r1
    branch_on_true r0, label_3
    branch_uncond label_4
label_3:
# then
# ans := 1;
    int_const r0, 1
    load r1, 0
    store_indirect r1, r0
    branch_uncond label_5
label_4:
# else
# call fibonacci_recursion(n - 1, tmp[0]);
    load r0, 1
    int_const r1, 1
    sub_int r0, r0, r1
    int_const r1, 0
    load_address r2, 2
    sub_offset r1, r2, r1
    call proc_fibonacci_recursion
# call fibonacci_recursion(n - 1, tmp[1]);
    load r0, 1
    int_const r1, 1
    sub_int r0, r0, r1
    int_const r1, 1
    load_address r2, 2
    sub_offset r1, r2, r1
    call proc_fibonacci_recursion
# ans := tmp[0] + tmp[1];
    int_const r0, 0
    load_address r1, 2
    sub_offset r0, r1, r0
    load_indirect r0, r0
    int_const r1, 1
    load_address r2, 2
    sub_offset r1, r2, r1
    load_indirect r1, r1
    add_int r0, r0, r1
    load r1, 0
    store_indirect r1, r0
# fi
label_5:
# fi
label_2:
# epilogue
    pop_stack_frame 4
    return
proc_fibonacci_iteration:
# prologue
    push_stack_frame 6
# parameter passing
    store 5, r0
    store 1, r1
# initialise int val a
    int_const r0, 0
    store 0, r0
# initialise int val b
    int_const r0, 0
    store 2, r0
# initialise int val c
    int_const r0, 0
    store 3, r0
# initialise int val i
    int_const r0, 0
    store 4, r0
# if n < 0
    load r0, 5
    int_const r1, 0
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_6
    branch_uncond label_7
label_6:
# then
# write "Incorrect input\n";
    string_const r0, "Incorrect input\n"
    call_builtin print_string
    branch_uncond label_8
label_7:
# else
# if n <= 2
    load r0, 5
    int_const r1, 2
    cmp_le_int r0, r0, r1
    branch_on_true r0, label_9
    branch_uncond label_10
label_9:
# then
# ans := 1;
    int_const r0, 1
    load r1, 1
    store_indirect r1, r0
    branch_uncond label_11
label_10:
# else
# a := 1;
    int_const r0, 1
    store 0, r0
# b := 1;
    int_const r0, 1
    store 2, r0
# i := 2;
    int_const r0, 2
    store 4, r0
# while i < n
label_12:
    load r0, 4
    load r1, 5
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_13
    branch_uncond label_14
label_13:
# do
# c := a + b;
    load r0, 0
    load r1, 2
    add_int r0, r0, r1
    store 3, r0
# a := b;
    load r0, 2
    store 0, r0
# b := c;
    load r0, 3
    store 2, r0
# i := i + 1;
    load r0, 4
    int_const r1, 1
    add_int r0, r0, r1
    store 4, r0
    branch_uncond label_12
# od
label_14:
# ans := c;
    load r0, 3
    load r1, 1
    store_indirect r1, r0
# fi
label_11:
# fi
label_8:
# epilogue
    pop_stack_frame 6
    return
