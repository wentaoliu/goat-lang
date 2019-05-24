    call proc_main
    halt
proc_main:
# prologue
    push_stack_frame 2
# initialise int val n
    int_const r0, 0
    store 0, r0
# initialise int val result
    int_const r0, 0
    store 1, r0
# write "Fibonacci calculator using DP.\n";
    string_const r0, "Fibonacci calculator using DP.\n"
    call_builtin print_string
# write "This calculator finds the 12th Fibonacci number.\n";
    string_const r0, "This calculator finds the 12th Fibonacci number.\n"
    call_builtin print_string
# n := 12;
    int_const r0, 12
    store 0, r0
# call fib(n, result);
    load r0, 0
    load_address r1, 1
    call proc_fib
# write "The 12th Fibonacci number = ";
    string_const r0, "The 12th Fibonacci number = "
    call_builtin print_string
# write result;
    load r0, 1
    call_builtin print_int
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# epilogue
    pop_stack_frame 2
    return
proc_fib:
# prologue
    push_stack_frame 17
# parameter passing
    store 15, r0
    store 16, r1
# initialise int val dp[14]
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
    int_const r5, 0
    store 5, r5
    int_const r6, 0
    store 6, r6
    int_const r7, 0
    store 7, r7
    int_const r8, 0
    store 8, r8
    int_const r9, 0
    store 9, r9
    int_const r10, 0
    store 10, r10
    int_const r11, 0
    store 11, r11
    int_const r12, 0
    store 12, r12
    int_const r13, 0
    store 13, r13
# initialise int val i
    int_const r0, 0
    store 14, r0
# dp[0] := 0;
    int_const r0, 0
    int_const r1, 0
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# dp[1] := 1;
    int_const r0, 1
    int_const r1, 1
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# i := 2;
    int_const r0, 2
    store 14, r0
# while i <= n
label_0:
    load r0, 14
    load r1, 15
    cmp_le_int r0, r0, r1
    branch_on_true r0, label_1
    branch_uncond label_2
label_1:
# do
# dp[i] := dp[i - 1] + dp[i - 2];
    load r0, 14
    int_const r1, 1
    sub_int r0, r0, r1
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    load r1, 14
    int_const r2, 2
    sub_int r1, r1, r2
    load_address r2, 0
    sub_offset r1, r2, r1
    load_indirect r1, r1
    add_int r0, r0, r1
    load r1, 14
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# i := i + 1;
    load r0, 14
    int_const r1, 1
    add_int r0, r0, r1
    store 14, r0
    branch_uncond label_0
# od
label_2:
# result := dp[n];
    load r0, 15
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    load r1, 16
    store_indirect r1, r0
# epilogue
    pop_stack_frame 17
    return
