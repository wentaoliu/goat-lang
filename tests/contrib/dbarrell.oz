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
# write "Calculates the n-th number in the Fibonacci sequence\n";
    string_const r0, "Calculates the n-th number in the Fibonacci sequence\n"
    call_builtin print_string
# write "Enter n (<= 45): \n";
    string_const r0, "Enter n (<= 45): \n"
    call_builtin print_string
# read n;
    call_builtin read_int
    store 0, r0
# if n <= 45
    load r0, 0
    int_const r1, 45
    cmp_le_int r0, r0, r1
    branch_on_true r0, label_0
    branch_uncond label_1
label_0:
# then
# call fib(n, result);
    load r0, 0
    load_address r1, 1
    call proc_fib
# write result;
    load r0, 1
    call_builtin print_int
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
    branch_uncond label_2
label_1:
# else
# write "Number must be less than or equal to 45\n";
    string_const r0, "Number must be less than or equal to 45\n"
    call_builtin print_string
# fi
label_2:
# epilogue
    pop_stack_frame 2
    return
proc_fib:
# prologue
    push_stack_frame 48
# parameter passing
    store 46, r0
    store 47, r1
# initialise int val f[45]
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
    int_const r14, 0
    store 14, r14
    int_const r15, 0
    store 15, r15
    int_const r16, 0
    store 16, r16
    int_const r17, 0
    store 17, r17
    int_const r18, 0
    store 18, r18
    int_const r19, 0
    store 19, r19
    int_const r20, 0
    store 20, r20
    int_const r21, 0
    store 21, r21
    int_const r22, 0
    store 22, r22
    int_const r23, 0
    store 23, r23
    int_const r24, 0
    store 24, r24
    int_const r25, 0
    store 25, r25
    int_const r26, 0
    store 26, r26
    int_const r27, 0
    store 27, r27
    int_const r28, 0
    store 28, r28
    int_const r29, 0
    store 29, r29
    int_const r30, 0
    store 30, r30
    int_const r31, 0
    store 31, r31
    int_const r32, 0
    store 32, r32
    int_const r33, 0
    store 33, r33
    int_const r34, 0
    store 34, r34
    int_const r35, 0
    store 35, r35
    int_const r36, 0
    store 36, r36
    int_const r37, 0
    store 37, r37
    int_const r38, 0
    store 38, r38
    int_const r39, 0
    store 39, r39
    int_const r40, 0
    store 40, r40
    int_const r41, 0
    store 41, r41
    int_const r42, 0
    store 42, r42
    int_const r43, 0
    store 43, r43
    int_const r44, 0
    store 44, r44
# initialise int val i
    int_const r0, 0
    store 45, r0
# f[0] := 0;
    int_const r0, 0
    int_const r1, 0
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# f[1] := 1;
    int_const r0, 1
    int_const r1, 1
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# i := 2;
    int_const r0, 2
    store 45, r0
# while i <= n
label_3:
    load r0, 45
    load r1, 46
    cmp_le_int r0, r0, r1
    branch_on_true r0, label_4
    branch_uncond label_5
label_4:
# do
# f[i] := f[i - 1] + f[i - 2];
    load r0, 45
    int_const r1, 1
    sub_int r0, r0, r1
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    load r1, 45
    int_const r2, 2
    sub_int r1, r1, r2
    load_address r2, 0
    sub_offset r1, r2, r1
    load_indirect r1, r1
    add_int r0, r0, r1
    load r1, 45
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# i := i + 1;
    load r0, 45
    int_const r1, 1
    add_int r0, r0, r1
    store 45, r0
    branch_uncond label_3
# od
label_5:
# result := f[n];
    load r0, 46
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    load r1, 47
    store_indirect r1, r0
# epilogue
    pop_stack_frame 48
    return
