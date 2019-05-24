    call proc_main
    halt
proc_main:
# prologue
    push_stack_frame 1
# initialise int val number
    int_const r0, 0
    store 0, r0
# write "Give an integer:\n";
    string_const r0, "Give an integer:\n"
    call_builtin print_string
# read number;
    call_builtin read_int
    store 0, r0
# write "The factors are:\n";
    string_const r0, "The factors are:\n"
    call_builtin print_string
# call factors(number);
    load r0, 0
    call proc_factors
# epilogue
    pop_stack_frame 1
    return
proc_factors:
# prologue
    push_stack_frame 3
# parameter passing
    store 1, r0
# initialise int val i
    int_const r0, 0
    store 0, r0
# initialise int val r
    int_const r0, 0
    store 2, r0
# i := 1;
    int_const r0, 1
    store 0, r0
# while i <= n
label_0:
    load r0, 0
    load r1, 1
    cmp_le_int r0, r0, r1
    branch_on_true r0, label_1
    branch_uncond label_2
label_1:
# do
# call mod(n, i, r);
    load r0, 1
    load r1, 0
    load_address r2, 2
    call proc_mod
# if r = 0
    load r0, 2
    int_const r1, 0
    cmp_eq_int r0, r0, r1
    branch_on_true r0, label_3
    branch_uncond label_4
label_3:
# then
# write i;
    load r0, 0
    call_builtin print_int
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# fi
label_4:
# i := i + 1;
    load r0, 0
    int_const r1, 1
    add_int r0, r0, r1
    store 0, r0
    branch_uncond label_0
# od
label_2:
# epilogue
    pop_stack_frame 3
    return
proc_mod:
# prologue
    push_stack_frame 3
# parameter passing
    store 0, r0
    store 1, r1
    store 2, r2
# r := a - ((a / b) * b);
    load r0, 0
    load r1, 0
    load r2, 1
    div_int r1, r1, r2
    load r2, 1
    mul_int r1, r1, r2
    sub_int r0, r0, r1
    load r1, 2
    store_indirect r1, r0
# epilogue
    pop_stack_frame 3
    return
