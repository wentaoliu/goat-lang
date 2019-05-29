    call proc_main
    halt
proc_main:
# prologue
    push_stack_frame 3
# initialise int val x
    int_const r0, 0
    store 0, r0
# initialise int val y
    int_const r0, 0
    store 1, r0
# initialise int val z
    int_const r0, 0
    store 2, r0
# write "legs: x ";
    string_const r0, "legs: x "
    call_builtin print_string
# read x;
    call_builtin read_int
    store 0, r0
# write "legs: y ";
    string_const r0, "legs: y "
    call_builtin print_string
# read y;
    call_builtin read_int
    store 1, r0
# write "hypotenuse: z ";
    string_const r0, "hypotenuse: z "
    call_builtin print_string
# read z;
    call_builtin read_int
    store 2, r0
# call test(x, y, z);
    load r0, 0
    load r1, 1
    load r2, 2
    call proc_test
# epilogue
    pop_stack_frame 3
    return
proc_test:
# prologue
    push_stack_frame 3
# parameter passing
    store 0, r0
    store 1, r1
    store 2, r2
# if ((x * x) + (y * y)) = (z * z)
    load r0, 0
    load r1, 0
    mul_int r0, r0, r1
    load r1, 1
    load r2, 1
    mul_int r1, r1, r2
    add_int r0, r0, r1
    load r1, 2
    load r2, 2
    mul_int r1, r1, r2
    cmp_eq_int r0, r0, r1
    branch_on_true r0, label_0
    branch_uncond label_1
label_0:
# then
# write "True\n";
    string_const r0, "True\n"
    call_builtin print_string
    branch_uncond label_2
label_1:
# else
# write "False\n";
    string_const r0, "False\n"
    call_builtin print_string
# fi
label_2:
# epilogue
    pop_stack_frame 3
    return
