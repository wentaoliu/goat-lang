    call proc_main
    halt
proc_main:
# prologue
    push_stack_frame 5
# initialise int val a
    int_const r0, 0
    store 0, r0
# initialise int val b
    int_const r0, 0
    store 1, r0
# initialise int val c
    int_const r0, 0
    store 2, r0
# initialise int val d
    int_const r0, 0
    store 3, r0
# initialise int val result
    int_const r0, 0
    store 4, r0
# write "This program is going to calculate the value of ((a+b)*c)/d.";
    string_const r0, "This program is going to calculate the value of ((a+b)*c)/d."
    call_builtin print_string
# write "\nPlease type in the value of a: ";
    string_const r0, "\nPlease type in the value of a: "
    call_builtin print_string
# read a;
    call_builtin read_int
    store 0, r0
# write "\nPlease type in the value of b: ";
    string_const r0, "\nPlease type in the value of b: "
    call_builtin print_string
# read b;
    call_builtin read_int
    store 1, r0
# write "\nPlease type in the value of c: ";
    string_const r0, "\nPlease type in the value of c: "
    call_builtin print_string
# read c;
    call_builtin read_int
    store 2, r0
# write "\nPlease type in the value of d: ";
    string_const r0, "\nPlease type in the value of d: "
    call_builtin print_string
# read d;
    call_builtin read_int
    store 3, r0
# call calculate(a, b, c, d, result);
    load r0, 0
    load r1, 1
    load r2, 2
    load r3, 3
    load_address r4, 4
    call proc_calculate
# write "\nThe result is: ";
    string_const r0, "\nThe result is: "
    call_builtin print_string
# write result;
    load r0, 4
    call_builtin print_int
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# epilogue
    pop_stack_frame 5
    return
proc_calculate:
# prologue
    push_stack_frame 5
# parameter passing
    store 0, r0
    store 1, r1
    store 2, r2
    store 3, r3
    store 4, r4
# if d = 0
    load r0, 3
    int_const r1, 0
    cmp_eq_int r0, r0, r1
    branch_on_true r0, label_0
    branch_uncond label_1
label_0:
# then
# out := 0;
    int_const r0, 0
    load r1, 4
    store_indirect r1, r0
    branch_uncond label_2
label_1:
# else
# out := ((a + b) * c) / d;
    load r0, 0
    load r1, 1
    add_int r0, r0, r1
    load r1, 2
    mul_int r0, r0, r1
    load r1, 3
    div_int r0, r0, r1
    load r1, 4
    store_indirect r1, r0
# fi
label_2:
# epilogue
    pop_stack_frame 5
    return
