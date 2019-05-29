    call proc_main
    halt
proc_modInplace:
# prologue
    push_stack_frame 3
# parameter passing
    store 0, r0
    store 1, r1
# initialise int val m
    int_const r0, 0
    store 2, r0
# m := a / b;
    load r0, 0
    load_indirect r0, r0
    load r1, 1
    div_int r0, r0, r1
    store 2, r0
# a := a - (m * b);
    load r0, 0
    load_indirect r0, r0
    load r1, 2
    load r2, 1
    mul_int r1, r1, r2
    sub_int r0, r0, r1
    load r1, 0
    store_indirect r1, r0
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
# m := a / b;
    load r0, 0
    load r1, 1
    div_int r0, r0, r1
    load r1, 2
    store_indirect r1, r0
# m := a - (m * b);
    load r0, 0
    load r1, 2
    load_indirect r1, r1
    load r2, 1
    mul_int r1, r1, r2
    sub_int r0, r0, r1
    load r1, 2
    store_indirect r1, r0
# epilogue
    pop_stack_frame 3
    return
proc_powMod:
# prologue
    push_stack_frame 5
# parameter passing
    store 0, r0
    store 3, r1
    store 2, r2
    store 1, r3
# initialise int val tmp
    int_const r0, 0
    store 4, r0
# call modInplace(base, modBy);
    load_address r0, 0
    load r1, 2
    call proc_modInplace
# m := 1;
    int_const r0, 1
    load r1, 1
    store_indirect r1, r0
# while pow > 0
label_0:
    load r0, 3
    int_const r1, 0
    cmp_gt_int r0, r0, r1
    branch_on_true r0, label_1
    branch_uncond label_2
label_1:
# do
# call mod(pow, 2, tmp);
    load r0, 3
    int_const r1, 2
    load_address r2, 4
    call proc_mod
# if tmp > 0
    load r0, 4
    int_const r1, 0
    cmp_gt_int r0, r0, r1
    branch_on_true r0, label_3
    branch_uncond label_4
label_3:
# then
# m := m * base;
    load r0, 1
    load_indirect r0, r0
    load r1, 0
    mul_int r0, r0, r1
    load r1, 1
    store_indirect r1, r0
# call modInplace(m, modBy);
    load r0, 1
    load r1, 2
    call proc_modInplace
# fi
label_4:
# base := base * base;
    load r0, 0
    load r1, 0
    mul_int r0, r0, r1
    store 0, r0
# call modInplace(base, modBy);
    load_address r0, 0
    load r1, 2
    call proc_modInplace
# pow := pow / 2;
    load r0, 3
    int_const r1, 2
    div_int r0, r0, r1
    store 3, r0
    branch_uncond label_0
# od
label_2:
# call modInplace(m, modBy);
    load r0, 1
    load r1, 2
    call proc_modInplace
# epilogue
    pop_stack_frame 5
    return
proc_main:
# prologue
    push_stack_frame 6
# initialise bool val b
    int_const r0, 0
    store 0, r0
# initialise int val base
    int_const r0, 0
    store 1, r0
# initialise float val f
    real_const r0, 0.0
    store 2, r0
# initialise int val i
    int_const r0, 0
    store 3, r0
# initialise int val lastDigit
    int_const r0, 0
    store 4, r0
# initialise int val pow
    int_const r0, 0
    store 5, r0
# i := 90045;
    int_const r0, 90045
    store 3, r0
# f := 21.45;
    real_const r0, 21.45
    store 2, r0
    branch_uncond label_5
label_5:
    int_const r0, 1
    store 0, r0
    branch_uncond label_7
label_6:
    int_const r0, 0
    store 0, r0
label_7:
# write "int value i = ";
    string_const r0, "int value i = "
    call_builtin print_string
# write i;
    load r0, 3
    call_builtin print_int
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# write "float value f = ";
    string_const r0, "float value f = "
    call_builtin print_string
# write f;
    load r0, 2
    call_builtin print_real
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# write "boolean value b = ";
    string_const r0, "boolean value b = "
    call_builtin print_string
# write b;
    load r0, 0
    call_builtin print_bool
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# write "input base (non-negative int): ";
    string_const r0, "input base (non-negative int): "
    call_builtin print_string
# read base;
    call_builtin read_int
    store 1, r0
# write "input pow (positive int): ";
    string_const r0, "input pow (positive int): "
    call_builtin print_string
# read pow;
    call_builtin read_int
    store 5, r0
# call powMod(base, pow, 10, lastDigit);
    load r0, 1
    load r1, 5
    int_const r2, 10
    load_address r3, 4
    call proc_powMod
# write "\n----------------------------------\n";
    string_const r0, "\n----------------------------------\n"
    call_builtin print_string
# write "The last digit of (base ^ pow) is ";
    string_const r0, "The last digit of (base ^ pow) is "
    call_builtin print_string
# write lastDigit;
    load r0, 4
    call_builtin print_int
# write ".\n";
    string_const r0, ".\n"
    call_builtin print_string
# epilogue
    pop_stack_frame 6
    return
