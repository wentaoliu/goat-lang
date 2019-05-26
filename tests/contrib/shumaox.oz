    call proc_main
    halt
proc_main:
# prologue
    push_stack_frame 6
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
# initialise int val e
    int_const r0, 0
    store 4, r0
# initialise int val temp
    int_const r0, 0
    store 5, r0
# a := 2;
    int_const r0, 2
    store 0, r0
# write a;
    load r0, 0
    call_builtin print_int
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# b := 4;
    int_const r0, 4
    store 1, r0
# write "Integer: ";
    string_const r0, "Integer: "
    call_builtin print_string
# read b;
    call_builtin read_int
    store 1, r0
# write b;
    load r0, 1
    call_builtin print_int
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# c := ((a + b) + (2 * 2)) + ((20 + 20) / 2);
    load r0, 0
    load r1, 1
    add_int r0, r0, r1
    int_const r1, 2
    int_const r2, 2
    mul_int r1, r1, r2
    add_int r0, r0, r1
    int_const r1, 20
    int_const r2, 20
    add_int r1, r1, r2
    int_const r2, 2
    div_int r1, r1, r2
    add_int r0, r0, r1
    store 2, r0
# write c;
    load r0, 2
    call_builtin print_int
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# d := 1;
    int_const r0, 1
    store 3, r0
# e := 2;
    int_const r0, 2
    store 4, r0
# temp := e;
    load r0, 4
    store 5, r0
# if e < d
    load r0, 4
    load r1, 3
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_0
    branch_uncond label_1
label_0:
# then
# temp := d;
    load r0, 3
    store 5, r0
# fi
label_1:
# write "The bigger number is ";
    string_const r0, "The bigger number is "
    call_builtin print_string
# write temp;
    load r0, 5
    call_builtin print_int
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# epilogue
    pop_stack_frame 6
    return
