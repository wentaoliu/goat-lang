    call proc_main
    halt
proc_main:
# prologue
    push_stack_frame 2
# initialise int val input
    int_const r0, 0
    store 0, r0
# initialise int val result
    int_const r0, 0
    store 1, r0
# write "Provide an integer value: ";
    string_const r0, "Provide an integer value: "
    call_builtin print_string
# read input;
    call_builtin read_int
    store 0, r0
# call mccarthy(input, result);
    load r0, 0
    load_address r1, 1
    call proc_mccarthy
# write "McCarthy's function applied to ";
    string_const r0, "McCarthy's function applied to "
    call_builtin print_string
# write input;
    load r0, 0
    call_builtin print_int
# write " yields ";
    string_const r0, " yields "
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
proc_mccarthy:
# prologue
    push_stack_frame 3
# parameter passing
    store 0, r0
    store 1, r1
# initialise int val value
    int_const r0, 0
    store 2, r0
# if in > 100
    load r0, 0
    int_const r1, 100
    cmp_gt_int r0, r0, r1
    branch_on_true r0, label_0
    branch_uncond label_1
label_0:
# then
# out := in - 10;
    load r0, 0
    int_const r1, 10
    sub_int r0, r0, r1
    load r1, 1
    store_indirect r1, r0
    branch_uncond label_2
label_1:
# else
# call mccarthy(in + 11, value);
    load r0, 0
    int_const r1, 11
    add_int r0, r0, r1
    load_address r1, 2
    call proc_mccarthy
# call mccarthy(value, out);
    load r0, 2
    load r1, 1
    call proc_mccarthy
# fi
label_2:
# epilogue
    pop_stack_frame 3
    return
