    call proc_main
    halt
proc_main:
# prologue
    push_stack_frame 2
# initialise int val i
    int_const r0, 0
    store 0, r0
# initialise int val n
    int_const r0, 0
    store 1, r0
# write "Enter number of line: ";
    string_const r0, "Enter number of line: "
    call_builtin print_string
# read n;
    call_builtin read_int
    store 1, r0
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# while n > 0
label_0:
    load r0, 1
    int_const r1, 0
    cmp_gt_int r0, r0, r1
    branch_on_true r0, label_1
    branch_uncond label_2
label_1:
# do
# i := 0;
    int_const r0, 0
    store 0, r0
# while i < n
label_3:
    load r0, 0
    load r1, 1
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_4
    branch_uncond label_5
label_4:
# do
# write "*";
    string_const r0, "*"
    call_builtin print_string
# i := i + 1;
    load r0, 0
    int_const r1, 1
    add_int r0, r0, r1
    store 0, r0
    branch_uncond label_3
# od
label_5:
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# n := n - 1;
    load r0, 1
    int_const r1, 1
    sub_int r0, r0, r1
    store 1, r0
    branch_uncond label_0
# od
label_2:
# epilogue
    pop_stack_frame 2
    return
