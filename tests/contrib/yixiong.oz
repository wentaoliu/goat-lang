    call proc_main
    halt
proc_drawA:
# prologue
    push_stack_frame 3
# parameter passing
    store 2, r0
# initialise int val i
    int_const r0, 0
    store 0, r0
# initialise int val j
    int_const r0, 0
    store 1, r0
# i := 0;
    int_const r0, 0
    store 0, r0
# while i < n
label_0:
    load r0, 0
    load r1, 2
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_1
    branch_uncond label_2
label_1:
# do
# j := 0;
    int_const r0, 0
    store 1, r0
# while j <= (n / 2)
label_3:
    load r0, 1
    load r1, 2
    int_const r2, 2
    div_int r1, r1, r2
    cmp_le_int r0, r0, r1
    branch_on_true r0, label_4
    branch_uncond label_5
label_4:
# do
# if ((((j = 0) || (j = (n / 2))) && (i != 0)) || (((i = 0) && (j != 0)) && (j != (n / 2)))) || (i = (n / 2))
    load r0, 1
    int_const r1, 0
    cmp_eq_int r0, r0, r1
    branch_on_true r0, label_11
    branch_uncond label_12
label_12:
    load r0, 1
    load r1, 2
    int_const r2, 2
    div_int r1, r1, r2
    cmp_eq_int r0, r0, r1
    branch_on_true r0, label_11
    branch_uncond label_10
label_11:
    load r0, 0
    int_const r1, 0
    cmp_ne_int r0, r0, r1
    branch_on_true r0, label_6
    branch_uncond label_10
label_10:
    load r0, 0
    int_const r1, 0
    cmp_eq_int r0, r0, r1
    branch_on_true r0, label_14
    branch_uncond label_9
label_14:
    load r0, 1
    int_const r1, 0
    cmp_ne_int r0, r0, r1
    branch_on_true r0, label_13
    branch_uncond label_9
label_13:
    load r0, 1
    load r1, 2
    int_const r2, 2
    div_int r1, r1, r2
    cmp_ne_int r0, r0, r1
    branch_on_true r0, label_6
    branch_uncond label_9
label_9:
    load r0, 0
    load r1, 2
    int_const r2, 2
    div_int r1, r1, r2
    cmp_eq_int r0, r0, r1
    branch_on_true r0, label_6
    branch_uncond label_7
label_6:
# then
# write "*";
    string_const r0, "*"
    call_builtin print_string
    branch_uncond label_8
label_7:
# else
# write " ";
    string_const r0, " "
    call_builtin print_string
# fi
label_8:
# j := j + 1;
    load r0, 1
    int_const r1, 1
    add_int r0, r0, r1
    store 1, r0
    branch_uncond label_3
# od
label_5:
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
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
proc_main:
# prologue
    push_stack_frame 0
# call drawA(5);
    int_const r0, 5
    call proc_drawA
# call drawA(10);
    int_const r0, 10
    call proc_drawA
# epilogue
    pop_stack_frame 0
    return
