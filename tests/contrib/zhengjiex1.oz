    call proc_main
    halt
proc_main:
# prologue
    push_stack_frame 4
# initialise int val a
    int_const r0, 0
    store 0, r0
# initialise int val b[3]
    int_const r0, 0
    store 1, r0
    store 2, r0
    store 3, r0
# if true
    branch_uncond label_0
label_0:
# then
# call p(a, b[2]);
    load r0, 0
    int_to_real r0, r0
    int_const r1, 2
    load_address r2, 1
    sub_offset r1, r2, r1
    call proc_p
# fi
label_1:
# epilogue
    pop_stack_frame 4
    return
proc_p:
# prologue
    push_stack_frame 5
# parameter passing
    store 3, r0
    store 4, r1
# initialise int val a
    int_const r0, 0
    store 0, r0
# initialise bool val b1
    int_const r0, 0
    store 1, r0
# initialise bool val b2
    int_const r0, 0
    store 2, r0
# if b1
    load r0, 1
    branch_on_true r0, label_2
    branch_uncond label_3
label_2:
# then
# write "i\n";
    string_const r0, "i\n"
    call_builtin print_string
# if b2
    load r0, 2
    branch_on_true r0, label_5
    branch_uncond label_6
label_5:
# then
# while b1
label_7:
    load r0, 1
    branch_on_true r0, label_8
    branch_uncond label_9
label_8:
# do
# write "iw";
    string_const r0, "iw"
    call_builtin print_string
# if b2
    load r0, 2
    branch_on_true r0, label_10
    branch_uncond label_11
label_10:
# then
# write "iwi";
    string_const r0, "iwi"
    call_builtin print_string
# fi
label_11:
    branch_uncond label_7
# od
label_9:
# fi
label_6:
    branch_uncond label_4
label_3:
# else
# write "e";
    string_const r0, "e"
    call_builtin print_string
# if b2
    load r0, 2
    branch_on_true r0, label_12
    branch_uncond label_13
label_12:
# then
# write "ei";
    string_const r0, "ei"
    call_builtin print_string
    branch_uncond label_14
label_13:
# else
# while b1
label_15:
    load r0, 1
    branch_on_true r0, label_16
    branch_uncond label_17
label_16:
# do
# write "eew";
    string_const r0, "eew"
    call_builtin print_string
    branch_uncond label_15
# od
label_17:
# fi
label_14:
# write "e";
    string_const r0, "e"
    call_builtin print_string
# fi
label_4:
# write "end";
    string_const r0, "end"
    call_builtin print_string
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# epilogue
    pop_stack_frame 5
    return
