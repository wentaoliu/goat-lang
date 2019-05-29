    call proc_main
    halt
proc_main:
# prologue
    push_stack_frame 7
# initialise bool val flag
    int_const r0, 0
    store 0, r0
# initialise int val i
    int_const r0, 0
    store 1, r0
# initialise int val q1
    int_const r0, 0
    store 2, r0
# initialise int val q2
    int_const r0, 0
    store 3, r0
# initialise int val q3
    int_const r0, 0
    store 4, r0
# initialise int val q4
    int_const r0, 0
    store 5, r0
# initialise int val q5
    int_const r0, 0
    store 6, r0
# write "Quiz: Would you survive Thanos? [SPOILER ALERT] \n\n";
    string_const r0, "Quiz: Would you survive Thanos? [SPOILER ALERT] \n\n"
    call_builtin print_string
    branch_uncond label_1
label_0:
    int_const r0, 1
    store 0, r0
    branch_uncond label_2
label_1:
    int_const r0, 0
    store 0, r0
label_2:
# while !flag
label_3:
    load r0, 0
    branch_on_true r0, label_5
    branch_uncond label_4
label_4:
# do
# write "Q1: Who is your favorite avenger? (Type the number)\n";
    string_const r0, "Q1: Who is your favorite avenger? (Type the number)\n"
    call_builtin print_string
# write "1-Iron Man, 2-Captain America, 3-Thor, 4-Black Widow, 5-Hulk, 6-Hawk Eye\n";
    string_const r0, "1-Iron Man, 2-Captain America, 3-Thor, 4-Black Widow, 5-Hulk, 6-Hawk Eye\n"
    call_builtin print_string
# read q1;
    call_builtin read_int
    store 2, r0
# call check(q1, 1, 6, flag);
    load r0, 2
    int_const r1, 1
    int_const r2, 6
    load_address r3, 0
    call proc_check
# if !flag
    load r0, 0
    branch_on_true r0, label_7
    branch_uncond label_6
label_6:
# then
# write "Invalid input! Please type one number.\n";
    string_const r0, "Invalid input! Please type one number.\n"
    call_builtin print_string
# fi
label_7:
    branch_uncond label_3
# od
label_5:
    branch_uncond label_9
label_8:
    int_const r0, 1
    store 0, r0
    branch_uncond label_10
label_9:
    int_const r0, 0
    store 0, r0
label_10:
# while !flag
label_11:
    load r0, 0
    branch_on_true r0, label_13
    branch_uncond label_12
label_12:
# do
# write "Q2: If you can wield the power of one Infinity Stone, which one do you prefer? \n";
    string_const r0, "Q2: If you can wield the power of one Infinity Stone, which one do you prefer? \n"
    call_builtin print_string
# write "1-Space, 2-Soul, 3-Time, 4-Reality, 5-Power, 6-Mind\n";
    string_const r0, "1-Space, 2-Soul, 3-Time, 4-Reality, 5-Power, 6-Mind\n"
    call_builtin print_string
# read q2;
    call_builtin read_int
    store 3, r0
# call check(q2, 1, 6, flag);
    load r0, 3
    int_const r1, 1
    int_const r2, 6
    load_address r3, 0
    call proc_check
# if !flag
    load r0, 0
    branch_on_true r0, label_15
    branch_uncond label_14
label_14:
# then
# write "Invalid input! Please type one number.\n";
    string_const r0, "Invalid input! Please type one number.\n"
    call_builtin print_string
# fi
label_15:
    branch_uncond label_11
# od
label_13:
    branch_uncond label_17
label_16:
    int_const r0, 1
    store 0, r0
    branch_uncond label_18
label_17:
    int_const r0, 0
    store 0, r0
label_18:
# while !flag
label_19:
    load r0, 0
    branch_on_true r0, label_21
    branch_uncond label_20
label_20:
# do
# write "Q3: Team Cap or Team Iron Man?\n";
    string_const r0, "Q3: Team Cap or Team Iron Man?\n"
    call_builtin print_string
# write "1-I'm with Cap, 2-I'm with Tony\n";
    string_const r0, "1-I'm with Cap, 2-I'm with Tony\n"
    call_builtin print_string
# read q3;
    call_builtin read_int
    store 4, r0
# call check(q3, 1, 2, flag);
    load r0, 4
    int_const r1, 1
    int_const r2, 2
    load_address r3, 0
    call proc_check
# if !flag
    load r0, 0
    branch_on_true r0, label_23
    branch_uncond label_22
label_22:
# then
# write "Invalid input! Please type one number.\n";
    string_const r0, "Invalid input! Please type one number.\n"
    call_builtin print_string
# fi
label_23:
    branch_uncond label_19
# od
label_21:
    branch_uncond label_25
label_24:
    int_const r0, 1
    store 0, r0
    branch_uncond label_26
label_25:
    int_const r0, 0
    store 0, r0
label_26:
# while !flag
label_27:
    load r0, 0
    branch_on_true r0, label_29
    branch_uncond label_28
label_28:
# do
# write "Q4: Who should lead the Avengers after Endgame?\n";
    string_const r0, "Q4: Who should lead the Avengers after Endgame?\n"
    call_builtin print_string
# write "1-Captain Marvel, 2-Falcon, 3-Scarlet Witch, 4-Nick Fury, 5-Doctor Strange, 6-Black Panther\n";
    string_const r0, "1-Captain Marvel, 2-Falcon, 3-Scarlet Witch, 4-Nick Fury, 5-Doctor Strange, 6-Black Panther\n"
    call_builtin print_string
# read q4;
    call_builtin read_int
    store 5, r0
# call check(q4, 1, 6, flag);
    load r0, 5
    int_const r1, 1
    int_const r2, 6
    load_address r3, 0
    call proc_check
# if !flag
    load r0, 0
    branch_on_true r0, label_31
    branch_uncond label_30
label_30:
# then
# write "Invalid input! Please type one number.\n";
    string_const r0, "Invalid input! Please type one number.\n"
    call_builtin print_string
# fi
label_31:
    branch_uncond label_27
# od
label_29:
    branch_uncond label_33
label_32:
    int_const r0, 1
    store 0, r0
    branch_uncond label_34
label_33:
    int_const r0, 0
    store 0, r0
label_34:
# while !flag
label_35:
    load r0, 0
    branch_on_true r0, label_37
    branch_uncond label_36
label_36:
# do
# write "Q5: Who leads the guardians of the galaxy after Endgame?\n";
    string_const r0, "Q5: Who leads the guardians of the galaxy after Endgame?\n"
    call_builtin print_string
# write "1-Quill, 2-Thor, 3-Rocket, 4-Drax, 5-Gamora, 6-I am Groot, 7-Nebula";
    string_const r0, "1-Quill, 2-Thor, 3-Rocket, 4-Drax, 5-Gamora, 6-I am Groot, 7-Nebula"
    call_builtin print_string
# read q5;
    call_builtin read_int
    store 6, r0
# call check(q5, 1, 7, flag);
    load r0, 6
    int_const r1, 1
    int_const r2, 7
    load_address r3, 0
    call proc_check
# if !flag
    load r0, 0
    branch_on_true r0, label_39
    branch_uncond label_38
label_38:
# then
# write "Invalid input! Please type one number.\n";
    string_const r0, "Invalid input! Please type one number.\n"
    call_builtin print_string
# fi
label_39:
    branch_uncond label_35
# od
label_37:
# i := 0;
    int_const r0, 0
    store 1, r0
# call mod2((((q1 + q2) + q3) + q4) + q5, i);
    load r0, 2
    load r1, 3
    add_int r0, r0, r1
    load r1, 4
    add_int r0, r0, r1
    load r1, 5
    add_int r0, r0, r1
    load r1, 6
    add_int r0, r0, r1
    load_address r1, 1
    call proc_mod2
# if i = 0
    load r0, 1
    int_const r1, 0
    cmp_eq_int r0, r0, r1
    branch_on_true r0, label_40
    branch_uncond label_41
label_40:
# then
# write "Congrats! You survived (but were left in sad and sorrow)\n";
    string_const r0, "Congrats! You survived (but were left in sad and sorrow)\n"
    call_builtin print_string
    branch_uncond label_42
label_41:
# else
# write "Oh No! You are dust now...\n";
    string_const r0, "Oh No! You are dust now...\n"
    call_builtin print_string
# fi
label_42:
# epilogue
    pop_stack_frame 7
    return
proc_check:
# prologue
    push_stack_frame 4
# parameter passing
    store 3, r0
    store 2, r1
    store 1, r2
    store 0, r3
# if (q >= min) && (q <= max)
    load r0, 3
    load r1, 2
    cmp_ge_int r0, r0, r1
    branch_on_true r0, label_46
    branch_uncond label_44
label_46:
    load r0, 3
    load r1, 1
    cmp_le_int r0, r0, r1
    branch_on_true r0, label_43
    branch_uncond label_44
label_43:
# then
    branch_uncond label_47
label_47:
    int_const r0, 1
    load r1, 0
    store_indirect r1, r0
    branch_uncond label_49
label_48:
    int_const r0, 0
    load r1, 0
    store_indirect r1, r0
label_49:
    branch_uncond label_45
label_44:
# else
    branch_uncond label_51
label_50:
    int_const r0, 1
    load r1, 0
    store_indirect r1, r0
    branch_uncond label_52
label_51:
    int_const r0, 0
    load r1, 0
    store_indirect r1, r0
label_52:
# fi
label_45:
# epilogue
    pop_stack_frame 4
    return
proc_mod2:
# prologue
    push_stack_frame 2
# parameter passing
    store 1, r0
    store 0, r1
# if q <= 1
    load r0, 1
    int_const r1, 1
    cmp_le_int r0, r0, r1
    branch_on_true r0, label_53
    branch_uncond label_54
label_53:
# then
# i := q;
    load r0, 1
    load r1, 0
    store_indirect r1, r0
    branch_uncond label_55
label_54:
# else
# call mod2(q - 2, i);
    load r0, 1
    int_const r1, 2
    sub_int r0, r0, r1
    load r1, 0
    call proc_mod2
# fi
label_55:
# epilogue
    pop_stack_frame 2
    return
