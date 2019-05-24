    call proc_main
    halt
proc_checkIsWin:
# prologue
    push_stack_frame 3
# parameter passing
    store 1, r0
    store 2, r1
    store 0, r2
# if (opleft = 0) && (opright = 0)
    load r0, 1
    int_const r1, 0
    cmp_eq_int r0, r0, r1
    branch_on_true r0, label_3
    branch_uncond label_1
label_3:
    load r0, 2
    int_const r1, 0
    cmp_eq_int r0, r0, r1
    branch_on_true r0, label_0
    branch_uncond label_1
label_0:
# then
    branch_uncond label_4
label_4:
    int_const r0, 1
    load r1, 0
    store_indirect r1, r0
    branch_uncond label_6
label_5:
    int_const r0, 0
    load r1, 0
    store_indirect r1, r0
label_6:
    branch_uncond label_2
label_1:
# else
    branch_uncond label_8
label_7:
    int_const r0, 1
    load r1, 0
    store_indirect r1, r0
    branch_uncond label_9
label_8:
    int_const r0, 0
    load r1, 0
    store_indirect r1, r0
label_9:
# fi
label_2:
# epilogue
    pop_stack_frame 3
    return
proc_printHand:
# prologue
    push_stack_frame 3
# parameter passing
    store 0, r0
    store 2, r1
    store 1, r2
# write "Player ";
    string_const r0, "Player "
    call_builtin print_string
# write player;
    load r0, 1
    call_builtin print_int
# write " hands: ";
    string_const r0, " hands: "
    call_builtin print_string
# write left;
    load r0, 0
    call_builtin print_int
# write " ";
    string_const r0, " "
    call_builtin print_string
# write right;
    load r0, 2
    call_builtin print_int
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# epilogue
    pop_stack_frame 3
    return
proc_tap:
# prologue
    push_stack_frame 7
# parameter passing
    store 1, r0
    store 3, r1
    store 2, r2
    store 4, r3
    store 5, r4
# initialise bool val fromLeft
    int_const r0, 0
    store 0, r0
# initialise bool val toLeft
    int_const r0, 0
    store 6, r0
# read fromLeft;
    call_builtin read_bool
    store 0, r0
# read toLeft;
    call_builtin read_bool
    store 6, r0
# if player = 1
    load r0, 5
    int_const r1, 1
    cmp_eq_int r0, r0, r1
    branch_on_true r0, label_10
    branch_uncond label_11
label_10:
# then
# if fromLeft && toLeft
    load r0, 0
    branch_on_true r0, label_15
    branch_uncond label_14
label_15:
    load r0, 6
    branch_on_true r0, label_13
    branch_uncond label_14
label_13:
# then
# p2Left := p2Left + p1Left;
    load r0, 3
    load_indirect r0, r0
    load r1, 1
    load_indirect r1, r1
    add_int r0, r0, r1
    load r1, 3
    store_indirect r1, r0
# fi
label_14:
# if fromLeft && !toLeft
    load r0, 0
    branch_on_true r0, label_18
    branch_uncond label_17
label_18:
    load r0, 6
    branch_on_true r0, label_17
    branch_uncond label_16
label_16:
# then
# p2Right := p2Right + p1Left;
    load r0, 4
    load_indirect r0, r0
    load r1, 1
    load_indirect r1, r1
    add_int r0, r0, r1
    load r1, 4
    store_indirect r1, r0
# fi
label_17:
# if !fromLeft && toLeft
    load r0, 0
    branch_on_true r0, label_20
    branch_uncond label_21
label_21:
    load r0, 6
    branch_on_true r0, label_19
    branch_uncond label_20
label_19:
# then
# p2Left := p2Left + p1Right;
    load r0, 3
    load_indirect r0, r0
    load r1, 2
    load_indirect r1, r1
    add_int r0, r0, r1
    load r1, 3
    store_indirect r1, r0
# fi
label_20:
# if !fromLeft && !toLeft
    load r0, 0
    branch_on_true r0, label_23
    branch_uncond label_24
label_24:
    load r0, 6
    branch_on_true r0, label_23
    branch_uncond label_22
label_22:
# then
# p2Right := p2Right + p1Right;
    load r0, 4
    load_indirect r0, r0
    load r1, 2
    load_indirect r1, r1
    add_int r0, r0, r1
    load r1, 4
    store_indirect r1, r0
# fi
label_23:
    branch_uncond label_12
label_11:
# else
# if fromLeft && toLeft
    load r0, 0
    branch_on_true r0, label_27
    branch_uncond label_26
label_27:
    load r0, 6
    branch_on_true r0, label_25
    branch_uncond label_26
label_25:
# then
# p1Left := p1Left + p2Left;
    load r0, 1
    load_indirect r0, r0
    load r1, 3
    load_indirect r1, r1
    add_int r0, r0, r1
    load r1, 1
    store_indirect r1, r0
# fi
label_26:
# if fromLeft && !toLeft
    load r0, 0
    branch_on_true r0, label_30
    branch_uncond label_29
label_30:
    load r0, 6
    branch_on_true r0, label_29
    branch_uncond label_28
label_28:
# then
# p1Right := p1Right + p2Left;
    load r0, 2
    load_indirect r0, r0
    load r1, 3
    load_indirect r1, r1
    add_int r0, r0, r1
    load r1, 2
    store_indirect r1, r0
# fi
label_29:
# if !fromLeft && toLeft
    load r0, 0
    branch_on_true r0, label_32
    branch_uncond label_33
label_33:
    load r0, 6
    branch_on_true r0, label_31
    branch_uncond label_32
label_31:
# then
# p1Left := p1Left + p2Right;
    load r0, 1
    load_indirect r0, r0
    load r1, 4
    load_indirect r1, r1
    add_int r0, r0, r1
    load r1, 1
    store_indirect r1, r0
# fi
label_32:
# if !fromLeft && !toLeft
    load r0, 0
    branch_on_true r0, label_35
    branch_uncond label_36
label_36:
    load r0, 6
    branch_on_true r0, label_35
    branch_uncond label_34
label_34:
# then
# p1Right := p1Right + p2Right;
    load r0, 2
    load_indirect r0, r0
    load r1, 4
    load_indirect r1, r1
    add_int r0, r0, r1
    load r1, 2
    store_indirect r1, r0
# fi
label_35:
# fi
label_12:
# if p1Left >= 5
    load r0, 1
    load_indirect r0, r0
    int_const r1, 5
    cmp_ge_int r0, r0, r1
    branch_on_true r0, label_37
    branch_uncond label_38
label_37:
# then
# p1Left := p1Left - 5;
    load r0, 1
    load_indirect r0, r0
    int_const r1, 5
    sub_int r0, r0, r1
    load r1, 1
    store_indirect r1, r0
# fi
label_38:
# if p2Left >= 5
    load r0, 3
    load_indirect r0, r0
    int_const r1, 5
    cmp_ge_int r0, r0, r1
    branch_on_true r0, label_39
    branch_uncond label_40
label_39:
# then
# p2Left := p2Left - 5;
    load r0, 3
    load_indirect r0, r0
    int_const r1, 5
    sub_int r0, r0, r1
    load r1, 3
    store_indirect r1, r0
# fi
label_40:
# if p1Right >= 5
    load r0, 2
    load_indirect r0, r0
    int_const r1, 5
    cmp_ge_int r0, r0, r1
    branch_on_true r0, label_41
    branch_uncond label_42
label_41:
# then
# p1Right := p1Right - 5;
    load r0, 2
    load_indirect r0, r0
    int_const r1, 5
    sub_int r0, r0, r1
    load r1, 2
    store_indirect r1, r0
# fi
label_42:
# if p2Right >= 5
    load r0, 4
    load_indirect r0, r0
    int_const r1, 5
    cmp_ge_int r0, r0, r1
    branch_on_true r0, label_43
    branch_uncond label_44
label_43:
# then
# p2Right := p2Right - 5;
    load r0, 4
    load_indirect r0, r0
    int_const r1, 5
    sub_int r0, r0, r1
    load r1, 4
    store_indirect r1, r0
# fi
label_44:
# epilogue
    pop_stack_frame 7
    return
proc_swap:
# prologue
    push_stack_frame 4
# parameter passing
    store 0, r0
    store 2, r1
# initialise int val nPoints
    int_const r0, 0
    store 1, r0
# initialise bool val toLeft
    int_const r0, 0
    store 3, r0
# read toLeft;
    call_builtin read_bool
    store 3, r0
# read nPoints;
    call_builtin read_int
    store 1, r0
# if toLeft
    load r0, 3
    branch_on_true r0, label_45
    branch_uncond label_46
label_45:
# then
# left := left + nPoints;
    load r0, 0
    load_indirect r0, r0
    load r1, 1
    add_int r0, r0, r1
    load r1, 0
    store_indirect r1, r0
# right := right - nPoints;
    load r0, 2
    load_indirect r0, r0
    load r1, 1
    sub_int r0, r0, r1
    load r1, 2
    store_indirect r1, r0
    branch_uncond label_47
label_46:
# else
# left := left - nPoints;
    load r0, 0
    load_indirect r0, r0
    load r1, 1
    sub_int r0, r0, r1
    load r1, 0
    store_indirect r1, r0
# right := right + nPoints;
    load r0, 2
    load_indirect r0, r0
    load r1, 1
    add_int r0, r0, r1
    load r1, 2
    store_indirect r1, r0
# fi
label_47:
# epilogue
    pop_stack_frame 4
    return
proc_main:
# prologue
    push_stack_frame 8
# initialise bool val isWin
    int_const r0, 0
    store 0, r0
# initialise int val move
    int_const r0, 0
    store 1, r0
# initialise int val p1Left
    int_const r0, 0
    store 2, r0
# initialise int val p1Right
    int_const r0, 0
    store 3, r0
# initialise int val p2Left
    int_const r0, 0
    store 4, r0
# initialise int val p2Right
    int_const r0, 0
    store 5, r0
# initialise int val player
    int_const r0, 0
    store 6, r0
# initialise int val winner
    int_const r0, 0
    store 7, r0
# player := 1;
    int_const r0, 1
    store 6, r0
    branch_uncond label_49
label_48:
    int_const r0, 1
    store 0, r0
    branch_uncond label_50
label_49:
    int_const r0, 0
    store 0, r0
label_50:
# p1Left := 1;
    int_const r0, 1
    store 2, r0
# p2Left := 1;
    int_const r0, 1
    store 4, r0
# p1Right := 1;
    int_const r0, 1
    store 3, r0
# p2Right := 1;
    int_const r0, 1
    store 5, r0
# while !isWin
label_51:
    load r0, 0
    branch_on_true r0, label_53
    branch_uncond label_52
label_52:
# do
# write "Enter move: ";
    string_const r0, "Enter move: "
    call_builtin print_string
# read move;
    call_builtin read_int
    store 1, r0
# if move = 0
    load r0, 1
    int_const r1, 0
    cmp_eq_int r0, r0, r1
    branch_on_true r0, label_54
    branch_uncond label_55
label_54:
# then
# call tap(p1Left, p2Left, p1Right, p2Right, player);
    load_address r0, 2
    load_address r1, 4
    load_address r2, 3
    load_address r3, 5
    load r4, 6
    call proc_tap
    branch_uncond label_56
label_55:
# else
# if player = 1
    load r0, 6
    int_const r1, 1
    cmp_eq_int r0, r0, r1
    branch_on_true r0, label_57
    branch_uncond label_58
label_57:
# then
# call swap(p1Left, p1Right);
    load_address r0, 2
    load_address r1, 3
    call proc_swap
    branch_uncond label_59
label_58:
# else
# call swap(p2Left, p2Right);
    load_address r0, 4
    load_address r1, 5
    call proc_swap
# fi
label_59:
# fi
label_56:
# call printHand(p1Left, p1Right, player);
    load r0, 2
    load r1, 3
    load r2, 6
    call proc_printHand
# call printHand(p2Left, p2Right, player);
    load r0, 4
    load r1, 5
    load r2, 6
    call proc_printHand
# if player = 1
    load r0, 6
    int_const r1, 1
    cmp_eq_int r0, r0, r1
    branch_on_true r0, label_60
    branch_uncond label_61
label_60:
# then
# call checkIsWin(p2Left, p2Right, isWin);
    load r0, 4
    load r1, 5
    load_address r2, 0
    call proc_checkIsWin
# if isWin
    load r0, 0
    branch_on_true r0, label_63
    branch_uncond label_64
label_63:
# then
# winner := player;
    load r0, 6
    store 7, r0
# fi
label_64:
    branch_uncond label_62
label_61:
# else
# call checkIsWin(p1Left, p1Right, isWin);
    load r0, 2
    load r1, 3
    load_address r2, 0
    call proc_checkIsWin
# if isWin
    load r0, 0
    branch_on_true r0, label_65
    branch_uncond label_66
label_65:
# then
# winner := player;
    load r0, 6
    store 7, r0
# fi
label_66:
# fi
label_62:
# if player = 1
    load r0, 6
    int_const r1, 1
    cmp_eq_int r0, r0, r1
    branch_on_true r0, label_67
    branch_uncond label_68
label_67:
# then
# player := 2;
    int_const r0, 2
    store 6, r0
    branch_uncond label_69
label_68:
# else
# player := 1;
    int_const r0, 1
    store 6, r0
# fi
label_69:
    branch_uncond label_51
# od
label_53:
# write "Winner: ";
    string_const r0, "Winner: "
    call_builtin print_string
# write winner;
    load r0, 7
    call_builtin print_int
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# epilogue
    pop_stack_frame 8
    return
