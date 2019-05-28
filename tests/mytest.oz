    call proc_main
    halt
proc_main:
  # procedure main
    push_stack_frame 30
  # formal parameter section
  # initialize int val i1
    int_const r0, 0
    store 0, r0
  # initialize int val i2
    int_const r0, 0
    store 1, r0
  # initialize float val f1
    real_const r0, 0.0
    store 2, r0
  # initialize float val f2
    real_const r0, 0.0
    store 3, r0
  # initialize bool val b1
    int_const r0, 0
    store 4, r0
  # initialize bool val b2
    int_const r0, 0
    store 5, r0
  # initialize int val ia[2]
    int_const r0, 0
    store 6, r0
    int_const r0, 0
    store 7, r0
  # initialize int val im[2,3]
    int_const r0, 0
    store 8, r0
    int_const r0, 0
    store 9, r0
    int_const r0, 0
    store 10, r0
    int_const r0, 0
    store 11, r0
    int_const r0, 0
    store 12, r0
    int_const r0, 0
    store 13, r0
  # initialize float val fa[2]
    real_const r0, 0.0
    store 14, r0
    real_const r0, 0.0
    store 15, r0
  # initialize float val fm[2,3]
    real_const r0, 0.0
    store 16, r0
    real_const r0, 0.0
    store 17, r0
    real_const r0, 0.0
    store 18, r0
    real_const r0, 0.0
    store 19, r0
    real_const r0, 0.0
    store 20, r0
    real_const r0, 0.0
    store 21, r0
  # initialize bool val ba[2]
    int_const r0, 0
    store 22, r0
    int_const r0, 0
    store 23, r0
  # initialize bool val bm[2,3]
    int_const r0, 0
    store 24, r0
    int_const r0, 0
    store 25, r0
    int_const r0, 0
    store 26, r0
    int_const r0, 0
    store 27, r0
    int_const r0, 0
    store 28, r0
    int_const r0, 0
    store 29, r0
  # write i1;
    load r0, 0
    call_builtin print_int
  # write f1;
    load r0, 2
    call_builtin print_real
  # write b1;
    load r0, 4
    call_builtin print_bool
  # write ia[0];
    int_const r0, 0
    load_address r1, 6
    sub_offset r1, r1, r0
    load_indirect r1, r1
    move r0, r1
    call_builtin print_int
  # write im[0, 0];
    int_const r0, 0
    int_const r1, 0
    load_address r2, 8
    int_const r3, 3
    mul_int r0, r0, r3
    add_int r0, r0, r1
    sub_offset r2, r2, r0
    load_indirect r2, r2
    move r0, r2
    call_builtin print_int
  # write fa[1];
    int_const r0, 1
    load_address r1, 14
    sub_offset r1, r1, r0
    load_indirect r1, r1
    move r0, r1
    call_builtin print_real
  # write fm[0, 2];
    int_const r0, 0
    int_const r1, 2
    load_address r2, 16
    int_const r3, 3
    mul_int r0, r0, r3
    add_int r0, r0, r1
    sub_offset r2, r2, r0
    load_indirect r2, r2
    move r0, r2
    call_builtin print_real
  # write ba[1];
    int_const r0, 1
    load_address r1, 22
    sub_offset r1, r1, r0
    load_indirect r1, r1
    move r0, r1
    call_builtin print_bool
  # write bm[1, 2];
    int_const r0, 1
    int_const r1, 2
    load_address r2, 24
    int_const r3, 3
    mul_int r0, r0, r3
    add_int r0, r0, r1
    sub_offset r2, r2, r0
    load_indirect r2, r2
    move r0, r2
    call_builtin print_bool
  # write i1 = i2;
    load r0, 0
    load r1, 1
    cmp_eq_int r0, r0, r1
    call_builtin print_bool
  # write f1 = f2;
    load r0, 2
    load r1, 3
    cmp_eq_real r0, r0, r1
    call_builtin print_bool
  # write b1 = b2;
    load r0, 4
    load r1, 5
    cmp_eq_int r0, r0, r1
    call_builtin print_bool
  # write i1 = f1;
    load r0, 0
    load r1, 2
    int_to_real r0, r0
    cmp_eq_real r0, r0, r1
    call_builtin print_bool
  # write f1 = i1;
    load r0, 2
    load r1, 0
    int_to_real r1, r1
    cmp_eq_real r0, r0, r1
    call_builtin print_bool
  # write i1 != i2;
    load r0, 0
    load r1, 1
    cmp_ne_int r0, r0, r1
    call_builtin print_bool
  # write f1 != f2;
    load r0, 2
    load r1, 3
    cmp_ne_real r0, r0, r1
    call_builtin print_bool
  # write b1 != b2;
    load r0, 4
    load r1, 5
    cmp_ne_int r0, r0, r1
    call_builtin print_bool
  # write i1 != f1;
    load r0, 0
    load r1, 2
    int_to_real r0, r0
    cmp_ne_real r0, r0, r1
    call_builtin print_bool
  # write f1 != i1;
    load r0, 2
    load r1, 0
    int_to_real r1, r1
    cmp_ne_real r0, r0, r1
    call_builtin print_bool
  # write i1 < i2;
    load r0, 0
    load r1, 1
    cmp_lt_int r0, r0, r1
    call_builtin print_bool
  # write i1 <= i2;
    load r0, 0
    load r1, 1
    cmp_le_int r0, r0, r1
    call_builtin print_bool
  # write i1 > i2;
    load r0, 0
    load r1, 1
    cmp_gt_int r0, r0, r1
    call_builtin print_bool
  # write i1 >= i2;
    load r0, 0
    load r1, 1
    cmp_ge_int r0, r0, r1
    call_builtin print_bool
  # write f1 < f2;
    load r0, 2
    load r1, 3
    cmp_lt_real r0, r0, r1
    call_builtin print_bool
  # write f1 <= f2;
    load r0, 2
    load r1, 3
    cmp_le_real r0, r0, r1
    call_builtin print_bool
  # write f1 > f2;
    load r0, 2
    load r1, 3
    cmp_gt_real r0, r0, r1
    call_builtin print_bool
  # write f1 >= f2;
    load r0, 2
    load r1, 3
    cmp_ge_real r0, r0, r1
    call_builtin print_bool
  # write b1 < b2;
    load r0, 4
    load r1, 5
    cmp_lt_int r0, r0, r1
    call_builtin print_bool
  # write b1 <= b2;
    load r0, 4
    load r1, 5
    cmp_le_int r0, r0, r1
    call_builtin print_bool
  # write b1 > b2;
    load r0, 4
    load r1, 5
    cmp_gt_int r0, r0, r1
    call_builtin print_bool
  # write b1 >= b2;
    load r0, 4
    load r1, 5
    cmp_ge_int r0, r0, r1
    call_builtin print_bool
  # write i1 < f1;
    load r0, 0
    load r1, 2
    int_to_real r0, r0
    cmp_lt_real r0, r0, r1
    call_builtin print_bool
  # write i1 <= f1;
    load r0, 0
    load r1, 2
    int_to_real r0, r0
    cmp_le_real r0, r0, r1
    call_builtin print_bool
  # write i1 > f1;
    load r0, 0
    load r1, 2
    int_to_real r0, r0
    cmp_gt_real r0, r0, r1
    call_builtin print_bool
  # write i1 >= f1;
    load r0, 0
    load r1, 2
    int_to_real r0, r0
    cmp_ge_real r0, r0, r1
    call_builtin print_bool
  # i2 := 1;
    int_const r0, 1
    store 1, r0
  # f2 := 1.5;
    real_const r0, 1.5
    store 3, r0
  # write i1 + i2;
    load r0, 0
    load r1, 1
    add_int r0, r0, r1
    call_builtin print_int
  # write i1 - i2;
    load r0, 0
    load r1, 1
    sub_int r0, r0, r1
    call_builtin print_int
  # write i1 * i2;
    load r0, 0
    load r1, 1
    mul_int r0, r0, r1
    call_builtin print_int
  # write i1 / i2;
    load r0, 0
    load r1, 1
    div_int r0, r0, r1
    call_builtin print_int
  # write f1 + f2;
    load r0, 2
    load r1, 3
    add_real r0, r0, r1
    call_builtin print_real
  # write f1 - f2;
    load r0, 2
    load r1, 3
    sub_real r0, r0, r1
    call_builtin print_real
  # write f1 * f2;
    load r0, 2
    load r1, 3
    mul_real r0, r0, r1
    call_builtin print_real
  # write f1 / f2;
    load r0, 2
    load r1, 3
    div_real r0, r0, r1
    call_builtin print_real
  # write i1 + f2;
    load r0, 0
    load r1, 3
    int_to_real r0, r0
    add_real r0, r0, r1
    call_builtin print_real
  # write i1 - f2;
    load r0, 0
    load r1, 3
    int_to_real r0, r0
    sub_real r0, r0, r1
    call_builtin print_real
  # write i1 * f2;
    load r0, 0
    load r1, 3
    int_to_real r0, r0
    mul_real r0, r0, r1
    call_builtin print_real
  # write i1 / f2;
    load r0, 0
    load r1, 3
    int_to_real r0, r0
    div_real r0, r0, r1
    call_builtin print_real
  # write f1 + i2;
    load r0, 2
    load r1, 1
    int_to_real r1, r1
    add_real r0, r0, r1
    call_builtin print_real
  # write f1 - i2;
    load r0, 2
    load r1, 1
    int_to_real r1, r1
    sub_real r0, r0, r1
    call_builtin print_real
  # write f1 * i2;
    load r0, 2
    load r1, 1
    int_to_real r1, r1
    mul_real r0, r0, r1
    call_builtin print_real
  # write f1 / i2;
    load r0, 2
    load r1, 1
    int_to_real r1, r1
    div_real r0, r0, r1
    call_builtin print_real
  # write b1 && b2;
    load r0, 4
    branch_on_true r0, label_0
    branch_uncond label_1
label_0:
    load r1, 5
    move r0, r1
    branch_uncond label_1
label_1:
    call_builtin print_bool
  # write b1 || b2;
    load r0, 4
    branch_on_true r0, label_2
    load r1, 5
    move r0, r1
    branch_on_true r0, label_2
    int_const r0, 0
label_2:
    call_builtin print_bool
  # write !b1;
    load r0, 4
    not r0, r0
    call_builtin print_bool
  # write 1 > 0;
    int_const r0, 1
    int_const r1, 0
    cmp_gt_int r0, r0, r1
    call_builtin print_bool
  # write (1 = 1) && !(2 = 3);
    int_const r0, 1
    int_const r1, 1
    cmp_eq_int r0, r0, r1
    branch_on_true r0, label_4
    branch_uncond label_5
label_4:
    int_const r2, 2
    int_const r3, 3
    cmp_eq_int r2, r2, r3
    not r2, r2
    move r0, r2
    branch_uncond label_5
label_5:
    call_builtin print_bool
  # write "\n";
    string_const r0, "\n"
    call_builtin print_string
  # write (0 > 8) && ((3 / 0) = 5);
    int_const r0, 0
    int_const r1, 8
    cmp_gt_int r0, r0, r1
    branch_on_true r0, label_6
    branch_uncond label_7
label_6:
    int_const r2, 3
    int_const r3, 0
    div_int r2, r2, r3
    int_const r4, 5
    cmp_eq_int r2, r2, r4
    move r0, r2
    branch_uncond label_7
label_7:
    call_builtin print_bool
  # write (4 = 4) || ((3 / 0) = 5);
    int_const r0, 4
    int_const r1, 4
    cmp_eq_int r0, r0, r1
    branch_on_true r0, label_8
    int_const r2, 3
    int_const r3, 0
    div_int r2, r2, r3
    int_const r4, 5
    cmp_eq_int r2, r2, r4
    move r0, r2
    branch_on_true r0, label_8
    int_const r0, 0
label_8:
    call_builtin print_bool
  # i1 := 1;
    int_const r0, 1
    store 0, r0
  # f1 := 2.45;
    real_const r0, 2.45
    store 2, r0
  # f1 := 5;
    int_const r0, 5
    int_to_real r0, r0
    store 2, r0
  # b1 := b2;
    load r0, 5
    store 4, r0
  # b1 := true;
    int_const r0, 1
    store 4, r0
  # b2 := b1;
    load r0, 4
    store 5, r0
  # if true then
    int_const r0, 1
    branch_on_true r0, label_10
    branch_uncond label_11
label_10:
  # write true;
    int_const r0, 1
    call_builtin print_bool
label_11:
  # fi
  # if true then
    int_const r0, 1
    branch_on_true r0, label_12
    branch_uncond label_13
label_12:
  # write true;
    int_const r0, 1
    call_builtin print_bool
    branch_uncond label_14
  # else
label_13:
  # write false;
    int_const r0, 0
    call_builtin print_bool
label_14:
  # fi
  # while i1 < 3 do
label_15:
    load r0, 0
    int_const r1, 3
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_16
    branch_uncond label_17
label_16:
  # i1 := i1 + 1;
    load r0, 0
    int_const r1, 1
    add_int r0, r0, r1
    store 0, r0
    branch_uncond label_15
label_17:
  # od
  # write i1;
    load r0, 0
    call_builtin print_int
  # call p(i1, i2);
    load r1, 0
    move r0, r1
    load_address r1, 1
    call proc_p
  # write "\n";
    string_const r0, "\n"
    call_builtin print_string
  # ia[0] := 69;
    int_const r0, 0
    load_address r1, 6
    sub_offset r1, r1, r0
    int_const r2, 69
    store_indirect r1, r2
  # ia[1] := 11;
    int_const r0, 1
    load_address r1, 6
    sub_offset r1, r1, r0
    int_const r2, 11
    store_indirect r1, r2
  # write "\nia values: ";
    string_const r0, "\nia values: "
    call_builtin print_string
  # write ia[0];
    int_const r0, 0
    load_address r1, 6
    sub_offset r1, r1, r0
    load_indirect r1, r1
    move r0, r1
    call_builtin print_int
  # write ia[1];
    int_const r0, 1
    load_address r1, 6
    sub_offset r1, r1, r0
    load_indirect r1, r1
    move r0, r1
    call_builtin print_int
  # call p(3, ia[1]);
    int_const r1, 3
    move r0, r1
    int_const r2, 1
    load_address r1, 6
    sub_offset r1, r1, r2
    call proc_p
  # i1 := 10;
    int_const r0, 10
    store 0, r0
  # write i1;
    load r0, 0
    call_builtin print_int
  # write "\n";
    string_const r0, "\n"
    call_builtin print_string
  # call refMod(i1);
    load_address r0, 0
    call proc_refMod
  # write i1;
    load r0, 0
    call_builtin print_int
  # write "\n";
    string_const r0, "\n"
    call_builtin print_string
  # ia[0] := 9;
    int_const r0, 0
    load_address r1, 6
    sub_offset r1, r1, r0
    int_const r2, 9
    store_indirect r1, r2
  # write ia[0];
    int_const r0, 0
    load_address r1, 6
    sub_offset r1, r1, r0
    load_indirect r1, r1
    move r0, r1
    call_builtin print_int
  # write "\n";
    string_const r0, "\n"
    call_builtin print_string
  # call refMod(ia[0]);
    int_const r1, 0
    load_address r0, 6
    sub_offset r0, r0, r1
    call proc_refMod
  # write ia[0];
    int_const r0, 0
    load_address r1, 6
    sub_offset r1, r1, r0
    load_indirect r1, r1
    move r0, r1
    call_builtin print_int
  # write "\n";
    string_const r0, "\n"
    call_builtin print_string
  # write "\n";
    string_const r0, "\n"
    call_builtin print_string
  # ia[0] := 10;
    int_const r0, 0
    load_address r1, 6
    sub_offset r1, r1, r0
    int_const r2, 10
    store_indirect r1, r2
  # ia[1] := 11;
    int_const r0, 1
    load_address r1, 6
    sub_offset r1, r1, r0
    int_const r2, 11
    store_indirect r1, r2
  # read i1;
    call_builtin read_int
    store 0, r0
  # write ia[i1];
    load r0, 0
    load_address r1, 6
    sub_offset r1, r1, r0
    load_indirect r1, r1
    move r0, r1
    call_builtin print_int
    pop_stack_frame 30
    return
proc_refMod:
  # procedure refMod
    push_stack_frame 1
  # formal parameter section
    store 0, r0
  # num := 0;
    load r0, 0
    int_const r1, 0
    store_indirect r0, r1
    pop_stack_frame 1
    return
proc_p:
  # procedure p
    push_stack_frame 2
  # formal parameter section
    store 0, r0
    store 1, r1
  # write "\n";
    string_const r0, "\n"
    call_builtin print_string
  # write "proc p";
    string_const r0, "proc p"
    call_builtin print_string
  # write "\n";
    string_const r0, "\n"
    call_builtin print_string
  # write "passed val arg: ";
    string_const r0, "passed val arg: "
    call_builtin print_string
  # write in;
    load r0, 0
    call_builtin print_int
  # write "\n";
    string_const r0, "\n"
    call_builtin print_string
  # write "passed reference arg: ";
    string_const r0, "passed reference arg: "
    call_builtin print_string
  # write out;
    load r0, 1
    load_indirect r0, r0
    call_builtin print_int
  # write "\n";
    string_const r0, "\n"
    call_builtin print_string
    pop_stack_frame 2
    return