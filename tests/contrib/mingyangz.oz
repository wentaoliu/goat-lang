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
# initialise int val f
    int_const r0, 0
    store 5, r0
# a := 1;
    int_const r0, 1
    store 0, r0
# b := 10;
    int_const r0, 10
    store 1, r0
# c := 100;
    int_const r0, 100
    store 2, r0
# d := 1000;
    int_const r0, 1000
    store 3, r0
# d := 1000;
    int_const r0, 1000
    store 3, r0
# e := 10000;
    int_const r0, 10000
    store 4, r0
# f := 20000;
    int_const r0, 20000
    store 5, r0
# while (((((a <= 100) && (b <= 110)) && (c <= 200)) && (d <= 1100)) && (e <= 10100)) && (f <= 20100)
label_0:
    load r0, 0
    int_const r1, 100
    cmp_le_int r0, r0, r1
    branch_on_true r0, label_7
    branch_uncond label_2
label_7:
    load r0, 1
    int_const r1, 110
    cmp_le_int r0, r0, r1
    branch_on_true r0, label_6
    branch_uncond label_2
label_6:
    load r0, 2
    int_const r1, 200
    cmp_le_int r0, r0, r1
    branch_on_true r0, label_5
    branch_uncond label_2
label_5:
    load r0, 3
    int_const r1, 1100
    cmp_le_int r0, r0, r1
    branch_on_true r0, label_4
    branch_uncond label_2
label_4:
    load r0, 4
    int_const r1, 10100
    cmp_le_int r0, r0, r1
    branch_on_true r0, label_3
    branch_uncond label_2
label_3:
    load r0, 5
    int_const r1, 20100
    cmp_le_int r0, r0, r1
    branch_on_true r0, label_1
    branch_uncond label_2
label_1:
# do
# a := a + 1;
    load r0, 0
    int_const r1, 1
    add_int r0, r0, r1
    store 0, r0
# b := b + 1;
    load r0, 1
    int_const r1, 1
    add_int r0, r0, r1
    store 1, r0
# c := c + 1;
    load r0, 2
    int_const r1, 1
    add_int r0, r0, r1
    store 2, r0
# d := d + 1;
    load r0, 3
    int_const r1, 1
    add_int r0, r0, r1
    store 3, r0
# e := e + 1;
    load r0, 4
    int_const r1, 1
    add_int r0, r0, r1
    store 4, r0
# f := f + 1;
    load r0, 5
    int_const r1, 1
    add_int r0, r0, r1
    store 5, r0
    branch_uncond label_0
# od
label_2:
# while ((a <= 100) || ((b <= 110) && (c <= 200))) || (((d <= 1100) && (e <= 10100)) && (f <= 20100))
label_8:
    load r0, 0
    int_const r1, 100
    cmp_le_int r0, r0, r1
    branch_on_true r0, label_9
    branch_uncond label_12
label_12:
    load r0, 1
    int_const r1, 110
    cmp_le_int r0, r0, r1
    branch_on_true r0, label_13
    branch_uncond label_11
label_13:
    load r0, 2
    int_const r1, 200
    cmp_le_int r0, r0, r1
    branch_on_true r0, label_9
    branch_uncond label_11
label_11:
    load r0, 3
    int_const r1, 1100
    cmp_le_int r0, r0, r1
    branch_on_true r0, label_15
    branch_uncond label_10
label_15:
    load r0, 4
    int_const r1, 10100
    cmp_le_int r0, r0, r1
    branch_on_true r0, label_14
    branch_uncond label_10
label_14:
    load r0, 5
    int_const r1, 20100
    cmp_le_int r0, r0, r1
    branch_on_true r0, label_9
    branch_uncond label_10
label_9:
# do
# a := a + 1;
    load r0, 0
    int_const r1, 1
    add_int r0, r0, r1
    store 0, r0
# b := b + 1;
    load r0, 1
    int_const r1, 1
    add_int r0, r0, r1
    store 1, r0
# c := c + 1;
    load r0, 2
    int_const r1, 1
    add_int r0, r0, r1
    store 2, r0
# d := d + 1;
    load r0, 3
    int_const r1, 1
    add_int r0, r0, r1
    store 3, r0
# e := e + 1;
    load r0, 4
    int_const r1, 1
    add_int r0, r0, r1
    store 4, r0
# f := f + 1;
    load r0, 5
    int_const r1, 1
    add_int r0, r0, r1
    store 5, r0
    branch_uncond label_8
# od
label_10:
# if (((((a <= 100) && (b <= 110)) && (c <= 200)) && (d <= 1100)) && (e <= 10100)) && (f <= 20100)
    load r0, 0
    int_const r1, 100
    cmp_le_int r0, r0, r1
    branch_on_true r0, label_22
    branch_uncond label_17
label_22:
    load r0, 1
    int_const r1, 110
    cmp_le_int r0, r0, r1
    branch_on_true r0, label_21
    branch_uncond label_17
label_21:
    load r0, 2
    int_const r1, 200
    cmp_le_int r0, r0, r1
    branch_on_true r0, label_20
    branch_uncond label_17
label_20:
    load r0, 3
    int_const r1, 1100
    cmp_le_int r0, r0, r1
    branch_on_true r0, label_19
    branch_uncond label_17
label_19:
    load r0, 4
    int_const r1, 10100
    cmp_le_int r0, r0, r1
    branch_on_true r0, label_18
    branch_uncond label_17
label_18:
    load r0, 5
    int_const r1, 20100
    cmp_le_int r0, r0, r1
    branch_on_true r0, label_16
    branch_uncond label_17
label_16:
# then
# a := a + 1;
    load r0, 0
    int_const r1, 1
    add_int r0, r0, r1
    store 0, r0
# b := b + 1;
    load r0, 1
    int_const r1, 1
    add_int r0, r0, r1
    store 1, r0
# c := c + 1;
    load r0, 2
    int_const r1, 1
    add_int r0, r0, r1
    store 2, r0
# d := d + 1;
    load r0, 3
    int_const r1, 1
    add_int r0, r0, r1
    store 3, r0
# e := e + 1;
    load r0, 4
    int_const r1, 1
    add_int r0, r0, r1
    store 4, r0
# f := f + 1;
    load r0, 5
    int_const r1, 1
    add_int r0, r0, r1
    store 5, r0
# fi
label_17:
# if ((a <= 100) || ((b <= 110) && (c <= 200))) || (((d <= 1100) && (e <= 10100)) && (f <= 20100))
    load r0, 0
    int_const r1, 100
    cmp_le_int r0, r0, r1
    branch_on_true r0, label_23
    branch_uncond label_26
label_26:
    load r0, 1
    int_const r1, 110
    cmp_le_int r0, r0, r1
    branch_on_true r0, label_27
    branch_uncond label_25
label_27:
    load r0, 2
    int_const r1, 200
    cmp_le_int r0, r0, r1
    branch_on_true r0, label_23
    branch_uncond label_25
label_25:
    load r0, 3
    int_const r1, 1100
    cmp_le_int r0, r0, r1
    branch_on_true r0, label_29
    branch_uncond label_24
label_29:
    load r0, 4
    int_const r1, 10100
    cmp_le_int r0, r0, r1
    branch_on_true r0, label_28
    branch_uncond label_24
label_28:
    load r0, 5
    int_const r1, 20100
    cmp_le_int r0, r0, r1
    branch_on_true r0, label_23
    branch_uncond label_24
label_23:
# then
# a := a + 1;
    load r0, 0
    int_const r1, 1
    add_int r0, r0, r1
    store 0, r0
# b := b + 1;
    load r0, 1
    int_const r1, 1
    add_int r0, r0, r1
    store 1, r0
# c := c + 1;
    load r0, 2
    int_const r1, 1
    add_int r0, r0, r1
    store 2, r0
# d := d + 1;
    load r0, 3
    int_const r1, 1
    add_int r0, r0, r1
    store 3, r0
# e := e + 1;
    load r0, 4
    int_const r1, 1
    add_int r0, r0, r1
    store 4, r0
# f := f + 1;
    load r0, 5
    int_const r1, 1
    add_int r0, r0, r1
    store 5, r0
# fi
label_24:
# epilogue
    pop_stack_frame 6
    return
