    call proc_main
    halt
proc_main:
# prologue
    push_stack_frame 7
# initialise int val a[5]
    int_const r0, 0
    store 0, r0
    int_const r1, 0
    store 1, r1
    int_const r2, 0
    store 2, r2
    int_const r3, 0
    store 3, r3
    int_const r4, 0
    store 4, r4
# initialise int val b
    int_const r0, 0
    store 5, r0
# initialise int val c
    int_const r0, 0
    store 6, r0
# write "BAAAAAAAAAAAAAAAAAAAAAAAAAA!\n";
    string_const r0, "BAAAAAAAAAAAAAAAAAAAAAAAAAA!\n"
    call_builtin print_string
# write "Welcome to quantum bogo sort, for additional info please visit: http://wiki.c2.com/?QuantumBogoSort\n";
    string_const r0, "Welcome to quantum bogo sort, for additional info please visit: http://wiki.c2.com/?QuantumBogoSort\n"
    call_builtin print_string
# write "disclaimer, this algorithim may destory your universe, the author of this code will not take any responsibilities for the annhiliation of your world.\n";
    string_const r0, "disclaimer, this algorithim may destory your universe, the author of this code will not take any responsibilities for the annhiliation of your world.\n"
    call_builtin print_string
# write "Please press 1 to continue, press 2 to terminate\n";
    string_const r0, "Please press 1 to continue, press 2 to terminate\n"
    call_builtin print_string
# read b;
    call_builtin read_int
    store 5, r0
# if b = 1
    load r0, 5
    int_const r1, 1
    cmp_eq_int r0, r0, r1
    branch_on_true r0, label_0
    branch_uncond label_1
label_0:
# then
# write "Please enter a random number\n";
    string_const r0, "Please enter a random number\n"
    call_builtin print_string
# read a[0];
    call_builtin read_int
    int_const r1, 0
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# write "Please enter more numbers\n";
    string_const r0, "Please enter more numbers\n"
    call_builtin print_string
# read a[1];
    call_builtin read_int
    int_const r1, 1
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# write "Please enter more numbers\n";
    string_const r0, "Please enter more numbers\n"
    call_builtin print_string
# read a[2];
    call_builtin read_int
    int_const r1, 2
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# write "Please enter more numbers\n";
    string_const r0, "Please enter more numbers\n"
    call_builtin print_string
# read a[3];
    call_builtin read_int
    int_const r1, 3
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# write "Please enter more number\n";
    string_const r0, "Please enter more number\n"
    call_builtin print_string
# read a[4];
    call_builtin read_int
    int_const r1, 4
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# write "Begin Sorting";
    string_const r0, "Begin Sorting"
    call_builtin print_string
# c := 0;
    int_const r0, 0
    store 6, r0
# while c < 5
label_3:
    load r0, 6
    int_const r1, 5
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_4
    branch_uncond label_5
label_4:
# do
# if a[c] > a[c - 1]
    load r0, 6
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    load r1, 6
    int_const r2, 1
    sub_int r1, r1, r2
    load_address r2, 0
    sub_offset r1, r2, r1
    load_indirect r1, r1
    cmp_gt_int r0, r0, r1
    branch_on_true r0, label_6
    branch_uncond label_7
label_6:
# then
# write "TERMINATING UNIVERSE PLEASE STAND BY\n";
    string_const r0, "TERMINATING UNIVERSE PLEASE STAND BY\n"
    call_builtin print_string
# write "ERROR: CANNOT FIND UNIVERSE TERMINATING PROGRAM\n";
    string_const r0, "ERROR: CANNOT FIND UNIVERSE TERMINATING PROGRAM\n"
    call_builtin print_string
# write "A SUPPORT TICKET HAS BEEN SUBMITTED TO MICROSOFT TECH SUPPORT\n";
    string_const r0, "A SUPPORT TICKET HAS BEEN SUBMITTED TO MICROSOFT TECH SUPPORT\n"
    call_builtin print_string
# c := 100;
    int_const r0, 100
    store 6, r0
# fi
label_7:
# c := c + 1;
    load r0, 6
    int_const r1, 1
    add_int r0, r0, r1
    store 6, r0
    branch_uncond label_3
# od
label_5:
    branch_uncond label_2
label_1:
# else
# write "Stopping";
    string_const r0, "Stopping"
    call_builtin print_string
# fi
label_2:
# epilogue
    pop_stack_frame 7
    return
