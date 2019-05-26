    call proc_main
    halt
proc_main:
# prologue
    push_stack_frame 5
# initialise int val arr[4]
    int_const r0, 0
    store 0, r0
    store 1, r0
    store 2, r0
    store 3, r0
# initialise int val tmp
    int_const r0, 0
    store 4, r0
# write "Please input 4 integers: ";
    string_const r0, "Please input 4 integers: "
    call_builtin print_string
# read arr[0];
    call_builtin read_int
    int_const r1, 0
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# read arr[1];
    call_builtin read_int
    int_const r1, 1
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# read arr[2];
    call_builtin read_int
    int_const r1, 2
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# read arr[3];
    call_builtin read_int
    int_const r1, 3
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# tmp := arr[0];
    int_const r0, 0
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    store 4, r0
# arr[0] := arr[3];
    int_const r0, 3
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    int_const r1, 0
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# arr[3] := tmp;
    load r0, 4
    int_const r1, 3
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# tmp := arr[1];
    int_const r0, 1
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    store 4, r0
# arr[1] := arr[2];
    int_const r0, 2
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    int_const r1, 1
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# arr[2] := tmp;
    load r0, 4
    int_const r1, 2
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# write "The reversed array is: ";
    string_const r0, "The reversed array is: "
    call_builtin print_string
# write arr[0];
    int_const r0, 0
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    call_builtin print_int
# write " ";
    string_const r0, " "
    call_builtin print_string
# write arr[1];
    int_const r0, 1
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    call_builtin print_int
# write " ";
    string_const r0, " "
    call_builtin print_string
# write arr[2];
    int_const r0, 2
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    call_builtin print_int
# write " ";
    string_const r0, " "
    call_builtin print_string
# write arr[3];
    int_const r0, 3
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    call_builtin print_int
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# epilogue
    pop_stack_frame 5
    return
