    call proc_main
    halt
proc_main:
# prologue
    push_stack_frame 205
# initialise float val a[10,20]
    real_const r0, 0.0
    store 0, r0
    store 1, r0
    store 2, r0
    store 3, r0
    store 4, r0
    store 5, r0
    store 6, r0
    store 7, r0
    store 8, r0
    store 9, r0
    store 10, r0
    store 11, r0
    store 12, r0
    store 13, r0
    store 14, r0
    store 15, r0
    store 16, r0
    store 17, r0
    store 18, r0
    store 19, r0
    store 20, r0
    store 21, r0
    store 22, r0
    store 23, r0
    store 24, r0
    store 25, r0
    store 26, r0
    store 27, r0
    store 28, r0
    store 29, r0
    store 30, r0
    store 31, r0
    store 32, r0
    store 33, r0
    store 34, r0
    store 35, r0
    store 36, r0
    store 37, r0
    store 38, r0
    store 39, r0
    store 40, r0
    store 41, r0
    store 42, r0
    store 43, r0
    store 44, r0
    store 45, r0
    store 46, r0
    store 47, r0
    store 48, r0
    store 49, r0
    store 50, r0
    store 51, r0
    store 52, r0
    store 53, r0
    store 54, r0
    store 55, r0
    store 56, r0
    store 57, r0
    store 58, r0
    store 59, r0
    store 60, r0
    store 61, r0
    store 62, r0
    store 63, r0
    store 64, r0
    store 65, r0
    store 66, r0
    store 67, r0
    store 68, r0
    store 69, r0
    store 70, r0
    store 71, r0
    store 72, r0
    store 73, r0
    store 74, r0
    store 75, r0
    store 76, r0
    store 77, r0
    store 78, r0
    store 79, r0
    store 80, r0
    store 81, r0
    store 82, r0
    store 83, r0
    store 84, r0
    store 85, r0
    store 86, r0
    store 87, r0
    store 88, r0
    store 89, r0
    store 90, r0
    store 91, r0
    store 92, r0
    store 93, r0
    store 94, r0
    store 95, r0
    store 96, r0
    store 97, r0
    store 98, r0
    store 99, r0
    store 100, r0
    store 101, r0
    store 102, r0
    store 103, r0
    store 104, r0
    store 105, r0
    store 106, r0
    store 107, r0
    store 108, r0
    store 109, r0
    store 110, r0
    store 111, r0
    store 112, r0
    store 113, r0
    store 114, r0
    store 115, r0
    store 116, r0
    store 117, r0
    store 118, r0
    store 119, r0
    store 120, r0
    store 121, r0
    store 122, r0
    store 123, r0
    store 124, r0
    store 125, r0
    store 126, r0
    store 127, r0
    store 128, r0
    store 129, r0
    store 130, r0
    store 131, r0
    store 132, r0
    store 133, r0
    store 134, r0
    store 135, r0
    store 136, r0
    store 137, r0
    store 138, r0
    store 139, r0
    store 140, r0
    store 141, r0
    store 142, r0
    store 143, r0
    store 144, r0
    store 145, r0
    store 146, r0
    store 147, r0
    store 148, r0
    store 149, r0
    store 150, r0
    store 151, r0
    store 152, r0
    store 153, r0
    store 154, r0
    store 155, r0
    store 156, r0
    store 157, r0
    store 158, r0
    store 159, r0
    store 160, r0
    store 161, r0
    store 162, r0
    store 163, r0
    store 164, r0
    store 165, r0
    store 166, r0
    store 167, r0
    store 168, r0
    store 169, r0
    store 170, r0
    store 171, r0
    store 172, r0
    store 173, r0
    store 174, r0
    store 175, r0
    store 176, r0
    store 177, r0
    store 178, r0
    store 179, r0
    store 180, r0
    store 181, r0
    store 182, r0
    store 183, r0
    store 184, r0
    store 185, r0
    store 186, r0
    store 187, r0
    store 188, r0
    store 189, r0
    store 190, r0
    store 191, r0
    store 192, r0
    store 193, r0
    store 194, r0
    store 195, r0
    store 196, r0
    store 197, r0
    store 198, r0
    store 199, r0
# initialise int val a_m
    int_const r0, 0
    store 200, r0
# initialise int val a_n
    int_const r0, 0
    store 201, r0
# initialise int val m
    int_const r0, 0
    store 202, r0
# initialise int val n
    int_const r0, 0
    store 203, r0
# initialise int val x
    int_const r0, 0
    store 204, r0
# write "Please provide an integer: ";
    string_const r0, "Please provide an integer: "
    call_builtin print_string
# read x;
    call_builtin read_int
    store 204, r0
# a_m := 10;
    int_const r0, 10
    store 200, r0
# a_n := 20;
    int_const r0, 20
    store 201, r0
# m := 0;
    int_const r0, 0
    store 202, r0
# while m < a_m
label_0:
    load r0, 202
    load r1, 200
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_1
    branch_uncond label_2
label_1:
# do
# n := 0;
    int_const r0, 0
    store 203, r0
# while n < a_n
label_3:
    load r0, 203
    load r1, 201
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_4
    branch_uncond label_5
label_4:
# do
# call mul_by_two(x, a[m, n]);
    load r0, 204
    int_to_real r0, r0
    load r1, 202
    load r2, 203
    int_const r3, 20
    mul_int r1, r3, r1
    add_int r1, r2, r1
    load_address r2, 0
    sub_offset r1, r2, r1
    call proc_mul_by_two
# n := n + 1;
    load r0, 203
    int_const r1, 1
    add_int r0, r0, r1
    store 203, r0
    branch_uncond label_3
# od
label_5:
# m := m + 1;
    load r0, 202
    int_const r1, 1
    add_int r0, r0, r1
    store 202, r0
    branch_uncond label_0
# od
label_2:
# write "Verifying array contents\n";
    string_const r0, "Verifying array contents\n"
    call_builtin print_string
# write a[6, 8];
    int_const r0, 6
    int_const r1, 8
    int_const r2, 20
    mul_int r0, r2, r0
    add_int r0, r0, r1
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    call_builtin print_real
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# epilogue
    pop_stack_frame 205
    return
proc_mul_by_two:
# prologue
    push_stack_frame 2
# parameter passing
    store 0, r0
    store 1, r1
# out := in * 2;
    load r0, 0
    int_const r1, 2
    int_to_real r1, r1
    mul_real r0, r0, r1
    load r1, 1
    store_indirect r1, r0
# epilogue
    pop_stack_frame 2
    return
