    call proc_main
    halt
proc_main:
# prologue
    push_stack_frame 6
# initialise int val difference
    int_const r0, 0
    store 0, r0
# initialise int val division_result
    int_const r0, 0
    store 1, r0
# initialise int val num_1
    int_const r0, 0
    store 2, r0
# initialise int val num_2
    int_const r0, 0
    store 3, r0
# initialise int val product
    int_const r0, 0
    store 4, r0
# initialise int val sum
    int_const r0, 0
    store 5, r0
# write "Hello, give us two numbers, we will do some simple arithmetic operations for you: ";
    string_const r0, "Hello, give us two numbers, we will do some simple arithmetic operations for you: "
    call_builtin print_string
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# write "Please enter the first number: ";
    string_const r0, "Please enter the first number: "
    call_builtin print_string
# read num_1;
    call_builtin read_int
    store 2, r0
# write "Please enter the second number: ";
    string_const r0, "Please enter the second number: "
    call_builtin print_string
# read num_2;
    call_builtin read_int
    store 3, r0
# write "\nNice, the two numbers you entered were ";
    string_const r0, "\nNice, the two numbers you entered were "
    call_builtin print_string
# write num_1;
    load r0, 2
    call_builtin print_int
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# write num_2;
    load r0, 3
    call_builtin print_int
# write "\nThe sum of the numbers are: ";
    string_const r0, "\nThe sum of the numbers are: "
    call_builtin print_string
# sum := num_1 + num_2;
    load r0, 2
    load r1, 3
    add_int r0, r0, r1
    store 5, r0
# write sum;
    load r0, 5
    call_builtin print_int
# write "\nThe difference of the numbers are: ";
    string_const r0, "\nThe difference of the numbers are: "
    call_builtin print_string
# difference := num_1 - num_2;
    load r0, 2
    load r1, 3
    sub_int r0, r0, r1
    store 0, r0
# write difference;
    load r0, 0
    call_builtin print_int
# write "\nThe product of the numbers are: ";
    string_const r0, "\nThe product of the numbers are: "
    call_builtin print_string
# product := num_1 * num_2;
    load r0, 2
    load r1, 3
    mul_int r0, r0, r1
    store 4, r0
# write product;
    load r0, 4
    call_builtin print_int
# write "\nThe division result of the numbers are: ";
    string_const r0, "\nThe division result of the numbers are: "
    call_builtin print_string
# product := num_1 / num_2;
    load r0, 2
    load r1, 3
    div_int r0, r0, r1
    store 4, r0
# write division_result;
    load r0, 1
    call_builtin print_int
# epilogue
    pop_stack_frame 6
    return
