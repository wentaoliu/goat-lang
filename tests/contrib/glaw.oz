    call proc_main
    halt
proc_binary_search:
# prologue
    push_stack_frame 11
# initialise int val collection[5]
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
# initialise bool val flag
    int_const r0, 0
    store 5, r0
# initialise int val high
    int_const r0, 0
    store 6, r0
# initialise int val length
    int_const r0, 0
    store 7, r0
# initialise int val low
    int_const r0, 0
    store 8, r0
# initialise int val mid
    int_const r0, 0
    store 9, r0
# initialise int val x
    int_const r0, 0
    store 10, r0
# collection[0] := 1;
    int_const r0, 1
    int_const r1, 0
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# collection[1] := 2;
    int_const r0, 2
    int_const r1, 1
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# collection[2] := 3;
    int_const r0, 3
    int_const r1, 2
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# collection[3] := 4;
    int_const r0, 4
    int_const r1, 3
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# collection[4] := 5;
    int_const r0, 5
    int_const r1, 4
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# length := 5;
    int_const r0, 5
    store 7, r0
# x := 3;
    int_const r0, 3
    store 10, r0
# low := 0;
    int_const r0, 0
    store 8, r0
# high := length - 1;
    load r0, 7
    int_const r1, 1
    sub_int r0, r0, r1
    store 6, r0
    branch_uncond label_1
label_0:
    int_const r0, 1
    store 5, r0
    branch_uncond label_2
label_1:
    int_const r0, 0
    store 5, r0
label_2:
# write "collection: ";
    string_const r0, "collection: "
    call_builtin print_string
# write collection[0];
    int_const r0, 0
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    call_builtin print_int
# write collection[1];
    int_const r0, 1
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    call_builtin print_int
# write collection[2];
    int_const r0, 2
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    call_builtin print_int
# write collection[3];
    int_const r0, 3
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    call_builtin print_int
# write collection[4];
    int_const r0, 4
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    call_builtin print_int
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# write "x: ";
    string_const r0, "x: "
    call_builtin print_string
# write x;
    load r0, 10
    call_builtin print_int
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# while low <= high
label_3:
    load r0, 8
    load r1, 6
    cmp_le_int r0, r0, r1
    branch_on_true r0, label_4
    branch_uncond label_5
label_4:
# do
# mid := (low + high) / 2;
    load r0, 8
    load r1, 6
    add_int r0, r0, r1
    int_const r1, 2
    div_int r0, r0, r1
    store 9, r0
# if x = collection[mid]
    load r0, 10
    load r1, 9
    load_address r2, 0
    sub_offset r1, r2, r1
    load_indirect r1, r1
    cmp_eq_int r0, r0, r1
    branch_on_true r0, label_6
    branch_uncond label_7
label_6:
# then
    branch_uncond label_9
label_9:
    int_const r0, 1
    store 5, r0
    branch_uncond label_11
label_10:
    int_const r0, 0
    store 5, r0
label_11:
# low := high;
    load r0, 6
    store 8, r0
    branch_uncond label_8
label_7:
# else
# if x < collection[mid]
    load r0, 10
    load r1, 9
    load_address r2, 0
    sub_offset r1, r2, r1
    load_indirect r1, r1
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_12
    branch_uncond label_13
label_12:
# then
# high := mid - 1;
    load r0, 9
    int_const r1, 1
    sub_int r0, r0, r1
    store 6, r0
    branch_uncond label_14
label_13:
# else
# low := mid + 1;
    load r0, 9
    int_const r1, 1
    add_int r0, r0, r1
    store 8, r0
# fi
label_14:
# fi
label_8:
    branch_uncond label_3
# od
label_5:
# if flag
    load r0, 5
    branch_on_true r0, label_15
    branch_uncond label_16
label_15:
# then
# write "x is found\n";
    string_const r0, "x is found\n"
    call_builtin print_string
    branch_uncond label_17
label_16:
# else
# write "x is not found\n";
    string_const r0, "x is not found\n"
    call_builtin print_string
# fi
label_17:
# epilogue
    pop_stack_frame 11
    return
proc_factorial_recurse:
# prologue
    push_stack_frame 2
# parameter passing
    store 0, r0
    store 1, r1
# if num = 0
    load r0, 0
    int_const r1, 0
    cmp_eq_int r0, r0, r1
    branch_on_true r0, label_18
    branch_uncond label_19
label_18:
# then
# ret := 1;
    int_const r0, 1
    load r1, 1
    store_indirect r1, r0
    branch_uncond label_20
label_19:
# else
# call factorial_recurse(num - 1, ret);
    load r0, 0
    int_const r1, 1
    sub_int r0, r0, r1
    load r1, 1
    call proc_factorial_recurse
# ret := num * ret;
    load r0, 0
    load r1, 1
    load_indirect r1, r1
    mul_int r0, r0, r1
    load r1, 1
    store_indirect r1, r0
# fi
label_20:
# epilogue
    pop_stack_frame 2
    return
proc_factorial:
# prologue
    push_stack_frame 2
# initialise int val ans
    int_const r0, 0
    store 0, r0
# initialise int val num
    int_const r0, 0
    store 1, r0
# num := 5;
    int_const r0, 5
    store 1, r0
# ans := 1;
    int_const r0, 1
    store 0, r0
# call factorial_recurse(num, ans);
    load r0, 1
    load_address r1, 0
    call proc_factorial_recurse
# write "factorial of ";
    string_const r0, "factorial of "
    call_builtin print_string
# write num;
    load r0, 1
    call_builtin print_int
# write " is ";
    string_const r0, " is "
    call_builtin print_string
# write ans;
    load r0, 0
    call_builtin print_int
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# epilogue
    pop_stack_frame 2
    return
proc_fibonacci_recurse:
# prologue
    push_stack_frame 4
# parameter passing
    store 0, r0
    store 1, r1
# initialise int val ret1
    int_const r0, 0
    store 2, r0
# initialise int val ret2
    int_const r0, 0
    store 3, r0
# if num = 0
    load r0, 0
    int_const r1, 0
    cmp_eq_int r0, r0, r1
    branch_on_true r0, label_21
    branch_uncond label_22
label_21:
# then
# ret := 0;
    int_const r0, 0
    load r1, 1
    store_indirect r1, r0
    branch_uncond label_23
label_22:
# else
# if num = 1
    load r0, 0
    int_const r1, 1
    cmp_eq_int r0, r0, r1
    branch_on_true r0, label_24
    branch_uncond label_25
label_24:
# then
# ret := 1;
    int_const r0, 1
    load r1, 1
    store_indirect r1, r0
    branch_uncond label_26
label_25:
# else
# ret1 := 0;
    int_const r0, 0
    store 2, r0
# call fibonacci_recurse(num - 1, ret1);
    load r0, 0
    int_const r1, 1
    sub_int r0, r0, r1
    load_address r1, 2
    call proc_fibonacci_recurse
# ret2 := 0;
    int_const r0, 0
    store 3, r0
# call fibonacci_recurse(num - 2, ret2);
    load r0, 0
    int_const r1, 2
    sub_int r0, r0, r1
    load_address r1, 3
    call proc_fibonacci_recurse
# ret := ret1 + ret2;
    load r0, 2
    load r1, 3
    add_int r0, r0, r1
    load r1, 1
    store_indirect r1, r0
# fi
label_26:
# fi
label_23:
# epilogue
    pop_stack_frame 4
    return
proc_fibonacci:
# prologue
    push_stack_frame 2
# initialise int val ans
    int_const r0, 0
    store 0, r0
# initialise int val num
    int_const r0, 0
    store 1, r0
# num := 8;
    int_const r0, 8
    store 1, r0
# ans := 0;
    int_const r0, 0
    store 0, r0
# call fibonacci_recurse(num, ans);
    load r0, 1
    load_address r1, 0
    call proc_fibonacci_recurse
# write "fibonacci of ";
    string_const r0, "fibonacci of "
    call_builtin print_string
# write num;
    load r0, 1
    call_builtin print_int
# write " is ";
    string_const r0, " is "
    call_builtin print_string
# write ans;
    load r0, 0
    call_builtin print_int
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# epilogue
    pop_stack_frame 2
    return
proc_selection_sort:
# prologue
    push_stack_frame 8
# initialise int val collection[3]
    int_const r0, 0
    store 0, r0
    int_const r1, 0
    store 1, r1
    int_const r2, 0
    store 2, r2
# initialise int val i
    int_const r0, 0
    store 3, r0
# initialise int val j
    int_const r0, 0
    store 4, r0
# initialise int val length
    int_const r0, 0
    store 5, r0
# initialise int val min
    int_const r0, 0
    store 6, r0
# initialise int val tmp
    int_const r0, 0
    store 7, r0
# length := 3;
    int_const r0, 3
    store 5, r0
# collection[0] := 3;
    int_const r0, 3
    int_const r1, 0
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# collection[1] := 6;
    int_const r0, 6
    int_const r1, 1
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# collection[2] := 2;
    int_const r0, 2
    int_const r1, 2
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# write "unsorted: ";
    string_const r0, "unsorted: "
    call_builtin print_string
# write collection[0];
    int_const r0, 0
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    call_builtin print_int
# write collection[1];
    int_const r0, 1
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    call_builtin print_int
# write collection[2];
    int_const r0, 2
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    call_builtin print_int
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# i := 0;
    int_const r0, 0
    store 3, r0
# while i < (length - 1)
label_27:
    load r0, 3
    load r1, 5
    int_const r2, 1
    sub_int r1, r1, r2
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_28
    branch_uncond label_29
label_28:
# do
# min := i;
    load r0, 3
    store 6, r0
# j := i + 1;
    load r0, 3
    int_const r1, 1
    add_int r0, r0, r1
    store 4, r0
# while j < length
label_30:
    load r0, 4
    load r1, 5
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_31
    branch_uncond label_32
label_31:
# do
# if collection[j] < collection[min]
    load r0, 4
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    load r1, 6
    load_address r2, 0
    sub_offset r1, r2, r1
    load_indirect r1, r1
    cmp_lt_int r0, r0, r1
    branch_on_true r0, label_33
    branch_uncond label_34
label_33:
# then
# min := j;
    load r0, 4
    store 6, r0
# fi
label_34:
# j := j + 1;
    load r0, 4
    int_const r1, 1
    add_int r0, r0, r1
    store 4, r0
    branch_uncond label_30
# od
label_32:
# tmp := collection[min];
    load r0, 6
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    store 7, r0
# collection[min] := collection[i];
    load r0, 3
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    load r1, 6
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# collection[i] := tmp;
    load r0, 7
    load r1, 3
    load_address r2, 0
    sub_offset r1, r2, r1
    store_indirect r1, r0
# i := i + 1;
    load r0, 3
    int_const r1, 1
    add_int r0, r0, r1
    store 3, r0
    branch_uncond label_27
# od
label_29:
# write "sorted: ";
    string_const r0, "sorted: "
    call_builtin print_string
# write collection[0];
    int_const r0, 0
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    call_builtin print_int
# write collection[1];
    int_const r0, 1
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    call_builtin print_int
# write collection[2];
    int_const r0, 2
    load_address r1, 0
    sub_offset r0, r1, r0
    load_indirect r0, r0
    call_builtin print_int
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# epilogue
    pop_stack_frame 8
    return
proc_move:
# prologue
    push_stack_frame 4
# parameter passing
    store 1, r0
    store 2, r1
    store 0, r2
    store 3, r3
# if disks > 0
    load r0, 1
    int_const r1, 0
    cmp_gt_int r0, r0, r1
    branch_on_true r0, label_35
    branch_uncond label_36
label_35:
# then
# call move(disks - 1, source, target, auxiliary);
    load r0, 1
    int_const r1, 1
    sub_int r0, r0, r1
    load r1, 2
    load r2, 3
    load r3, 0
    call proc_move
# write "Move disk ";
    string_const r0, "Move disk "
    call_builtin print_string
# write disks;
    load r0, 1
    call_builtin print_int
# write " from ";
    string_const r0, " from "
    call_builtin print_string
# write source;
    load r0, 2
    call_builtin print_int
# write "->";
    string_const r0, "->"
    call_builtin print_string
# write target;
    load r0, 3
    call_builtin print_int
# write "\n";
    string_const r0, "\n"
    call_builtin print_string
# call move(disks - 1, auxiliary, source, target);
    load r0, 1
    int_const r1, 1
    sub_int r0, r0, r1
    load r1, 0
    load r2, 2
    load r3, 3
    call proc_move
# fi
label_36:
# epilogue
    pop_stack_frame 4
    return
proc_tower_of_hanoi:
# prologue
    push_stack_frame 1
# initialise int val N
    int_const r0, 0
    store 0, r0
# N := 3;
    int_const r0, 3
    store 0, r0
# call move(N, 1, 2, 3);
    load r0, 0
    int_const r1, 1
    int_const r2, 2
    int_const r3, 3
    call proc_move
# epilogue
    pop_stack_frame 1
    return
proc_main:
# prologue
    push_stack_frame 0
# write "binary_search:\n";
    string_const r0, "binary_search:\n"
    call_builtin print_string
# call binary_search();
    call proc_binary_search
# write "\nfactorial:\n";
    string_const r0, "\nfactorial:\n"
    call_builtin print_string
# call factorial();
    call proc_factorial
# write "\nfibonacci:\n";
    string_const r0, "\nfibonacci:\n"
    call_builtin print_string
# call fibonacci();
    call proc_fibonacci
# write "\nselection_sort:\n";
    string_const r0, "\nselection_sort:\n"
    call_builtin print_string
# call selection_sort();
    call proc_selection_sort
# write "\ntower_of_hanoi:\n";
    string_const r0, "\ntower_of_hanoi:\n"
    call_builtin print_string
# call tower_of_hanoi();
    call proc_tower_of_hanoi
# epilogue
    pop_stack_frame 0
    return
