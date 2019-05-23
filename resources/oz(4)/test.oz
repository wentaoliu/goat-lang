    call proc_main
    halt
proc_main:
    real_const r0, 1.04
    call_builtin print_real
    return