
    stack_size = 100

    alloc! stack_size
    add! stack_size
    set_sp

    push! 10
loop:
    add! -1
    load8! &0
    jump_not_zero! loop
    exit

    # Make room for initial stack.
    data [0 0 0 0 0 0 0 0]
    data [0 0 0 0 0 0 0 0]

    # EXPECTED END STACK:
    # 0
