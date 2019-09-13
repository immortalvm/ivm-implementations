### Write number to text channel

    EXPORT write_decimal_number

    stack_size = 16384
    allocate! stack_size
    add! stack_size
    set_sp

    push! 12345678901234567890  # Push argument (64 bits)
    call! write_decimal_number
    set_sp! &1                  # Pop argument
    put_char! 10                # Write newline
    exit

write_decimal_number:
    ## 0: return address, 1: argument
    push! (/u $1 10)
    jump_zero!! $0 next
    call! write_decimal_number  # Recursive call
next:
    set_sp! &1
    zero = 48
    put_char! (+ (%u $1 10) zero)
    return

### EXPECTED STACK:
