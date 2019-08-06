### Offsets

    stack_size = 16384
    allocate! stack_size
    add! stack_size
    set_sp

    push! 3
    push! $0
    add

    push! 7
    push! &4
    push! -32
    add
    load8
    add

    exit

    ## Make room for initial stack.
    data [0 0 0 0 0 0 0 0]
    data [0 0 0 0 0 0 0 0]

### EXPECTED STACK:
###
### 14
### 6
