### iVM assembly language introduction.
###
### Part 3 - Advanced
###
### This is part 3 of the iVM assembly language introduction.


### 1. THE INITIAL STACK POINTER

    ## Here we allocate a new stack without loosing the old stack pointer.
    ## We do this this with an initial stack of only 24 bytes.
    store8!! &0 initial_stack_pointer
    jump! allocate_new_stack
initial_stack_pointer:
    data [0 0 0 0 0 0 0 0]      # 64 bits
allocate_new_stack:
    stack_size = 16384
    allocate! stack_size
    add! stack_size
    set_sp


### 2.

    ## To be continued...

    exit

### EXPECTED STACK:
