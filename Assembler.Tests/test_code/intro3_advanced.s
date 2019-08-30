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
    data8 [0]                   # 64 bits
allocate_new_stack:
    stack_size = 16384
    allocate! stack_size
    add! stack_size
    set_sp


### 2. COMPLEX DATA STATEMENTS

    ## Whereas  the  expressions  used  to  initialize  data  segments  must  be
    ## "assembly  time"  constants,  they  may involve  both  abbreviations  and
    ## labels.
    some_constant = 17
    jump! after_data
my_data:
    data4 [ (* some_constant (+ after_data -initial_stack_pointer)) ]
after_data:
    push! (load4 my_data)       # Push 408 (17*24) onto the stack.


### 3.

    ## To be continued...

    exit


### EXPECTED STACK:
###
### 408
