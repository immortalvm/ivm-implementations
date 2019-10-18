### iVM assembly language introduction.
###
### Part 3 - Advanced
###
### This is part 3 of the iVM assembly language introduction.


### 1. THE HEAP

    ## The code prepended to every executable stores a pointer to the start of
    ## the heap in the 8 bytes preceding the rest of the code:
main:
    heap_start = (load8 (+ main -8))

    ## Compiled C programs should only access the heap through libc.


### 2. ARGUMENT FILE

    ## Similarly, we also have a pointer to the argument file:

    arg_location = (load8 (+ main -16))

    ## More precisely, arg_location is the address of a memory location holding
    ## the length of the argument file. The file contents follows.

    arg_length = (load8 arg_location)
    arg_start = (+ arg_location 8)
    arg_stop = (+ arg_start arg_length)

    push! arg_length            # Push 0 if there is no argument file.


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
### 0
