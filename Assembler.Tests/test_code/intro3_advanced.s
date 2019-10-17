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


### 2. ARGUMENT FILES

    ## As of October  2019 how to deal  with runtime argument files  is still in
    ## flux. In particular, the result of providing a non-empty argument file is
    ## unpredictable if  the program  uses 'space'.  Otherwise, the  contents of
    ## the argument file is initially found at 'head_start'.

    argument_start = heap_start

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
