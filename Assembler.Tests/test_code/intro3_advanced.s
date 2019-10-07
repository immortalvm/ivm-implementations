### iVM assembly language introduction.
###
### Part 3 - Advanced
###
### This is part 3 of the iVM assembly language introduction.


### 1. THE ARGUMENT FILE

    ## How  to deal  with  runtime arguments  has not  yet  been finalized,  but
    ## currently it  works as  follows: The code  prepended to  every executable
    ## binary (which  creates a new  stack) stores a  pointer to the  first byte
    ## after the contents of the argument file in the 8 bytes preceding the rest
    ## of the code. Thus, we can reach this data through this pointer.
main:
    argument_stop = (load8 (+ main -8))

    ## Beware that if the argument file is empty (or not provided),
    ## argument_stop will probably be an unallocated memory address.


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
