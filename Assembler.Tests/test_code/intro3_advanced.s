### iVM assembly language introduction.
###
### Part 3 - Advanced
###
### This is part 3 of the iVM assembly language introduction.


### 1. MEMORY MAP WITH NO ENTRY POINT

    ## In most cases you should specify an entry point when calling the
    ## assembler, e.g. "-e main". This situation is explained in section 2
    ## below.

    ## When an  entry point is  _not_ specified,  execution starts at  the first
    ## statement of the first source file.  Moreover, there is code prepended to
    ## the executable which stores  a pointer to the start of the  heap in the 8
    ## bytes preceding the rest of the code:
program:
    heap_start = (load8 (+ program -8))

    ## Similarly, we also have a pointer to the argument file:
    arg_location = (load8 (+ program -16))

    ## More precisely, arg_location is the address of a memory location holding
    ## the length of the argument file. The file contents follows.
    arg_length = (load8 arg_location)
    arg_start = (+ arg_location 8)
    arg_stop = (+ arg_start arg_length)


### 2. MEMORY MAP WITH ENTRY POINT

    ## If an entry point _was_ specified ("-e main"), then this is where
    ## execution starts, and the stack looks like this:
    ##
    ## 0: return address
    ## 1: start of argument array
    ## 2: length of argument array
    ## 3: start of free heap space

    ## The entry point is essentially called like this:
    push!!! heap_start arg_length arg_start
    call! main
    set_sp! &2
    exit

    ## The top of the end stack is (truncated and) used as the exit status of
    ## the virtual machine (unless it is run with "ivm check"). Hence, the
    ## pointer to the heap start should be overwritten with 0 to signal success.

    ## Observe that the stack and heap share the same memory. In other words,
    ## the stack pointer marks the end of the heap.

main:


### 3. COMPLEX DATA STATEMENTS

    ## Whereas  the  expressions  used  to  initialize  data  segments  must  be
    ## "assembly  time"  constants,  they  may involve  both  abbreviations  and
    ## labels.
    some_constant = 17
    jump! after_data
my_data:
    data4 [ (* some_constant (+ after_data -main)) ]
after_data:
    # Set exit status 119 (17*7).
    store8!! (load4 my_data) &3


### 4.

    ## To be continued...
    return


### EXPECTED STACK:
###
### 119
