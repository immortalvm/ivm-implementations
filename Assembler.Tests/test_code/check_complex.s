###
###
###
###
###
###
###
###
###

### 1. THE STACK

    ## Fresh stack size in bytes, normally a multiple of 8
    stack_size = 16384
    ## Important: Also add an empty data segment  of 16 bytes or more at the end
    ## of  this  file for  the  initial  stack.  Otherwise,  your data  will  be
    ## overwritten when we set up the fresh stack:
    alloc! stack_size
    add! stack_size
    set_sp


    ## It is often necessary push a copy of an element in the stack onto the
    ## stack. This can be done as follows:
    push!!!! 13 12 11 10        # Push 4 numbers onto the stack, so that it is
                                # not empty.
    push!! $0 $3                # Push the stack elements 0 and 3 (counting from 0).

    ## Be careful to use the right number of exclamation marks.
    ## Otherwise, the parser gets very confused.
    ## Now the stack is (13, 10, 10, 11, 12, 13) from the top,
    ## since the second statement is sugar for:
    push! $0
    push! $4                    # Notice the offset.

    ## Now the stack is (11, 13, 13, 10, 10, 11, 12, 13)

    ## The previous statement is sugar for:
    push! (load8 &4)

    ## which is sugar for
    push! &4
    load8

    ## In other words, &n is the address of the element n on the stack.
    ## This is useful when we want to pop elements from the stack:
    set_sp! &10

    ## Now the stack is empty again.


### 2. LABELS

    ## A data segment can be used for shared global variables.
    jump! after_x               # Equivalent to 'jump! (+ x 1)'
x:
    data [0]
after_x:
    store1!! (+ 1 (load1 x)) x  # Increase x with 1

    ## The statement above is simply (syntactic) sugar for:
    push! (+ 1 (load1 x))
    push! x
    store1

    ## Moreover, the first push statement is sugar for:
    push! 1
    push! (load1 x)
    add

    ## Finally, the second of these statements is sugar for:
    push! x
    load1

    ## Now the byte at x is 2, and the stack is (2, 3) from the top.

    ## Observe  that x  is  not translated  into a  constant  by the  assembler.
    ## Whereas we plan to add an  optimization for the initial program (which is
    ## guaranteed to  be located at  address 0) the  default mode is  to produce
    ## code which is independent of where it  is located in memory. In fact, the
    ## current virtual  machine currently  randomizes where the  code is  put in
    ## order to detect code relying on absolute addresses.


### 3.

    ## TO BE CONTINUED ...


    exit

    ## Make room for initial stack.
    data [0 0 0 0 0 0 0 0]
    data [0 0 0 0 0 0 0 0]

### EXPECTED STACK:
### 2
### 3
