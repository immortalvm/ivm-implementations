### iVM assembly language introduction.
###
### Part 1 - Statements
###
### This file explains the iVM assembly  language. It is itself a valid assembly
### file, but the code does not make  much sense. Notice that '#' indicates that
### the rest of the line is a comment.
###
### Other than comments,  an iVM assembly file consist of  a list of statements.
### Whitespace is not significant, but it recommended to put each statement on a
### separate line.
###
### The assembly language  is case-sensitive, with all  the instructions written
### in lower case (even though we have  used upper case in the headings for them
### to stand out).


### 1. SPECIAL STATEMENTS

    ## There are three special statements: labels, definitions and data

    ## A  label statement  indicates a  place in  the code  (memory address)  at
    ## runtime. By convention, all other statements should be indented.
my_label:

    ## Definitions define abbreviations, usually constants.
    prime_number = 982451653

    ## Labels and definitions use the  same namespace, which is independent from
	## the names  of instructions. Thus, you  are free to define  a label called
	## 'add'  or 'exit'.  Names of  labels and  definitions consist  of letters,
	## digits and underscore (_), and they cannot start with a digit.

    ## A data statement specifies a list of bytes that should be included in the
    ## binary as is.  It is a whitespace-separated list of  numbers between -255
    ## and 255  with "wrapping". For example,  -1 and 255 denote  the same byte.
    ## All numbers can be specified in decimal, octal or hexadecimal notation.
    data [ 0 1 -2 0o200 -0xff ]

    ## Starting a program with a data block is currently a bad idea, as
    ## currently our VM does always execute programs from the top.
    ## (Incidentally, 0 means that the VM should terminate immediately. Thus,
    ## the meaningless statements below do not cause the machine to crash.)

    ## The  remaining  statements  correspond to  actual  machine  instructions.
    ## However, there is not a  one-to-one correspondence. The assembly language
    ## also contain  several "pseudo-instructions" that translate  into multiple
    ## native instructions.  Moreover, the assembler will  handle some technical
    ## issues such as choosing between long and short conditional jumps.


### 2. PUSH

    ## The push statement pushes 64-bit numbers onto the stack.
    push! 13    # Push the number 13 onto the stack.

    ## As above, these numbers can be in either decimal, octal or hexadecimal
    ## notation; and they can be both positive and negative, with wrapping.
    push! -1    # Push 0xffffffffffffffff onto the stack.

    ## A single push statement can push multiple numbers, indicated by the
    ## number of exclamation marks (!).
    push!! 0 1  # Push 0 onto the stack, then 1.
    push        # Do nothing

    ## The push statement is not only used to push constants.
    push! my_label      # Push the address of my_label.
    push! prime_number  # Push the result of expanding its definition.

    n = 7      # An arbitrary number
    push! &n   # Push the address PC + n * 8 (of the n'th element on the stack).
    push! $n   # Push the (64-bit) value at PC + n * 8.

    ## It is also possible to push the value of complex (Lisp style)
    ## expressions.
    push! (+ my_label -$0) # Push the label address minus the top stack element.

    ## This will be explained in detail later, but observe that
    #
    ##    'push!! 17 $0'    is equivalent to    'push! 17 push! $1'
    #
    ## since within a statement $x and &x will be relative to value of the stack
    ## pointer at the start of the statement.


### 3. JUMP, JUMP_ZERO, JUMP_NOT_ZERO

    ## The jump statement changes the program counter to an address popped from
    ## the stack.
    jump

    ## 'jump! x' is (syntactic) sugar for 'push! x jump'.
    jump! my_label  # Jump to my_label

    ## The jump_zero statement pops an address first, then a value, and jumps to
    ## the address if the value is zero.
    jump_zero

    ## 'jump_zero! x' is sugar for 'push! x jump_zero'.
    jump_zero! my_label  # Jump to my_label if value popped is zero.

    ## Similarly, 'jump_zero!! x y' is sugar for 'push!! x y jump_zero'.
    ## The following statement jumps to my_label if the fourth element on the
    ## stack (counting from 0) is equal to prime_number.
    jump_zero!! (+ prime_number -$4) my_label

    ## The jump_not_zero statement is similar.
    jump_not_zero


### 4. CALL, RETURN

    ## When calling a subroutine we want to continue execution at the next statement.
    call! my_label           # Sugar for 'push! <fresh> jump! my_label <fresh>:'
    return                   # Alias for 'jump'


### 5. LOAD, SIGX

    ## The four load statements all pop one address from the stack and push a 64
    ## bit value,  the contents of  the memory  at that location  (and onwards).
    ## When less than 8 bytes are  fetched, the value is considered unsigned and
    ## is 0-padded before  pushing. We use little-endian  encoding of multi-byte
    ## values.
    load1  # Pop address and push the unsigned byte at that memory location.
    load2  # ... unsigned 16-bit word ...
    load4  # ... unsigned 32-bit word ...
    load8  # Pop address and push the 64-bit word at that memory location.

    ## 'load1! x' is sugar for 'push! x load1', similarly for load2/4/8.
    load4! my_label  # Load the unsigned 32-bit word at my_label.

    ## If you want the signed value at a memory location instead, follow the load
    ## statement with a corresponding sigx statement (sigx1, sigx2 or sigx4).
    #
    ## sigx1 performs "sign extension" from 8 to 64 bits. More precisely, it
    ## (1) pops a 64-bit value from the stack,
    ## (2) sets bits 8..63 (counting from 0) to the same state as bit 7,
    ## (3) pushes the result back onto the stack.
    #
    ## sigx2 and sigx4 are similar.
    sigx4  # Sign extension from 32 to 64 bits.

    ## For completeness, we also let 'sigxN! x' be sugar for 'push! x sigxN'.
    ## However, it is perhaps not very useful.
    sigx1! 0xff  # Push -1.


### 6. STORE

    ## The four store operations pop an address A, then a value V from the stack
    ## and writes the least significant bytes of V to A.
    store1                      # Write the least significant byte of V to A.
    store2                      # Write 16 bits of V to A.
    store4                      # Write 32 bits of V to A.
    store8                      # Write all ov V to A.

    ## 'storeN! x' is sugar for 'push! x storeN'
    store4! my_label  # Pop value and write lower 32 bits to memory at my_label.

    ## 'storeN!! x y' is sugar for 'push!! x y storeN'
    store8!! prime_number my_label  # Write prime_number to memory at my_label.


### 7. ARITHMETIC OPERATIONS

    xx = 99
    yy = -13

    add           # Pop two values and push their sum (with wrapping).

    ## All arithmetic statements have corresponding "sugared" variants.
    add!  xx      # Sugar for 'push! xx add'.
    add!! xx yy   # Sugar for 'push!! xx yy add'.

    sub           # Pop x, then y, and push y - x.  Notice the order!
    sub! xx       # Subtract xx from the value on top of the stack.
    sub!! xx yy   # Push xx - yy.

    mult          # Pop two values and push their product.
    neg           # Pop value and push its additive inverse (two's complement).

    div_u         # Pop x, then y, and push y / x using unsigned division.
    div_s         # Similar, but using signed division.

    rem_u         # Pop two values and push the remainder of unsigned division.
    rem_s         # Similar, but using signed division.


### 8. BITWISE OPERATIONS

    and           # Pop two (64-bit) values and push their binary "and".

    ## All bitwise statements have corresponding "sugared" variants.
    and! 0x7f                 # Sugar for 'push! 0x7f and'.
    and!! 0xfff prime_number  # Sugar for 'push!! 0xfff prime_number'.

    or          # Pop two values and push their binary "or".
    xor         # Pop two values and push their binary "xor".
    not         # Pop one value and push its binary negation (one's complement).

    ## Notice the distinction between 'not' (flipping all bits) and 'neg' (which
    ## leaves 0 unchanged). These names are standard in assembly language.

    ## The pow2 statement pops x (unsigned) and pushes 2 to the power of x.
    pow2
    shift_l       # Sugar for 'pow2 mult'
    shift_ru      # Sugar for 'pow2 div_u'
    shift_rs      # Almost sugar for 'pow2 div_s', except handle the special
                  # case of shifting the least negative value 63 bits to the
                  # right. (It should be -1.)


### 9. COMPARISON

    ## For convenience, the comparison predicates below all return 0 for false
    ## and -1 (all bits set) for true. They also come in sugared variants.

    eq           # Pop two values. Push -1 if they are equal, otherwise push 0.
    eq! 7        # Pop value and compare it to 7.
    eq!! xx yy   # Sugar for 'push!! xx yy eq'.

    ## The remaining comparison operators come in signed and unsigned variants.
    lt_u         # Pop x, then y. Push -1 if y < x (unsigned), otherwise push 0.
    lt_s         # Pop x, then y. Push -1 if y < x (signed), otherwise push 0.
    lte_u        # less than or equal (unsigned)
    lte_s        # less than or equal (signed)

    gt_u         # y > x (unsigned)
    gt_s         # y > x (signed)
    gte_u        # greater than or equal (unsigned)
    gte_s        # greater than or equal (signed)


### 10. ALLOC, DEALLOC

    ## The allocate statement pops x (unsigned) from the stack, allocates a
    ## range of x consecutive bytes of unused memory, and pushes the address of
    ## the first byte back onto the stack. Undefined if x is 0.
    allocate

    allocate! prime_number  # Sugar for 'push! prime_number allocate'.

    ## The deallocate statement pops an address A from the stack. If it is the
    ## start address of a previously allocated range of memory (which has not in
    ## the mean time been deallocated), then the whole range is now deallocated.
    deallocate

    deallocate! $8      # Sugar for 'push! $8 deallocate'.


### 11. IO STATEMENTS

    ## To be determined later.


### 12. SET_SP, EXIT

    xx = 9      # Override previous definition of 9.

    set_sp      # Pop A from the stack, and set the stack pointer to A.
    set_sp! xx  # Sugar for 'push! xx set_sp'.

    exit        # Terminate the machine.

### EXPECTED STACK:
###
