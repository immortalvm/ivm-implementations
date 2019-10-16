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

    ## There are six special  statements: imports, labels, exports, definitions,
    ## data and space statements.

    ## Import statements can only occur at the  top of the file. It means that a
    ## label  in file  can  be referenced  below (provided  that  the label  was
    ## exported in the other file). Circular dependencies are not allowed.
    IMPORT intro2_basics.x

    ## A  label statement  indicates a  place in  the code  (memory address)  at
    ## runtime. By convention, all other statements should be indented.
my_label:

    ## For a label to be visible from other files, it must be "exported".
    EXPORT my_label

    ## Definitions define abbreviations, usually constants.
    prime_number = 982451653

    ## A label can be exported under a different name as follows:
    external_name = my_label
    EXPORT external_name

    ## Labels and definitions use the  same namespace, which is independent from
	## the names  of instructions. Thus, you  are free to define  a label called
	## 'add'  or 'exit'.  Names of  labels and  definitions consist  of letters,
	## digits and underscore (_), and they cannot start with a digit.

    ## Data statements specify bytes that should be included in the binary as is
    ## (more or less). It includes a whitespace-separated list of constant
    ## expressions, usually numbers. All numbers can be specified in decimal,
    ## octal or hexadecimal notation.

    ## data1 includes a list of bytes in the binary. Only the 8 least
    ## significant bits are used of each number. Thus, the last number can be
    ## replaced by 1.
    data1 [ 0 1 -2 0o200 -0x99ff ]

    ## Similarly, data2 includes a list of (little-endian) 16-bit words, using
    ## the 16 least significant bits.
    data2 [ 0x1000 0x2000 0x3000 ]

    ## data4 and data8 include lists of 32-bit and 64-bit words, respectively.
    data4 [ 0x40000000 ]
    data8 [-0x0123456789abcdef]

    ## It should be noted that starting a program with a data block is generally
    ## a bad  idea, as our VM  executes programs from the  top. (Incidentally, 0
    ## means that  the VM  should terminate  immediately. Thus,  the meaningless
    ## statements below do not cause the machine to crash.)

    ## A space statement inserts a 64-bit pointer to memory allocated by the
    ## program at startup. The argument specifies the number of bytes that will
    ## be allocated.
my_1000_byte_array:
    space 1000

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

    ## The following notation is more convenient when pushing many values onto
    ## the stack:
    push* [1 2 3 4 5 6 7]

    ## This is syntactic sugar for:
    push!!!!!!! 1 2 3 4 5 6 7


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


### 10. IO STATEMENTS

    ## To be determined later.


### 11. SET_SP, EXIT

    xx = 9      # Override previous definition of 9.

    set_sp      # Pop A from the stack, and set the stack pointer to A.
    set_sp! xx  # Sugar for 'push! xx set_sp'.

    exit        # Terminate the machine.

### EXPECTED STACK:
###
