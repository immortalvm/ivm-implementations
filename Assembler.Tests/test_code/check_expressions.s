### Expression checks

    stack_size = 16384
    alloc! stack_size
    add! stack_size
    set_sp

    ## Macros (not optimized away)
    x0 = (load1 stuff)
    x10 = (load1 (+ stuff 1))
    x20 = (load1 (+ stuff 2))
    x30 = (load1 (+ stuff 3))
    x40 = (load1 (+ stuff 4))
    x50 = (load1 (+ stuff 5))
    x60 = (load1 (+ stuff 6))
    x70 = (load1 (+ stuff 7))
    x80 = (load1 (+ stuff 8))
    x90 = (load1 (+ stuff 9))
    x100 = (load1 (+ stuff 10))
    x110 = (load1 (+ stuff 11))
    x120 = (load1 (+ stuff 12))

    y0 = (sign1 (load1 stuff))
    y10 = (sign1 (load1 (+ more_stuff 1)))
    y20 = (sign1 (load1 (+ more_stuff 2)))
    y30 = (sign1 (load1 (+ more_stuff 3)))
    y40 = (sign1 (load1 (+ more_stuff 4)))
    y50 = (sign1 (load1 (+ more_stuff 5)))
    y60 = (sign1 (load1 (+ more_stuff 6)))
    y70 = (sign1 (load1 (+ more_stuff 7)))
    y80 = (sign1 (load1 (+ more_stuff 8)))
    y90 = (sign1 (load1 (+ more_stuff 9)))
    y100 = (sign1 (load1 (+ more_stuff 10)))
    y110 = (sign1 (load1 (+ more_stuff 11)))
    y120 = (sign1 (load1 (+ more_stuff 12)))

    push! (+ more_stuff -stuff) # 13

    push! (+ 10 x0 20)          # 30
    push! (+ 0 x10 20)          # 30
    push! (+ x0 x10 x20)        # 30
    push! (* x30 x40 x50)       # 60000
    push! (& x20 x60 x120)      # 16
    push! (| x30 x90)           # 94
    push! (^ x40 50 x60)        # 38

    push! (+ (+ x70 y80) (+ x100 y90)) # 0
    push! (* (+ x20 1) (+ x20 -1)) # 399

    push! (<u 0 10)             # -1 (true)
    push! (<u x10 x20)          # -1
    push! (<u x30 30)           # 0 (false)
    push! (<u y10 0)            # 0

    push! (>u 0 10)             # 0
    push! (>u x10 x20)          # 0
    push! (>u x30 30)           # 0
    push! (>u y10 0)            # -1

    push! (<s y10 0)            # -1
    push! (>s y10 0)            # 0

    X1 = (<< 1 63)              # Which is equal to -X1
    push! (<s X1 y0)            # -1
    push! (>s y0 X1)            # -1
    push! (/u X1 X1)            # 1
    push! (/s X1 X1)            # 1
    push! (>>u X1 63)           # 1
    push! (>>s X1 63)           # -1
    push! (/su X1 X1)           # -1 (divide signed with unsigned)
    X2 = (load8 X_data)
    push! (= X1 X2)             # -1
    push! (= -X2 X1)            # -1
    push! (<s X2 y0)            # -1
    push! (>s y0 X2)            # -1
    push! (/u X2 X2)            # 1
    push! (/s X2 X2)            # 1
    push! (>>u X2 63)           # 1
    push! (>>s X2 63)           # -1
    push! (/su X2 X2)           # -1 (divide signed with unsigned)

    ## x/0 = 0  and x%0 = 0 !
    push! (/u 1 0)              # 0
    push! (/s 2 0)              # 0
    push! (/u x10 x0)           # 0
    push! (/s y10 y0)           # 0
    push! (%u 1 0)              # 0
    push! (%s 2 0)              # 0
    push! (%u x10 x0)           # 0
    push! (%s y10 y0)           # 0

    ## Flip all bits: ~
    push! (= ~0 -1)             # -1
    push! (= -1 ~x0)            # -1
    push! (= ~x0 -1)            # -1

    exit

    data [ 7 ]                  # Something other than 0
stuff:
    data [ 0 10 20 30 40 50 60 70 80 90 100 110 120 ]
more_stuff:
    data [ 0 -10 -20 -30 -40 -50 -60 -70 -80 -90 -100 -110 -120 ]
X_data:
    data [ 0 0 0 0 0 0 0 0x80 ]

    ## Make room for initial stack.
    data [0 0 0 0 0 0 0 0]
    data [0 0 0 0 0 0 0 0]

### EXPECTED STACK:
###

### -1
### -1
### -1 # Flip bits

### 0
### 0
### 0
### 0
### 0
### 0
### 0
### 0 # Division by zero

### -1
### -1
### 1
### 1
### 1
### -1
### -1
### -1
### -1   # X2
### -1
### -1
### 1
### 1
### 1
### -1
### -1    # X1

### 0
### -1

### -1
### 0
### 0
### 0

### 0
### 0
### -1
### -1

### 399
### 0
### 38
### 94
### 16
### 60000
### 30
### 30
### 30
### 13
