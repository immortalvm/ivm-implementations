### Check that splitting code across multiple files works as expected.

    IMPORT test_linking2.label2a
    IMPORT test_linking2.label2b
    IMPORT test_linking2.label2c
    IMPORT test_linking3.label3b
    IMPORT test_linking3.array_pointers

    push! 3                     # 3
    call! label2a               # 12
    store1! label2b
    push! (+ (load1 label2b) (load1 label2b)) # 24
    call! label3b                             # 25
    call! (load8 label2c)                     # 30

    a1 = (load8 array_pointers)
    a8 = (load8 (+ array_pointers (* 7 8)))
    store1! a1
    store8!! -1 a8
    push! (+ (load1 a1) (load8 a8)) # 30 - 1
    exit

### EXPECTED STACK:
### 29
