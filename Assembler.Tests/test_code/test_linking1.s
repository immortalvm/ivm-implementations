### Check that splitting code across multiple files works as expected.

    IMPORT test_linking2.label2a
    IMPORT test_linking2.label2b
    IMPORT test_linking3.label3b

    stack_size = 1000
    allocate! stack_size
    add! stack_size
    set_sp

    push! 3                     # 3
    call! label2a               # 12
    store1! label2b
    push! (+ (load1 label2b) (load1 label2b)) # 24
    call! label3b                             # 25
    exit

### EXPECTED STACK:
### 25
