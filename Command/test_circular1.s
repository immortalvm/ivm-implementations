    IMPORT test_circular2/helper
    EXPORT data

main:
    call! helper
    push! (+ (load1 data) 1)
    exit
data:
    data1 [3]

### EXPECTED STACK:
### 7
