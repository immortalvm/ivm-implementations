
program:
    push! &0
    push! (+ &0 lab -(+ lab -8))
    sub

### Payload
lab:
    exit

### EXPECTED STACK:
###
### 0
