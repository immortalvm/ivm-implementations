### Regression test for issue #6

    push! -label
blocker1:
    store8!! (+ $0 label) &0

    push! label
blocker2:
    store8!! (+ $0 -label) &0
    exit

label:

### EXPECTED STACK:
### 0
### 0
