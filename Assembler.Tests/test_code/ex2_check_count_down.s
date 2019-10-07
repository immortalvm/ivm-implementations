### Push the numbers 10 .. 0 onto the stack.

    first_number_pushed = 10
    push! first_number_pushed
loop:
    push! (+ $0 -1)
    jump_not_zero!! $0 loop
    exit

### EXPECTED STACK:
### 0
### 1
### 2
### 3
### 4
### 5
### 6
### 7
### 8
### 9
### 10
