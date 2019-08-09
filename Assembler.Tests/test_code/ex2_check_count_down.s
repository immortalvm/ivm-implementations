### Push the numbers 10 .. 0 onto the stack.

    stack_size = 200
    allocate! stack_size
    add! stack_size
    set_sp

    first_number_pushed = 10
    push! first_number_pushed
loop:
    load8! &0
    add! -1
    load8! &0
    jump_not_zero! loop
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
