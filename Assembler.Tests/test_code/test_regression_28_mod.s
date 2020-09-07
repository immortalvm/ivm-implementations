    n = 6
    m = (+ (* n 2) 1)

    x = (load1 xlabel)
    y = (load1 ylabel)

yloop:
    jump_not_zero!! y xloop
    exit
xloop:
    jump_not_zero!! x core
    store1!! m xlabel
    store1!! (+ y -1) ylabel
    jump! yloop
core:
    push! (%s (+ x -n -1) (+ y -n -1))
    store1!! (+ x -1) xlabel
    jump! xloop

xlabel:
    data1 [m]
ylabel:
    data1 [m]

### Pushed onto the stack (in reverse order):
###
###  -6%-6 ... 6%-6
###   ...      ...
###  -6%6  ... 6%6

### EXPECTED STACK:
###
###  0 -5 -4 -3 -2 -1  0  1  2  3  4  5  0
### -1  0 -4 -3 -2 -1  0  1  2  3  4  0  1
### -2 -1  0 -3 -2 -1  0  1  2  3  0  1  2
###  0 -2 -1  0 -2 -1  0  1  2  0  1  2  0
###  0 -1  0 -1  0 -1  0  1  0  1  0  1  0
###  0  0  0  0  0  0  0  0  0  0  0  0  0
###  0  0  0  0  0  0  0  0  0  0  0  0  0
###  0  0  0  0  0  0  0  0  0  0  0  0  0
###  0 -1  0 -1  0 -1  0  1  0  1  0  1  0
###  0 -2 -1  0 -2 -1  0  1  2  0  1  2  0
### -2 -1  0 -3 -2 -1  0  1  2  3  0  1  2
### -1  0 -4 -3 -2 -1  0  1  2  3  4  0  1
###  0 -5 -4 -3 -2 -1  0  1  2  3  4  5  0
