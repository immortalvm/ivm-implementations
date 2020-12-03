
    push! (+ dat 0)
lab0:
    load1
    sigx1

    push! (+ dat 1)
lab1:
    load2
    sigx2

    push! (+ dat 3)
lab2:
    load4
    sigx4

    exit

dat:
    data1 [ -111 ]
    data2 [ -2222 ]
    data4 [ -333333 ]

### EXPECTED STACK:
###
### -333333
### -2222
### -111
