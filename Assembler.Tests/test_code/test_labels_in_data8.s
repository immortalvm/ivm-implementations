    push! (load1 (load8 (load8 x)))
    exit

x:  data8 [ (+ y 1) ]
y:  data1 [ 123 ]
    data8 [ y ]

### EXPECTED STACK:
### 123
