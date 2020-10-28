    push! (sigx1 (load1 x))
    push! (sigx1 (load1 (+ x 1)))

    push! (sigx2 (load2 y))
    push! (sigx2 (load2 (+ y 2)))

    push! (sigx4 (load4 z))
    push! (sigx4 (load4 (+ z 4)))

    exit

x:
    data1 [127 128]
y:
    data2 [32767 32768]
z:
    data4 [0x7fffffff 0x80000000]

### EXPECTED STACK:
###
### -0x80000000
### 0x7fffffff
### -32768
### 32767
### -128
### 127
