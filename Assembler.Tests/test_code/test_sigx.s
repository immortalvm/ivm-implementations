### See also test_regression_33_sigx.s

    push! (sigx1 (load1 (+ x 0)))
    push! (sigx1 (load1 (+ x 1)))
    push! (sigx1 (load1 (+ x 2)))
    push! (sigx1 (load1 (+ x 3)))

    push! (sigx2 (load2 (+ y 0)))
    push! (sigx2 (load2 (+ y 2)))
    push! (sigx2 (load2 (+ y 4)))
    push! (sigx2 (load2 (+ y 6)))

    push! (sigx4 (load4 (+ z 0)))
    push! (sigx4 (load4 (+ z 4)))
    push! (sigx4 (load4 (+ z 8)))
    push! (sigx4 (load4 (+ z 12)))

    exit

x:
    data1 [127 128 -129 -128]
y:
    data2 [32767 32768 -32769 -32768]
z:
    data4 [0x7fffffff 0x80000000 -0x80000001 -0x80000000]

### EXPECTED STACK:
###
### -0x80000000 0x7fffffff
### -0x80000000 0x7fffffff
### -32768 32767
### -32768 32767
### -128 127
### -128 127
