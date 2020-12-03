### See also test_sigx.s

    push! (sigx1 (load8 (+ data 0)))
    push! (sigx2 (load8 (+ data 8)))
    push! (sigx4 (load8 (+ data 16)))
    exit
data:
    data8 [0x100 0x10000 0x100000000]

### EXPECTED STACK:
### 0 0 0
