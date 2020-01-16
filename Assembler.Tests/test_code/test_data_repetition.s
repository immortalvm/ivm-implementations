
    push!! data_start 0

loop:
    jump_not_zero!! (<u $1 data_stop) not_done
    store8! &1
    push! (+ data_stop -data_start)
    exit

not_done:
    add! (load1 $1)
    store8!! (+ $1 1) &1
    jump! loop

data_start:
    data1 [1 2 3] * 3
    data2 [4 5] * 2
    data4 [6 7] * 4
    data8 [8 9] * 1
    data1 [10 11 12] * 0
data_stop:

### EXPECTED STACK:
### 65  # 1*3*3 + 2*2*2 + 4*2*4 + 8*2*1 + 1*3*0
### 105 # (1+2+3)*3 + (4+5)*2 + (6+7)*4 + (8+9)*1 + (10+11+12)*0
