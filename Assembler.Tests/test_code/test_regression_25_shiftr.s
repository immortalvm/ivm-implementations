### Signed shift right

start:
    n = (+ values_stop -values_start)
    set_sp! &-n
    push! n
loop:
    jump_zero!! $0 done
    sub! 1
    shift_rs!! (sigx1 (load1 (+ values_start $0))) 1
    store8! &(+ $1 2)
    jump! loop
done:
    set_sp! &1
    exit

values_start:
    data1 [-4 -3 -2 -1 0 1 2 3 4 5]
values_stop:

### EXPECTED STACK:
### -2
### -2
### -1
### -1
###  0
###  0
###  1
###  1
###  2
###  2
