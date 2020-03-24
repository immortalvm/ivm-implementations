### Signed shift right

start:
    m = 3
    n = (+ values_stop -values_start)
    set_sp! &-(+ 2 (* n m))
    i = 0
    k = 1
    res = 2                     # Start of result stack
    store8!! m &k
next_k:
    jump_zero!! $k done
    store8!! (+ $k -1) &k
    store8!! n &i
next_i:
    jump_zero!! $i next_k
    store8!! (+ $i -1) &i
    x = (sigx1 (load1 (+ values_start $i)))
    j = (+ res $i (* n $k))
    store8!! (>>s x $k) &j
    jump! next_i
done:
    set_sp! &2
    exit

values_start:
    data1 [-4 -3 -2 -1 0 1 2 3 4 5]
values_stop:

### EXPECTED STACK:
### -4 -3 -2 -1 0 1 2 3 4 5
### -2 -2 -1 -1 0 0 1 1 2 2
### -1 -1 -1 -1 0 0 0 0 1 1
