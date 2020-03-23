### Write the contents of the arg file to the output directory

prog:
    ## If no entry point was specified
    arg = (load8 (+ prog -16))
    push* [0 (load8 arg) (+ arg 8) 0]

main:
    ## Entry point
    set_sp! &1
    store8!! (+ $0 $1) &1
per_byte:
    jump_not_zero!! (= $0 $1) done
    put_byte! (load1 $0)
    add! 1
    jump! per_byte
done:
    set_sp! &3
    exit! 0

### EXPECTED STACK:
### 0
