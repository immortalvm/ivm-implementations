### Loop read_char+put_char until receiving EOT (^D).
### NB. Do not add to the F# test project. Running the tests would get stuck.

    eot = 4
loop:
    read_char
    jump_not_zero!! (= $0 eot) done
    put_char
    jump! loop
done:
    set_sp! &1
    exit! 0

### EXPECTED STACK:
### 0
