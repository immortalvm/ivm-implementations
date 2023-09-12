### Echo (read and then write) 128 bytes.
### Fails if not enough input bytes are provided.
### Do not add to the F# test project.

    bytes = 128
    push! bytes
loop:
    jump_zero!! $0 done
    read_byte
    put_byte
    sub! 1
    jump! loop
done:
    exit

### EXPECTED STACK:
### 0
