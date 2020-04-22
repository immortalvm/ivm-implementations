### Write (B/W versions of) each input frame to the output

    set_sp! &-3
    push! 0
next_frame:                      # Possible entry point
    read_frame! $0               # Read frame number i
    x = 5
    y = 4
    v = 3
    i = 2
    w = 1
    h = 0
    jump_zero!! (+ $w $h) done
    new_frame!!! $w $h 0

    store8!! 0 &y
next_row:
    store8!! 0 &x
next_col:
    ## This would be more elegant if we allowed read_pixel in expressions.
    read_pixel* [$x $y]
    store8! &(+ v 1)

    set_pixel* [$x $y $v $v $v]
    store8!! (+ $x 1) &x
    jump_not_zero!! (<u $x $w) next_col

    store8!! (+ $y 1) &y
    jump_not_zero!! (<u $y $h) next_row

    set_sp! &2                  # Pop w,h
    add! 1                      # Increment i
    jump! next_frame

done:
    set_sp! &6                  # Pop x,y,v,i,w,h
    exit! 0

### EXPECTED STACK:
### 0
