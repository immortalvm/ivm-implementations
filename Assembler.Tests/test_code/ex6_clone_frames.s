### Write (B/W versions of) each input frame to the output
###
### NB. As of March 2020, read_frame does not work correctly on all platforms.

    set_sp! &-3
per_frame:
    read_frame
    x = 4
    y = 3
    v = 2
    w = 1
    h = 0
    jump_zero!! (+ $w $h) done
    new_frame!!! $w $h 0

    store8!! 0 &y
next_row:
    store8!! 0 &x
next_col:
    ## This would be more elegant if we allow read_pixel in expressions.
    read_pixel* [$x $y]
    store8! &(+ v 1)

    put_char! 48
    set_pixel* [$x $y $v $v $v]
    store8!! (+ $x 1) &x
    jump_not_zero!! (<u $x $w) next_col

    store8!! (+ $y 1) &y
    jump_not_zero!! (<u $y $h) next_row

    set_sp! &2                  # Pop w,h
    jump! per_frame

done:
    set_sp! &5                  # Pop x,y,v,w,h
    exit! 0

### EXPECTED STACK:
### 0
