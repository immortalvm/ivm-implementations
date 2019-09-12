
label3a:
    EXPORT label3b
    store8!! (* $2 2) &2
    return

label3b:
    EXPORT label3a
    store8!! (+ $1 1) &1
    return

### This is not currently tested, but you can verify the effect using 'trace'.
array_pointers:
    space 1
    space 2
    space 3
    space 4
    space 5
    space 6
