
label3a:
    EXPORT label3a
    store8!! (* $2 2) &2
    return

label3b:
    EXPORT label3b
    store8!! (+ $1 1) &1
    return

label3c:
    EXPORT label3c
    store8!! (+ $1 5) &1
    return

array_pointers:
    EXPORT array_pointers
    space 1
    space 2
    space 3
    space 4
    space 5
    space 6
    space 7
    space 8
