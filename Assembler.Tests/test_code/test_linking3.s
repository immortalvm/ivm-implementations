
label3a:
    EXPORT label3b
    store8!! (* $2 2) &2
    return

label3b:
    EXPORT label3a
    store8!! (+ $1 1) &1
    return
