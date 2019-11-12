    IMPORT test_linking3/label3a
    IMPORT test_linking3/label3c
    EXPORT label2a
    EXPORT label2b
    EXPORT label2c

label2a:
    call! label3a
    call! label3a
    return

label2b:
    data1 [0]

label2c:
    data8 [label3c]
