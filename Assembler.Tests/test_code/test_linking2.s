    IMPORT test_linking3.label3a
    EXPORT label2a
    EXPORT label2b

label2a:
    call! label3a
    call! label3a
    return

label2b:
    data [0]
