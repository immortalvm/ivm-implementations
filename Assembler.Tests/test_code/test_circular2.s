    IMPORT test_circular1/data
    EXPORT helper

helper:
    store1!! (* (load1 data) 2) data
    return
