    EXPORT main

    ## Make sure the entry point is called.
    exit

main:
    ## 0:<return address> 1:<arg start> 2:<arg length> 3:<heap start / return value>
    store8!! 777 &3
    return

### EXPECTED STACK:
### 777
