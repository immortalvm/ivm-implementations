    EXPORT main
    exit
main:
    ## 0:<return address> 1:<arg start> 2:<heap start>
    store8! &1
    store8!! 777 &1
    ## 0:<return address> 1:777
    return

### EXPECTED STACK:
### 777
