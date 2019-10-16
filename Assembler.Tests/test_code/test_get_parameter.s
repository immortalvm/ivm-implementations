### Check that get_parameter returns 0 when no parameters are specified.

    get_parameter! 0
    get_parameter! (<< 1 8)
    get_parameter! (<< 1 16)
    get_parameter! (<< 1 32)
    exit

### EXPECTED STACK:
###
### 0
### 0
### 0
### 0
