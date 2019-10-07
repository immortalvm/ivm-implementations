### Quicksort for 16-bit integers
### Example and integration test

    EXPORT sort                 # Make 'sort' as externally visible

    width = 2
    push!! data_start (+ data_stop -width)
    call! sort
    set_sp! &2

    n = (/u (+ data_stop -data_start) width)
    ## Write result to stack
    set_sp! &-n                 # Make room on stack
    push! n
copy:
    jump_zero!! $0 copy_done
    store8!! (+ $0 -1) &0
    store8!! (load2 (+ data_start (* $0 width))) &(+ $0 1)
    jump! copy
copy_done:
    set_sp! &1
    exit

sort:
    ## $0=return_address, $1=hi, $2=lo (no return value)
    jump_not_zero!! (<u $2 $1) must_sort
    return
must_sort:
    push!!! 0 $2 $1
    call! partition
    set_sp! &2
    ## $0=part, $1=return_address, $2=hi, $3=lo
    jump_not_zero!! (<u $0 $2) sort_upper
    add! -width
    jump! half_sorted
sort_upper:
    push!! (+ $0 width) $2
    call! sort
    set_sp! &2
half_sorted:
    store8! &2
    jump! sort                  # Tail-recursion

partition:
    ## $0=return_address, $1=hi, $2=lo, $3=result_placeholder
    ## hi > lo (or hi >= lo ??)
    ## Find pivot as median of 3.
    push!!! (load2 $2) (load2 (+ $2 (/u (+ $1 -$2) 2))) (load2 $1)
    ## $0=(load2 hi), $1=(load2 middle), $2=(load2 lo), $3=return_address, ...
    jump_zero!! (<u $0 $1) gte01
    jump_zero!! (<u $1 $2) lt01_gte12
    store8!! $1 &2              # $0 < $1 < $2
    jump! pivot_found
lt01_gte12:
    jump_zero!! (<u $0 $2) lt01_gte12_gte02
    jump! pivot_found # $0 < $2 <= $1
lt01_gte12_gte02:
    store8!! $0 &2              # $2 <= $0 < $1
    jump! pivot_found
gte01:
    jump_zero!! (<u $2 $1) gte01_gte21
    store8!! $1 &2              # $2 < $1 <= $0
    jump! pivot_found
gte01_gte21:
    jump_zero!! (<u $0 $2) pivot_found
    store8!! $0 &2              # $1 <= $0 < $2
pivot_found:
    set_sp! &2
    ## $0=pivot, $1=return_address, $2=hi, $3=lo, $4=placeholder
low_not_found:
    jump_zero!! (<u (load2 $3) $0) high_not_found
    ## load2 lo < $0
    store8!! (+ $3 width) &3
    jump! low_not_found
high_not_found:
    jump_zero!! (>u (load2 $2) $0) both_found
    ## load2 hi > $0
    store8!! (+ $2 -width) &2
    jump! high_not_found
both_found:
    jump_zero!! (<u $3 $2) partition_end
    push!!!! (load2 $2) $3 (load2 $3) $2 # Swap
    store2
    store2
    jump! low_not_found
partition_end:                  # lo >= hi
    store8!! $2 &4
    set_sp! &1                  # Pop pivot
    return


data_start:
    data2
    [
        0x6633
        0xf9cd
        0x94d7
        0xb25f
        0xd567
        0x12c4
        0xca23
        0x0c1c
        0x4ca5
        0xdce3
        0x5474
        0xbd97
        0x3172
        0xe78a
        0x434d
        0x6836
        0xd861
        0xcbf0
        0x680b
        0x6908
        0xdc55
        0x7da9
        0x0ab4
        0x8568
        0x01c1
        0xb5c2
        0xe75a
        0xdee6
        0x10ce
        0xa686
        0x7709
        0x2364
        0x5c9e
        0x4ab1
        0x50da
        0x3d00
        0x7944
        0x9787
        0x19aa
        0x6808
        0x05c9
        0x4c25
        0x8180
        0x790e
        0x12f1
        0xfbfd
        0x62e1
        0xdd5f
        0x8578
        0x58df
    ]
data_stop:


### EXPECTED STACK:

### 0x01c1
### 0x05c9
### 0x0ab4
### 0x0c1c
### 0x10ce
### 0x12c4
### 0x12f1
### 0x19aa
### 0x2364
### 0x3172
### 0x3d00
### 0x434d
### 0x4ab1
### 0x4c25
### 0x4ca5
### 0x50da
### 0x5474
### 0x58df
### 0x5c9e
### 0x62e1
### 0x6633
### 0x6808
### 0x680b
### 0x6836
### 0x6908
### 0x7709
### 0x790e
### 0x7944
### 0x7da9
### 0x8180
### 0x8568
### 0x8578
### 0x94d7
### 0x9787
### 0xa686
### 0xb25f
### 0xb5c2
### 0xbd97
### 0xca23
### 0xcbf0
### 0xd567
### 0xd861
### 0xdc55
### 0xdce3
### 0xdd5f
### 0xdee6
### 0xe75a
### 0xe78a
### 0xf9cd
### 0xfbfd
