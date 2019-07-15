﻿### Quicksort for 16-bit integers
### Example and integration test

    stack_size = 16384
    allocate! stack_size
    add! stack_size
    set_sp

    width = 2
    push!!! after_sort data_start (+ data_stop -width)
    jump! sort
after_sort:
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
    ## $0=hi, $1=lo, $2=return_address (no return value)
    jump_not_zero!! (<u $1 $0) must_sort
    set_sp! &2
    jump
must_sort:
    push!!!! 0 sort_partitioned $1 $0
    jump! partition
sort_partitioned:
    ## $0=part, $1=hi, $2=lo, $3=return_address
    jump_not_zero!! (<u $0 $1) sort_upper
    add! -width
    jump! half_sorted
sort_upper:
    push!!! half_sorted (+ $0 width) $1
    jump! sort
half_sorted:
    store8! &1
    ## $0=part, $1=lo, $3=return_address
    jump! sort                  # Tail-recursion!

partition:
    ## $0=hi, $1=lo, $2=return_address, $3=result_placeholder
    ## stop > start
    ## Find pivot as median of 3.
    push!!! (load2 $1) (load2 (+ $1 (/u (+ $0 -$1) 2))) (load2 $0)
    ## $0=(load2 hi), $1=(load2 middle), $2=(load2 lo), $3=hi, ...
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

    ## $0=pivot, $1=hi, $2=lo, $3=return_address, $4=placeholder
low_not_found:
    jump_zero!! (<u (load2 $2) $0) high_not_found
    ## load2 lo < $0
    store8!! (+ $2 width) &2
    jump! low_not_found
high_not_found:
    jump_zero!! (>u (load2 $1) $0) both_found
    ## load2 hi > $0
    store8!! (+ $1 -width) &1
    jump! high_not_found
both_found:
    jump_zero!! (<u $2 $1) partition_end

    push!!!! (load2 $1) $2 (load2 $2) $1
    store2
    store2
    jump! low_not_found
partition_end:                  # lo >= hi
    store8!! $1 &4
    set_sp! &3
    jump


data_start:
    data
    [
        0x33 0x66
        0xcd 0xf9
        0xd7 0x94
        0x5f 0xb2
        0x67 0xd5
        0xc4 0x12
        0x23 0xca
        0x1c 0x0c
        0xa5 0x4c
        0xe3 0xdc
        0x74 0x54
        0x97 0xbd
        0x72 0x31
        0x8a 0xe7
        0x4d 0x43
        0x36 0x68
        0x61 0xd8
        0xf0 0xcb
        0x0b 0x68
        0x08 0x69
        0x55 0xdc
        0xa9 0x7d
        0xb4 0x0a
        0x68 0x85
        0xc1 0x01
        0xc2 0xb5
        0x5a 0xe7
        0xe6 0xde
        0xce 0x10
        0x86 0xa6
        0x09 0x77
        0x64 0x23
        0x9e 0x5c
        0xb1 0x4a
        0xda 0x50
        0x00 0x3d
        0x44 0x79
        0x87 0x97
        0xaa 0x19
        0x08 0x68
        0xc9 0x05
        0x25 0x4c
        0x80 0x81
        0x0e 0x79
        0xf1 0x12
        0xfd 0xfb
        0xe1 0x62
        0x5f 0xdd
        0x78 0x85
        0xdf 0x58
    ]
data_stop:

    ## Make room for initial stack.
    data [0 0 0 0 0 0 0 0]
    data [0 0 0 0 0 0 0 0]


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