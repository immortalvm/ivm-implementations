### Quicksort of 32-bit signed integers from argument file

    push! 1                     # Wrong entry point
    exit

    EXPORT main
main:
    ## 0: return address
    ## 1: start of argument array
    ## 2: length of argument array
    ## 3: start of free heap space

    width = 4
    jump_zero!! (%u $2 width) length_ok
    push! 2                     # length not divisible by width
    exit
length_ok:

    push!! $1 (+ $1 $2 -width)
    call! sort
    set_sp! &2

    ## Ensure result sorted
    push! (+ $1 $2)
check_more:
    sub! width
    jump_zero!! (>u $0 $2) sorting_correct
    jump_not_zero!! (>=s (sigx4 (load4 $0)) (sigx4 (load4 (+ $0 -width)))) check_more
    push! 3                     # result not sorted
    exit
sorting_correct:
    set_sp! &1

    ## Print the 10 largest integers.
    numbers_to_print = 10
    push! numbers_to_print
    jump_not_zero!! (>=u (/u $3 width) numbers_to_print) print
    add! (+ (/u $3 width) -10)
print:
    jump_zero!! $0 printing_done

    push! (sigx4 (load4 (+ $2 $3 (* -$0 width))))
    call! write_signed_long
    set_sp! &1

    put_char! 10                # \n
    add! -1                     # decrement counter
    jump! print
printing_done:
    store8! &4                  # use the top 0 as exit code
    return

write_signed_long:
    ## 0: return address, 1: argument
    jump_not_zero!! (>=s $1 0) not_negative
    put_char! 45                # -
    store8!! -$1 &1
not_negative:
    push! (/u $1 10)
    jump_zero!! $0 next
    call! not_negative          # Recursive call
next:
    set_sp! &1
    zero = 48
    put_char! (+ (%u $1 10) zero)
    return

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
    push!!! (sigx4 (load4 $2)) (sigx4 (load4 (+ $2 (/u (+ $1 -$2) 2)))) (sigx4 (load4 $1))
    ## $0=*hi, $1=*middle, $2=*lo, $3=return_address, ...
    jump_zero!! (<s $0 $1) gte01
    jump_zero!! (<s $1 $2) lt01_gte12
    store8!! $1 &2              # $0 < $1 < $2
    jump! pivot_found
lt01_gte12:
    jump_zero!! (<s $0 $2) lt01_gte12_gte02
    jump! pivot_found # $0 < $2 <= $1
lt01_gte12_gte02:
    store8!! $0 &2              # $2 <= $0 < $1
    jump! pivot_found
gte01:
    jump_zero!! (<s $2 $1) gte01_gte21
    store8!! $1 &2              # $2 < $1 <= $0
    jump! pivot_found
gte01_gte21:
    jump_zero!! (<s $0 $2) pivot_found
    store8!! $0 &2              # $1 <= $0 < $2
pivot_found:
    set_sp! &2
    ## $0=pivot, $1=return_address, $2=hi, $3=lo, $4=placeholder
while_low_not_found:
    jump_zero!! (<s (sigx4 (load4 $3)) $0) while_high_not_found
    ## *lo < $0
    store8!! (+ $3 width) &3
    jump! while_low_not_found
while_high_not_found:
    jump_zero!! (>s (sigx4 (load4 $2)) $0) both_found
    ## *hi > $0
    store8!! (+ $2 -width) &2
    jump! while_high_not_found
both_found:
    jump_zero!! (<u $3 $2) partition_end
    push!!!! (load4 $2) $3 (load4 $3) $2 # Swap
    store4
    store4
    store8!! (+ $3 width) &3
    store8!! (+ $2 -width) &2
    jump! while_low_not_found
partition_end:                  # lo >= hi
    store8!! $2 &4
    set_sp! &1                  # Pop pivot
    return

### EXPECTED STACK:
### 0
