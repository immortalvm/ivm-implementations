# TODO

label1:
  push!  # No-op
  push! 1  # Push one number (64 bits)
  push!! 2 3  # Push two numbers
  push! label1
  jump  # Go to the address on top of the stack
  jump! label1  # Go to label1. Equilvalent to 'push! label1 jump'.

  # Pop two from the stack.
  # Jump to the address popped first if the value popped second is zero.
  jumpZero

  # Pop value and jump to label1 if the value is zero.
  # Equivalent to 'push! label jumpZero'.
  jumpZero! label1

  # Jump to label1 if the value at SP + 3 * 8 is zero.
  jumpZero!! $3 label1

label2:
  push! $pc  # Push the program counter (PC).

  push! &0  # Push the stack pointer (SP).
  push! &1  # Push SP + 8.
  push! &-2  # Push SP - 16.

  push! $n  # Push the (64 bit) value at SP + n * 8.
  add  # Pop two values and push their sum.
  add! 7  # Add 7, i.e. do 'push! 7', then 'add'.
