# TODO

label1:
  push 1
  push label1
  return  # Go to the address on top of the stack
  jump label1  # Go to label1, i.e. do 'push label1', then 'return'.

label2:
  push $pc  # Push the program counter (PC).

  push &0  # Push the stack pointer (SP).
  push &1  # Push SP + 8.
  push &-2  # Push SP - 16.

  push $n  # Push the (64 bit) value at SP + n * 8.
  add  # Pop two values and push their sum.
  add 7  # Add 7, i.e. do 'push 7', then 'add'.
