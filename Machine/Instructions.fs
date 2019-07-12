﻿module Machine.Instructions

[<Literal>]
let EXIT = 0y

[<Literal>]
let NOP = 1y

[<Literal>]
let JUMP = 2y

[<Literal>]
let JUMP_ZERO = 3y // Relative to next signed byte (immediate arg)

[<Literal>]
let SET_STACK = 4y

[<Literal>]
let GET_PC = 5y

[<Literal>]
let GET_STACK = 6y

[<Literal>]
let PUSH0 = 7y

// Push the next byte(s) as a zero-padded 64-bit integer.
[<Literal>]
let PUSH1 = 8y

[<Literal>]
let PUSH2 = 9y

[<Literal>]
let PUSH4 = 10y

[<Literal>]
let PUSH8 = 11y


// Sign extension
[<Literal>]
let SIGN1 = 12y // From 8 to 64 bits

[<Literal>]
let SIGN2 = 13y // From 16 to 64 bits

[<Literal>]
let SIGN4 = 14y // From 32 to 64 bits


// 15: Unused

[<Literal>]
let LOAD1 = 16y

[<Literal>]
let LOAD2 = 17y

[<Literal>]
let LOAD4 = 18y

[<Literal>]
let LOAD8 = 19y


[<Literal>]
let STORE1 = 20y

[<Literal>]
let STORE2 = 21y

[<Literal>]
let STORE4 = 22y

[<Literal>]
let STORE8 = 23y


[<Literal>]
let ALLOCATE = 24y

[<Literal>]
let DEALLOCATE = 25y

[<Literal>]
let OUTPUT = 26y

[<Literal>]
let INPUT = 27y


// 28-31: Unused

[<Literal>]
let ADD = 32y

[<Literal>]
let MULTIPLY = 33y

[<Literal>]
let DIVIDE = 34y // Unsigned. NB: x / 0 = 0 (for shift-right to work)

[<Literal>]
let REMAINDER = 35y // Unsigned. NB: x % 0 = 0

[<Literal>]
let LESS_THAN = 36y // Unsigned. 0: false. FF...F: true.

// 36-39: Unused

[<Literal>]
let AND = 40y

[<Literal>]
let OR = 41y

[<Literal>]
let NOT = 42y

[<Literal>]
let XOR = 43y

[<Literal>]
let POW2 = 44y

// Used by Machine.Disassembler to find the module.
// type internal ReflectionMarker = interface end
type ReflectionMarker = interface end