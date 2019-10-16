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
let SET_SP = 4y

[<Literal>]
let GET_PC = 5y

[<Literal>]
let GET_SP = 6y

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
let SIGX1 = 12y // From 8 to 64 bits

[<Literal>]
let SIGX2 = 13y // From 16 to 64 bits

[<Literal>]
let SIGX4 = 14y // From 32 to 64 bits


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

// 24-25: Unused

[<Literal>]
let GET_PARAMETER = 26y

// 27-31: Unused

[<Literal>]
let ADD = 32y

[<Literal>]
let MULT = 33y // Multiply

[<Literal>]
let DIV = 34y // Unsigned division. NB: x / 0 = 0 (for shift-right to work)

[<Literal>]
let REM = 35y // Unsigned remainder. NB: x % 0 = 0

[<Literal>]
let LT = 36y // Less than. Unsigned. 0: false. FF...F: true.

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

// 45-47: Unused

[<Literal>]
let NEW_FRAME = 48y

[<Literal>]
let SET_PIXEL = 49y

[<Literal>]
let ADD_SAMPLE = 50y

// 51: Unused

[<Literal>]
let PUT_CHAR = 52y

// 53-55: Unused

[<Literal>]
let READ_FRAME = 56y

[<Literal>]
let READ_PIXEL = 57y


// Used by Machine.Disassembler to find the module.
type ReflectionMarker = interface end
