module Machine.Instructions

// The current version of the machine and produced byte code.
// Unlike the constants below, this is not an instruction!
[<Literal>]
let VERSION = 2UL

[<Literal>]
let EXIT = 0y

[<Literal>]
let NOP = 1y

[<Literal>]
let JUMP = 2y

[<Literal>]
let JZ_FWD = 3y // PC += offset

[<Literal>]
let JZ_BACK = 4y // PC -= offset + 1

[<Literal>]
let SET_SP = 5y

[<Literal>]
let GET_PC = 6y

[<Literal>]
let GET_SP = 7y

[<Literal>]
let PUSH0 = 8y

// Push the next byte(s) as a zero-padded 64-bit integer.
[<Literal>]
let PUSH1 = 9y

[<Literal>]
let PUSH2 = 10y

[<Literal>]
let PUSH4 = 11y

[<Literal>]
let PUSH8 = 12y

// 13-15: Unused

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

// 24-31: Unused

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

[<Literal>]
let CHECK = 48y

// 49-247: Unused

[<Literal>]
let READ_FRAME = -1y  // 255

[<Literal>]
let READ_PIXEL = -2y  // 254

[<Literal>]
let NEW_FRAME = -3y   // 253

[<Literal>]
let SET_PIXEL = -4y   // 252

[<Literal>]
let ADD_SAMPLE = -5y  // 251

[<Literal>]
let PUT_CHAR = -6y    // 250

[<Literal>]
let PUT_BYTE = -7y    // 249

[<Literal>]
let READ_CHAR = -8y    // 248

// Used by Machine.Disassembler to find the module.
type IReflectionMarker = interface end
