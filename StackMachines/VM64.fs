module VM64

open System
open VmBase64

[<Literal>]
let EXIT = 0

[<Literal>]
let NOP = 1

[<Literal>]
let JUMP = 2

[<Literal>]
let JUMP_IF_ZERO = 3 // Relative to next signed byte (immediate arg)

[<Literal>]
let SET_STACK = 4

[<Literal>]
let GET_PC = 5

[<Literal>]
let GET_STACK = 6

// 7: unused for now

// Push the next byte(s) as a zero-padded 64-bit integer.
[<Literal>]
let PUSH1 = 8

[<Literal>]
let PUSH2 = 9

[<Literal>]
let PUSH4 = 10

[<Literal>]
let PUSH8 = 11


// Sign extension
[<Literal>]
let SIGN1 = 12 // From 8 to 64 bits

[<Literal>]
let SIGN2 = 13 // From 16 to 64 bits

[<Literal>]
let SIGN4 = 14 // From 32 to 64 bits


// 15: Unused

[<Literal>]
let LOAD1 = 16

[<Literal>]
let LOAD2 = 17

[<Literal>]
let LOAD4 = 18

[<Literal>]
let LOAD8 = 19


[<Literal>]
let STORE1 = 20

[<Literal>]
let STORE2 = 21

[<Literal>]
let STORE4 = 22

[<Literal>]
let STORE8 = 23


[<Literal>]
let ALLOCATE = 24

[<Literal>]
let DEALLOCATE = 25

[<Literal>]
let OUTPUT = 26

[<Literal>]
let INPUT = 27


// 28-31: Unused

[<Literal>]
let ADD = 32

[<Literal>]
let MULTIPLY = 33

[<Literal>]
let DIVIDE = 34 // Unsigned.

[<Literal>]
let REMAINDER = 35 // Unsigned.

[<Literal>]
let LESS_THAN = 36 // Unsigned. 0: false. FF...F: true.

// 36-39: Unused

[<Literal>]
let AND = 40

[<Literal>]
let OR = 41

[<Literal>]
let NOT = 42

[<Literal>]
let XOR = 43

[<Literal>]
let SHIFT = 44


type Machine(initialMemory, input, output) =
    inherit BaseMachine(initialMemory, input, output)

    // We normally start the machine with...
    // memory contents: <prog> 0y <args> 0L 0L 0L 0L
    // program counter: 0
    // stack pointer: after the last zero
    new (program: int seq, arguments: int seq, input: int seq, output) =
        Machine (seq [program; seq [0]; arguments; Seq.replicate (4*8) 0] |> Seq.concat |> Seq.map byte,
                 Seq.map byte input,
                 output)

    override m.Step () =
        let pushN n = m.NextOp n |> m.Push
        let signN n = m.Pop () |> signExtend n |> m.Push
        let loadN n = m.Pop () |> m.LoadN n |> m.Push
        let storeN n = m.StoreN n (m.Pop ()) (m.Pop ()) // Address on top!

        match m.NextOp 1 |> int with
        | EXIT -> m.Terminated <- true
        | NOP -> ()

        | JUMP -> m.ProgramCounter <- m.Pop ()
        | JUMP_IF_ZERO ->
            let offset = m.NextOp 1 |> signExtend 1
            if m.Pop () = 0UL
            then m.ProgramCounter <- m.ProgramCounter + offset
        | SET_STACK -> m.StackPointer <- m.Pop ()
        | GET_PC -> m.ProgramCounter |> m.Push
        | GET_STACK -> m.StackPointer |> m.Push

        | PUSH1 -> pushN 1
        | PUSH2 -> pushN 2
        | PUSH4 -> pushN 4
        | PUSH8 -> pushN 8

        | SIGN1 -> signN 1
        | SIGN2 -> signN 2
        | SIGN4 -> signN 4

        | LOAD1 -> loadN 1
        | LOAD2 -> loadN 2
        | LOAD4 -> loadN 4
        | LOAD8 -> loadN 8

        | STORE1 -> storeN 1
        | STORE2 -> storeN 2
        | STORE4 -> storeN 4
        | STORE8 -> storeN 8

        | ALLOCATE -> m.Pop () |> m.Allocate |> m.Push
        | DEALLOCATE -> m.Pop () |> m.Deallocate
        | OUTPUT -> flip m.Output (m.Pop ()) (m.Pop ())
        | INPUT -> flip m.Input (m.Pop ()) (m.Pop ()) |> m.Push

        | ADD -> m.Pop () + m.Pop () |> m.Push
        | MULTIPLY -> m.Pop () * m.Pop () |> m.Push
        | DIVIDE -> flip (/) (m.Pop ()) (m.Pop ()) |> m.Push
        | REMAINDER -> flip (%) (m.Pop ()) (m.Pop ()) |> m.Push
        | LESS_THAN -> (if m.Pop () > m.Pop () then -1 else 0) |> uint64 |> m.Push

        | AND -> (m.Pop ()) &&& (m.Pop ()) |> m.Push
        | OR -> (m.Pop ()) ||| (m.Pop ()) |> m.Push
        | NOT -> ~~~ (m.Pop ()) |> m.Push
        | XOR -> (m.Pop ()) ^^^ (m.Pop ()) |> m.Push

        | SHIFT ->
            let steps = m.Pop () |> int
            let value = m.Pop ()
            let result = if steps > 0 then value <<< steps
                         else if steps < 0 then value >>> -steps
                         else value
            result |> m.Push
        | _ -> raise UndefinedException


type Architecture() =
    inherit BaseArchitecture()

    let nBytes (x: int) =
        let y = Math.Abs x
        if y < 0x100 then 1
        else if y < 0x10000 then 2
        else 4

    let pushN n =
        match n with
        | 1 -> PUSH1
        | 2 -> PUSH2
        | 4 -> PUSH4
        | 8 -> PUSH8
        | _ -> raise UndefinedException

    let signN n =
        match n with
        | 1 -> SIGN1
        | 2 -> SIGN2
        | 4 -> SIGN4
        | _ -> raise UndefinedException

    let loadN n =
        match n with
        | 1 -> LOAD1
        | 2 -> LOAD2
        | 4 -> LOAD4
        | 8 -> LOAD8
        | _ -> raise UndefinedException

    let storeN n =
        match n with
        | 1 -> STORE1
        | 2 -> STORE2
        | 4 -> STORE4
        | 8 -> STORE8
        | _ -> raise UndefinedException

    let bytes n (x: int) = uint64 x |> toBytes n |> Seq.map int |> Seq.toList

    override a.CreateCore initialMemory input output =
        Machine(initialMemory, input, output) :> BaseMachine

    override a.Exit = [EXIT]
    override a.Input = [INPUT]
    override a.Output = [OUTPUT]

    override a.Push x =
        let n = nBytes x
        let bytes = uint64 x |> toBytes n |> Seq.map int |> Seq.toList
        let p = [pushN n] @ bytes
        if x >= 0 then p else p @ [signN n]

    override a.Allocate = [ALLOCATE]
    override a.Deallocate = [DEALLOCATE]
    override a.Add = [ADD]
    override a.Not = [NOT]
    override a.Multiply = [MULTIPLY]
    override a.Divide = [DIVIDE]
    override a.LessThan = [LESS_THAN]
    override a.And = [AND]
    override a.Or = [OR]
    override a.Xor = [NOT]
    override a.Shift = [SHIFT]

    override a.SignN n = [signN n]
    override a.LoadN n = [loadN n]

    override a.StoreN n = [storeN n]
    override a.SetStack = [SET_STACK]

    override a.Addr i = [GET_STACK] @ a.AddC (i*8)
    override a.Pop n = if n=1 then [JUMP_IF_ZERO; 0] else base.Pop n

    override a.Jump offset = a.Push (offset+2) @ [GET_PC; ADD; JUMP]
    override a.JumpIfZero offset =
        if offset >= -128 && offset <= 127
        then [JUMP_IF_ZERO; offset]
        else let jump = a.Jump offset in a.JumpIfNotZero (opLen jump) @ jump

    override a.JumpIfNotZero offset = a.Push 1 @ a.LessThan @ a.JumpIfZero offset

    override a.StoreLiterally data =
        let copyAndJump = a.CopyRange @ a.Jump (opLen data)
        let m = opLen copyAndJump
        let n = List.length data
        List.concat [
            [GET_PC; PUSH1; 4+3+m+n; ADD]
            [GET_PC; PUSH1; 3+m; ADD]
            copyAndJump // m
            data // n
        ]
