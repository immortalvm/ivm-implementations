module VM1

open System
open VmBase

[<Literal>]
let EXIT = 0

[<Literal>]
let NOP = 1

[<Literal>]
let JUMP = 2

[<Literal>]
let SET_STACK = 3

[<Literal>]
let SHIFT8 = 4 // Shift top of stack 8 bits left.

[<Literal>]
let LOAD = 5

[<Literal>]
let STORE = 6

[<Literal>]
let ALLOCATE = 7

[<Literal>]
let DEALLOCATE = 8

[<Literal>]
let OUTPUT = 9

[<Literal>]
let INPUT = 10

[<Literal>]
let ADD = 11

[<Literal>]
let MULTIPLY = 12

[<Literal>]
let AND = 13

[<Literal>]
let NOT = 14

[<Literal>]
let SKIP_IF_ZERO = 15

[<Literal>]
let LOAD_BYTE = 16

[<Literal>]
let STORE_BYTE = 17


type Machine(offsetBits, program, input, output) =
    inherit BaseMachine(program, input, output)

    let maskAndSignExtend bits (x: uint32) =
        let mask = (1u <<< bits) - 1u
        let y = x &&& mask
        if y >>> (bits - 1) = 0u
        then y
        else y ||| ~~~mask

    // Little-endian
    member m.LoadByte start offset =
        let addr = start + (offset >>> 2)
        let word = m.Load addr
        0xffu &&& (word >>> (8 * int (offset &&& 3u)))

    // Only the lower 8 bits of value will be used.
    member m.StoreByte start offset value =
        let addr = start + (offset >>> 2)
        let shift = 8 * int (offset &&& 3u)
        let word = m.Load addr ^^^ ~~~ (0xffu <<< shift)
        m.Store addr (word ||| ((value &&& 0xffu)) >>> shift)

    // We normally start the machine with...
    // memory contents: <prog> 0 <args> 0 0 0 0
    // program counter: 0
    // stack pointer: after the last zero
    new (offsetBits: int, program: int seq, arguments: int seq, input: int seq, output) =
        Machine (offsetBits,
                 seq [program; seq [0]; arguments; seq [0; 0; 0; 0]] |> Seq.concat |> Seq.map uint32,
                 Seq.map uint32 input,
                 output)

    override m.Step () =
        let mutable op = m.NextOp ()

        match int op &&& 3 with
            | 1 -> m.ProgramCounter |> m.Push
            | 2 -> m.StackPointer |> m.Push
            | 3 -> 0u |> m.Push
            | _ -> ()

        op <- op >>> 2

        let c = maskAndSignExtend offsetBits op
        if c <> 0u
        then m.Pop() + c |> m.Push

        op <- op >>> offsetBits
        match int op with
        | EXIT -> m.Terminated <- true
        | NOP -> ()
        | JUMP -> m.ProgramCounter <- m.Pop ()
        | SET_STACK -> m.StackPointer <- m.Pop ()
        | SHIFT8 -> m.Pop () <<< 8 |> m.Push
        | LOAD -> m.Pop () |> m.Load |> m.Push
        | STORE -> m.Store (m.Pop ()) (m.Pop ()) // Address on top!
        | ALLOCATE -> m.Pop () |> m.Allocate |> m.Push
        | DEALLOCATE -> m.Pop () |> m.Deallocate
        | OUTPUT -> flip m.Output (m.Pop ()) (m.Pop ())
        | INPUT -> flip m.Input (m.Pop ()) (m.Pop ()) |> m.Push
        | ADD -> m.Pop () + m.Pop () |> m.Push
        | MULTIPLY -> m.Pop () * m.Pop () |> m.Push
        | AND -> m.Pop () &&& m.Pop () |> m.Push
        | NOT -> ~~~ (m.Pop ()) |> m.Push
        | SKIP_IF_ZERO -> if m.Pop () = 0u then m.NextOp () |> ignore

        | LOAD_BYTE -> flip m.LoadByte (m.Pop ()) (m.Pop ()) |> m.Push
        | STORE_BYTE ->
            let value = m.Pop ()
            let offset = m.Pop ()
            let start = m.Pop ()
            m.StoreByte start offset value

        | _ -> raise UndefinedException



// Offset bits should be 24, 25 or 26.
type Architecture(?offsetBits: int) =
    inherit BaseArchitecture()

    let aBits = 2
    let bBits = defaultArg offsetBits 25
    let cBits = 32 - aBits - bBits

    let minOffset = -(1 <<< (bBits-1))
    let maxOffset = -minOffset - 1
    let possibleOffset i = minOffset <= i && i <= maxOffset
    let offsetOnes = (1 <<< bBits) - 1

    let instruction push offset operation = [(((operation <<< bBits) ||| (offset &&& offsetOnes)) <<< aBits) ||| push]
    let iN = instruction 0 // Nothing pushed before offset
    let iC = instruction 1 // Program counter pushed
    let iS = instruction 2 // Stack pointer pushed
    let iP = instruction 3 // 0 pushed

    let withConst n operation = if possibleOffset n
                                then iP n operation
                                else iP (n >>> 8) SHIFT8 @ iN (n &&& 255) operation

    let withAddr i operation =
        assert possibleOffset i
        iS (-i-1) operation

    override a.CreateCore initialMemory input output =
        Machine(bBits, initialMemory, input, output) :> BaseMachine

    override a.Exit = iN 0 EXIT
    override a.Input = iN 0 INPUT
    override a.Output = iN 0 OUTPUT
    override a.Push n = withConst n NOP
    override a.Allocate = iN 0 ALLOCATE
    override a.AllocateC n = withConst n ALLOCATE
    override a.Deallocate = iN 0 DEALLOCATE
    override a.Add = iN 0 ADD
    override a.AddC n = if possibleOffset n
                        then iN n NOP // Minor optimization
                        else withConst n ADD
    override a.Not = iN 0 NOT
    override a.Subtract = iN 0 NOT @ iN 1 ADD
    override a.Multiply = iN 0 MULTIPLY
    override a.MultiplyC n = withConst n MULTIPLY
    override a.Load = iN 0 LOAD
    override a.Store = iN 0 STORE
    override a.SetStack = iN 0 SET_STACK

    override a.Addr i = withAddr i NOP
    override a.Get i = withAddr i LOAD
    override a.Set i = withAddr i STORE

    override a.Pop n = if n=0 then [] else withAddr (n-1) SET_STACK
    override a.Jump n = if possibleOffset n
                        then iC n JUMP
                        // This is not very elegant, but presumably it is rare.
                        else iP (n >>> 8) SHIFT8 @ iC (n &&& 255) ADD @ iN 0 JUMP

    override a.JumpIfNotZero n =
        assert possibleOffset n
        iN 0 SKIP_IF_ZERO
        @ iC n JUMP

    override a.JumpIfZero n =
        assert possibleOffset n
        iN 0 SKIP_IF_ZERO
        @ iC 1 JUMP
        @ iC n JUMP

    override a.StoreLiterally data =
        let copyAndJump = a.CopyRange @ a.Jump (opLen data)
        let m = opLen copyAndJump
        let n = List.length data
        List.concat [
            iC (1+m+n) NOP
            iC m NOP
            copyAndJump
            data
        ]

    override a.LoadByte = iN 0 LOAD_BYTE
    override a.StoreByte = iN 0 STORE_BYTE
