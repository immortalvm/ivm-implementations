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
let JUMP_IF_ZERO = 3 // Offset: the next byte (signed)

[<Literal>]
let SET_STACK = 4

[<Literal>]
let GET_PC = 5

[<Literal>]
let GET_STACK = 6

// 7 unused for now

// Push the next byte as a zero-padded 64-bit integer.
[<Literal>]
let PUSH1 = 8

[<Literal>]
let PUSH2 = 9

[<Literal>]
let PUSH4 = 10

[<Literal>]
let PUSH8 = 11

// 12-15 unused for now

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


// 28-31 unused for now

[<Literal>]
let ADD = 32

[<Literal>]
let MULTIPLY = 33

[<Literal>]
let AND = 34

[<Literal>]
let NOT = 35


type Machine(initialMemory, input, output) =
    inherit BaseMachine64(initialMemory, input, output)

    // We normally start the machine with...
    // memory contents: <prog> 0 <args> 0 0 0 0
    // program counter: 0
    // stack pointer: after the last zero
    new (program: int seq, arguments: int seq, input: int seq, output) =
        Machine (seq [program; seq [0]; arguments; seq [0; 0; 0; 0]] |> Seq.concat |> Seq.map byte,
                 Seq.map byte input,
                 output)

    override m.Step () =
        let op = m.NextOp () |> int

        let jump () = m.ProgramCounter <- m.Pop ()
        let pushN n =
            [| 0 .. n-1 |]
            |> Seq.map (fun _ -> m.NextOp () |> uint64) // TODO: Reverse?
            |> Seq.mapi (fun i x -> x <<< i*8)
            |> Seq.sum
            |> m.Push

        let loadN n = m.Pop () |> m.LoadN n |> m.Push
        let storeN n = m.StoreN n (m.Pop ()) (m.Pop ()) // Address on top!

        match op with
        | EXIT -> m.Terminated <- true
        | NOP -> ()
        | JUMP -> jump ()
        | JUMP_IF_ZERO -> if m.Pop () = 0UL then jump () else ()
        | SET_STACK -> m.StackPointer <- m.Pop ()
        | GET_PC -> m.Push m.ProgramCounter
        | GET_STACK -> m.Push m.StackPointer
        | PUSH1 -> pushN 1
        | PUSH2 -> pushN 2
        | PUSH4 -> pushN 4
        | PUSH8 -> pushN 8
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
        | AND -> (m.Pop ()) &&& (m.Pop ()) |> m.Push
        | NOT -> ~~~ (m.Pop ()) |> m.Push
