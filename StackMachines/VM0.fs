module VM0

open System
open VmBase

[<Literal>]
let EXIT = 0

[<Literal>]
let JUMP = 1

[<Literal>]
let SET_STACK = 2

[<Literal>]
let PUSH = 3

[<Literal>]
let GET_PC = 4

[<Literal>]
let GET_STACK = 5

[<Literal>]
let LOAD = 6

[<Literal>]
let STORE = 7

[<Literal>]
let ALLOCATE = 8

[<Literal>]
let DEALLOCATE = 9

[<Literal>]
let OUTPUT = 10

[<Literal>]
let INPUT = 11

[<Literal>]
let ADD = 12

[<Literal>]
let MULTIPLY = 13

[<Literal>]
let NOT = 14

[<Literal>]
let IS_ZERO = 15


type Machine(initialMemory, input, output) =
    inherit BaseMachine(initialMemory, input, output)

    // We normally start the machine with...
    // memory contents: <prog> 0 <args> 0 0 0 0
    // program counter: 0
    // stack pointer: after the last zero
    new (program: int seq, arguments: int seq, input: int seq, output) =
        Machine (seq [program; seq [0]; arguments; seq [0; 0; 0; 0]] |> Seq.concat |> Seq.map uint32,
                 Seq.map uint32 input,
                 output)

    override m.Step () =
        let op = m.NextOp () |> int
        // printfn "Top: %6d, op: %2d" (if stackPointer > uint32 0 then load (stackPointer - 1u) |> int else -999) op

        match op with
        | EXIT -> m.Terminated <- true
        | JUMP -> m.ProgramCounter <- m.Pop ()
        | SET_STACK -> m.StackPointer <- m.Pop ()
        | PUSH -> m.NextOp () |> m.Push
        | GET_PC -> m.Push m.ProgramCounter
        | GET_STACK -> m.Push m.StackPointer
        | LOAD -> m.Pop () |> m.Load |> m.Push
        | STORE -> m.Store (m.Pop ()) (m.Pop ()) // Address on top!
        | ALLOCATE -> m.Pop () |> m.Allocate |> m.Push
        | DEALLOCATE -> m.Pop () |> m.Deallocate
        | OUTPUT -> flip m.Output (m.Pop ()) (m.Pop ())
        | INPUT -> flip m.Input (m.Pop ()) (m.Pop ()) |> m.Push
        | ADD -> m.Pop () + m.Pop () |> m.Push
        | MULTIPLY -> m.Pop () * m.Pop () |> m.Push
        | NOT -> ~~~ (m.Pop ()) |> m.Push
        | IS_ZERO -> (if m.Pop() = 0u then 1 else 0) |> uint32 |> m.Push
        | _ -> raise UndefinedException


type Architecture() =
    inherit BaseArchitecture()

    override a.CreateCore initialMemory input output =
        Machine(initialMemory, input, output) :> BaseMachine

    override a.Exit = [EXIT]
    override a.Input = [INPUT]
    override a.Output = [OUTPUT]
    override a.Push n = [PUSH; n]
    override a.Allocate = [ALLOCATE]
    override a.Deallocate = [DEALLOCATE]
    override a.Add = [ADD]
    override a.Not = [NOT]
    override a.Multiply = [MULTIPLY]
    override a.Load = [LOAD]
    override a.Store = [STORE]
    override a.SetStack = [SET_STACK]

    override a.Addr i = [GET_STACK] @ a.AddC -(i + 1)
    override a.Jump offset = [PUSH; offset+2; GET_PC; ADD; JUMP]
    override a.JumpIfZero offset =
        List.concat [
            [IS_ZERO]
            a.MultiplyC offset
            a.AddC 2
            [GET_PC; ADD; JUMP]
        ]
    override a.JumpIfNotZero offset =
        List.concat [
            [IS_ZERO]
            a.MultiplyC -offset
            a.AddC <| offset + 2
            [GET_PC; ADD; JUMP]
        ]

    override a.StoreLiterally data =
        let copyAndJump = a.CopyRange @ a.Jump (opLen data)
        let m = opLen copyAndJump
        let n = List.length data
        List.concat [
            [GET_PC; PUSH; 7+m+n; ADD]
            [GET_PC; PUSH; 3+m; ADD]
            copyAndJump
            data
        ]
