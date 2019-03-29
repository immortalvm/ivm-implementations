module VM1

open System
open VmBase

[<Literal>]
let EXIT = 0

[<Literal>]
let JUMP = 1

[<Literal>]
let SET_STACK = 2

[<Literal>]
let PUSH = 3 // Replace with shift?

[<Literal>]
let LOAD = 4

[<Literal>]
let STORE = 5

[<Literal>]
let ALLOCATE = 6

[<Literal>]
let DEALLOCATE = 7

[<Literal>]
let OUTPUT = 8

[<Literal>]
let INPUT = 9

[<Literal>]
let ADD = 10

[<Literal>]
let MULTIPLY = 11

[<Literal>]
let AND = 12

[<Literal>]
let NOT = 13

[<Literal>]
let IS_ZERO = 14


type Machine(program, input, output) =

    inherit BaseMachine(program, input, output)

    let maskAndSignExtend bits (x: uint32) =
        let mask = (1u <<< bits) - 1u
        let y = x &&& mask
        if y >>> (bits - 1) = 0u
        then y
        else y ||| ~~~mask

    override m.Step () =
        let mutable op = m.NextOp ()

        match int op &&& 3 with
            | 1 -> m.ProgramCounter |> m.Push
            | 2 -> m.StackPointer |> m.Push
            | 3 -> 0u |> m.Push
            | _ -> ()

        op <- op >>> 2

        let c = maskAndSignExtend 26 op
        if c <> 0u
        then m.Pop() + c |> m.Push

        op <- op >>> 26
        match int op with
        | EXIT -> m.Terminated <- true
        | JUMP -> m.ProgramCounter <- m.Pop ()
        | SET_STACK -> m.StackPointer <- m.Pop ()
        | PUSH -> m.NextOp () |> m.Push
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
        | IS_ZERO -> (if m.Pop() = 0u then 1 else 0) |> uint32 |> m.Push
        | _ -> raise UndefinedException
