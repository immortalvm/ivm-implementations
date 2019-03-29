module VmBase

open System

exception AccessException
exception UndefinedException

let flip f x y = f y x // Useful since arguments are popped.

[<AbstractClass>]
type BaseMachine(program: seq<uint32>, input: seq<uint32>, output: seq<uint32> -> unit) =

    // Reverse ordering
    let mutable arrays = [ (uint32 0, Seq.toArray program) ]

    let getArray (location: uint32) =
        match List.skipWhile
            (fun (start, _) -> start > location)
            arrays with
        | [] -> raise AccessException
        | (start, arr) :: _ ->
            if location < start + uint32 (Array.length arr)
            then arr, int (location - start)
            else raise AccessException

    let mutable nextUnused = arrays.[0] |> snd |> Array.length |> uint32

    member m.Allocate size =
        let start = nextUnused
        arrays <- (start, Array.zeroCreate (int size)) :: arrays
        nextUnused <- start + size
        start

    member m.Deallocate start =
        let rec de arrs =
            match arrs with
            | [] -> raise AccessException
            | (st, a) :: rest ->
                if st > start then (st, a) :: de rest
                elif st.Equals(start) then rest
                else raise AccessException
        arrays <- de arrays


    member val Terminated = false with get, set

    member val ProgramCounter = 0u with get, set // program counter (next location)
    member val StackPointer = nextUnused with get, set // stack pointer (next location)

    member m.Load location =
        let (a, i) = getArray location in a.[i]

    member m.Store location value =
        let (a, i) = getArray location in a.[i] <- value

    member m.NextOp () =
        let op = m.Load m.ProgramCounter
        m.ProgramCounter <- m.ProgramCounter + 1u
        op

    member m.Pop () =
        m.StackPointer <- m.StackPointer - 1u
        m.Load m.StackPointer

    member m.Push value =
        m.Store m.StackPointer value
        m.StackPointer <- m.StackPointer + 1u

    member m.Output start stop =
        Seq.map m.Load { start .. stop - 1u} |> output

    member m.Input start stop =
        let mutable counter = 0
        for (i, data) in Seq.zip {start .. stop - 1u} input do
            counter <- counter + 1
            m.Store i data
        start + uint32 counter

    abstract member Step: unit -> unit

    member m.Run () = while not m.Terminated do m.Step ()


    // For testing:

    member m.Allocated = List.rev [ for (start, arr) in arrays ->
                                       (int start, int start + Array.length arr) ]

    member m.Stack n = [m.StackPointer - (uint32 n) .. m.StackPointer - 1u]
                          |> Seq.map (m.Load >> int)
