module VmBase64

open System

exception AccessException
exception UndefinedException

let flip f x y = f y x // Useful since arguments are popped.

[<AbstractClass>]
type BaseMachine64(initialMemory: byte seq, input: byte seq, output: byte seq -> unit) =

    // Reverse ordering
    let mutable arrays = [ (uint64 0, Seq.toArray initialMemory) ]

    let getArray (location: uint64) =
        match List.skipWhile
            (fun (start, _) -> start > location)
            arrays with
        | [] -> raise AccessException
        | (start, arr) :: _ ->
            if location < start + uint64 (Array.length arr)
            then arr, int (location - start)
            else raise AccessException

    // Notice that this is just the initial value.
    let mutable nextUnused = arrays.[0] |> snd |> Array.length |> uint64

    let load location =
        let (a, i) = getArray location in a.[i]

    let store location value =
        let (a, i) = getArray location in a.[i] <- value


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

    member val ProgramCounter = 0UL with get, set // program counter (next location)
    member val StackPointer = nextUnused with get, set // stack pointer (next location)

    member m.Load1 location = load location |> uint64
    // Little-endian, n = 1,2,4,8
    member m.LoadN n location =
        [| location .. location + uint64(n-1) |]
        |> Seq.map m.Load1
        |> Seq.mapi (fun i x -> x <<< i*8)
        |> Seq.sum

    // Little-endian, n = 1,2,4,8
    member m.StoreN n location (value: uint64) =
        [| location .. location + uint64(n-1) |]
        |> Seq.iteri (fun i loc -> store (loc + uint64 i) (byte (value >>> i*8)))

    member m.NextOp () : byte =
        let op = load m.ProgramCounter
        m.ProgramCounter <- m.ProgramCounter + 1UL
        op

    // Only 64-bit values on stack.
    member m.Pop () =
        m.StackPointer <- m.StackPointer - 8UL
        m.LoadN 8 m.StackPointer

    member m.Push value =
        m.StoreN 8 m.StackPointer value
        m.StackPointer <- m.StackPointer + 8UL

    member m.Output start stop =
        Seq.map load { start .. stop - 1UL} |> output

    member m.Input start stop =
        let mutable counter = 0
        for (i, data) in Seq.zip {start .. stop - 1UL} input do
            counter <- counter + 1
            store i data
        start + uint64 counter

    abstract member Step: unit -> unit

    member m.Run () = while not m.Terminated do m.Step ()
