open System

exception AccessVioation
exception UndefinedOperation

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
let GET = 6

[<Literal>]
let SET = 7

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
let AND = 14

[<Literal>]
let NOT = 15

[<Literal>]
let IS_ZERO = 16

type Machine(program: seq<uint64>, input: seq<uint64>, output: seq<uint64> -> unit) =
    let mutable programCounter = 0UL // program counter (next location)
    let mutable stackPointer = 0UL // stack pointer (next location)

    let mutable terminated = false

    // Reverse ordering
    let mutable arrays = [ (uint64 0, Seq.toArray program) ]

    let mutable nextUnused = let (_, arr) = arrays.[0] in uint64(Array.length arr)

    let getArray (location: uint64) =
        match List.skipWhile
            (fun (start, _) -> start > location)
            arrays with
        | [] -> raise AccessVioation
        | (start, arr) :: _ ->
            if location < start + uint64 (Array.length arr)
            then (arr, int (location - start))
            else raise AccessVioation


    let get location =
        let (a, i) = getArray location in a.[i]

    let set location value = // Arguments order: as popped from the stack
        let (a, i) = getArray location in a.[i] <- value

    let nextOp () =
        let op = get programCounter
        programCounter <- programCounter + 1UL
        op

    let pop () =
        stackPointer <- stackPointer - 1UL
        get stackPointer

    let push value =
        set stackPointer value
        stackPointer <- stackPointer + 1UL

    let allocate size =
        let start = nextUnused
        arrays <- (start, Array.zeroCreate (int size)) :: arrays
        nextUnused <- start + size
        start

    let deallocate start =
        let rec de arrs =
            match arrs with
            | [] -> raise AccessVioation
            | (st, a) :: rest ->
                if st > start then (st, a) :: de rest
                elif st.Equals(start) then rest
                else raise AccessVioation
        arrays <- de arrays

    let output start stop =
        Seq.map get { start .. stop - 1UL} |> output

    let input start stop =
        let mutable counter = 0
        for (i, data) in Seq.zip {start .. stop - 1UL} input do
            counter <- counter + 1
            set i data
        start + uint64 counter

    let flip f x y = f y x // Useful since arguments are popped.

    let run1 () =
        let op = nextOp () |> int
        // printfn "Top: %6d, op: %s" (if sp > uint64 0 then get (sp - uint64 1) |> int else -1) op

        match op with
        | EXIT -> terminated <- true
        | JUMP -> programCounter <- pop ()
        | SET_STACK -> stackPointer <- pop ()
        | PUSH -> nextOp () |> push
        | GET_PC -> push programCounter
        | GET_STACK -> push stackPointer
        | GET -> pop () |> get |> push
        | SET -> flip set (pop ()) (pop ())
        | ALLOCATE -> pop () |> allocate |> push
        | DEALLOCATE -> pop () |> deallocate
        | OUTPUT -> flip output (pop ()) (pop ())
        | INPUT -> flip input (pop ()) (pop ()) |> push
        | ADD -> pop () + pop () |> push
        | MULTIPLY -> pop () * pop () |> push
        | AND -> pop () &&& pop () |> push
        | NOT -> ~~~ (pop ()) |> push
        | IS_ZERO -> (if pop().Equals(0) then 1 else 0) |> uint64 |> push
        | _ -> raise UndefinedOperation

    member this.run () = while not terminated do run1 ()

[<EntryPoint>]
let main argv =
    let input = Seq.map uint64 "abc 123"
    let output (s: seq<uint64>) = for x in s do printfn "Output: %d" x

    let program = [
        PUSH; 1<<<14; ALLOCATE; SET_STACK // Allocate 16k 64-bit word stack (overwriting address 0 in the process)
        PUSH; 1<<<8; ALLOCATE // Allocate 256 word temporary buffer (pointer on top of stack)

        GET_STACK; PUSH; -1; ADD; GET // Duplicate top of stack
        GET_STACK; PUSH; -1; ADD; GET // Duplicate top of stack
        PUSH; 5; ADD // Add 5 to (stop) pointer

        INPUT // Read max 5 words into buffer -> Next location (not written to).
        OUTPUT // Write everything that was read.
        EXIT
    ]
    let machine = new Machine(program |> Seq.map uint64, input, output)

    try machine.run () with
    | AccessVioation -> printfn "Access violation!"
    | UndefinedOperation -> printfn "Undefined operation!"

    0 // return an integer exit code
