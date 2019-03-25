module VM0

open System

exception AccessException
exception UndefinedException

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

type Machine(program: seq<uint32>, input: seq<uint32>, output: seq<uint32> -> unit) =

    // Reverse ordering
    let mutable arrays = [ (uint32 0, Seq.toArray program) ]

    let mutable nextUnused = let (_, arr) = arrays.[0] in uint32(Array.length arr)

    let getArray (location: uint32) =
        match List.skipWhile
            (fun (start, _) -> start > location)
            arrays with
        | [] -> raise AccessException
        | (start, arr) :: _ ->
            if location < start + uint32 (Array.length arr)
            then (arr, int (location - start))
            else raise AccessException

    let get location =
        let (a, i) = getArray location in a.[i]

    let set location value = // Arguments order: as popped from the stack
        let (a, i) = getArray location in a.[i] <- value


    let mutable programCounter = 0u // program counter (next location)
    let mutable stackPointer = nextUnused // stack pointer (next location)
    let mutable terminated = false


    let nextOp () =
        let op = get programCounter
        programCounter <- programCounter + 1u
        op

    let pop () =
        stackPointer <- stackPointer - 1u
        get stackPointer

    let push value =
        set stackPointer value
        stackPointer <- stackPointer + 1u

    let allocate size =
        let start = nextUnused
        arrays <- (start, Array.zeroCreate (int size)) :: arrays
        nextUnused <- start + size
        start

    let deallocate start =
        let rec de arrs =
            match arrs with
            | [] -> raise AccessException
            | (st, a) :: rest ->
                if st > start then (st, a) :: de rest
                elif st.Equals(start) then rest
                else raise AccessException
        arrays <- de arrays

    let output start stop =
        Seq.map get { start .. stop - 1u} |> output

    let input start stop =
        let mutable counter = 0
        for (i, data) in Seq.zip {start .. stop - 1u} input do
            counter <- counter + 1
            set i data
        start + uint32 counter

    let flip f x y = f y x // Useful since arguments are popped.

    let step () =
        let op = nextOp () |> int
        // printfn "Top: %6d, op: %2d" (if stackPointer > uint32 0 then get (stackPointer - 1u) |> int else -1) op

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
        | IS_ZERO -> (if pop() = 0u then 1 else 0) |> uint32 |> push
        | _ -> raise UndefinedException

    member this.Run () = while not terminated do step ()


    // For testing:

    member this.ProgramCounter
        with get () = programCounter
        and set (value) = programCounter <- value

    member this.StackPointer
        with get () = stackPointer
        and set (value) = stackPointer <- value

    member this.Allocated = List.rev [ for (start, arr) in arrays ->
                                       (start, start + uint32 (Array.length arr)) ]

    member this.Get location = get location


// ------------ Pseudo operations ------------

let addC n = [PUSH; n; ADD]

let subtractC n = addC -n

let multiplyC n = [PUSH; n; MULTIPLY]

// Push a copy of the n'th element of the stack on top of the stack, counting from 0.
let copy i = [GET_STACK] @ subtractC (i + 1) @ [GET]

// Repeat until 0.
// Pops top of stack after each iteration. Uses relative jump.
// Pushes the PC on the stack every iteration in order to avoid polluting the stack.
// It would be faster to do this once at the start, or at compile time (if the
// location is fixed).
let doWhileNotZero block =
    let n = List.length block
    List.concat [
        block
        [IS_ZERO] @ multiplyC (n + 10)
        [GET_PC] @ subtractC (n + 5)
        [ADD; JUMP]
    ]

// Remove the top n elements from the stack.
let removeTop n = [GET_STACK] @ subtractC n @ [SET_STACK]

// Swap two stack elements.
let swap i j =
    List.concat [
        [GET_STACK] @ subtractC (i + 1)
        [GET_STACK] @ subtractC (j + 2)
        [GET]
        [GET_STACK] @ subtractC (j + 3)
        [GET_STACK] @ subtractC (i + 4)
        [GET; SET; SET]
    ]

// Get "off the ground", starting from
// <prog> 0 <args> 0 0
// with stack counter after the last 0.
let obtainProperStack words =
    List.concat [
        [ADD; ADD] // Essentially pop the last two zeros, so that we do not cause stack overflow.
        [PUSH; words; ALLOCATE] // Allocate stack with space for 'words' 32-bit words.
        copy 0 @ [GET_STACK] @ subtractC 2 @ [SET] // Push current argument pointer to new stack
        addC 1 @ [SET_STACK]
    ]


// ------------ Example(s) ------------

// We currently start the machine with...
//
// memory contents: <prog> 0 <args> 0 0
// program counter: 0
// stack pointer: after the last zero
//
// The two zeros at the end are for the initial stack.
let runExample program arguments inputString =
    let output (s: seq<uint32>) = for x in s do printfn "Output: %d" x
    let machine = Machine (Seq.map uint32 (program @ [0] @ arguments @ [0;0]),
                           Seq.map uint32 inputString,
                           output)
    try machine.Run () with
    | AccessException -> printfn "Access violation!"
    | UndefinedException -> printfn "Undefined operation!"

// Outputs the input arguments (1, 22, 333, 4444)
let example1 () =
    let program =
        List.concat [
            obtainProperStack (1<<<14)
            copy 0
            doWhileNotZero <| subtractC 1 @ copy 0 @ [GET]
            addC 1
            swap 0 1
            [OUTPUT; EXIT]
        ]
    runExample program [1; 22; 333; 4444] ""
