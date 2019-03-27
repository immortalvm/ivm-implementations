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
let AND = 14

[<Literal>]
let NOT = 15

[<Literal>]
let IS_ZERO = 16

type Machine(program: seq<uint32>, input: seq<uint32>, output: seq<uint32> -> unit) =

    // Reverse ordering
    let mutable arrays = [ (uint32 0, Seq.toArray program) ]

    let mutable nextUnused = arrays.[0] |> snd |> Array.length |> uint32

    let getArray (location: uint32) =
        match List.skipWhile
            (fun (start, _) -> start > location)
            arrays with
        | [] -> raise AccessException
        | (start, arr) :: _ ->
            if location < start + uint32 (Array.length arr)
            then arr, int (location - start)
            else raise AccessException

    let load location =
        let (a, i) = getArray location in a.[i]

    let store location value = // Arguments order: as popped from the stack
        let (a, i) = getArray location in a.[i] <- value


    let mutable programCounter = 0u // program counter (next location)
    let mutable stackPointer = nextUnused // stack pointer (next location)
    let mutable terminated = false


    let nextOp () =
        let op = load programCounter
        programCounter <- programCounter + 1u
        op

    let pop () =
        stackPointer <- stackPointer - 1u
        load stackPointer

    let push value =
        store stackPointer value
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
        Seq.map load { start .. stop - 1u} |> output

    let input start stop =
        let mutable counter = 0
        for (i, data) in Seq.zip {start .. stop - 1u} input do
            counter <- counter + 1
            store i data
        start + uint32 counter

    let flip f x y = f y x // Useful since arguments are popped.

    let step () =
        let op = nextOp () |> int
        // printfn "Top: %6d, op: %2d" (if stackPointer > uint32 0 then load (stackPointer - 1u) |> int else -999) op

        match op with
        | EXIT -> terminated <- true
        | JUMP -> programCounter <- pop ()
        | SET_STACK -> stackPointer <- pop ()
        | PUSH -> nextOp () |> push
        | GET_PC -> push programCounter
        | GET_STACK -> push stackPointer
        | LOAD -> pop () |> load |> push
        | STORE -> store (pop ()) (pop ()) // Address on top!
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
        with get () = int programCounter
        and set (value) = programCounter <- uint32 value

    member this.StackPointer
        with get () = int stackPointer
        and set (value) = stackPointer <- uint32 value

    member this.Allocated = List.rev [ for (start, arr) in arrays ->
                                       (int start, int start + Array.length arr) ]

    member this.Get location = uint32 location |> load  |> int

    member this.Stack n = [this.StackPointer - n .. this.StackPointer - 1]
                          |> Seq.map (this.Get >> int)

// ------------ Pseudo operations ------------

// Subtract the top of the stack from the element below.
let subtract = [NOT; PUSH; 1; ADD; ADD]

let addC n = [PUSH; n; ADD]

let subtractC n = addC -n

let multiplyC n = [PUSH; n; MULTIPLY]

// Push the address of the i'th stack element, counting from 0.
let addr i = [GET_STACK] @ subtractC (i + 1)

// Push a copy of the i'th element of the stack on top of the stack, counting from 0.
let copy i = addr i @ [LOAD]

// Replace the i'th element of the stack with the top of the stack (reducing its size by 1).
let replace i = addr i @ [STORE]

// Swap two stack elements.
let swap i j = copy i @ copy (j + 1) @ replace (i + 2) @ replace (j + 1)

// Remove the top n elements from the stack.
let removeTop n = [GET_STACK] @ subtractC n @ [SET_STACK]

// Jump to a relative position
let jumpRel offset = [PUSH; offset+2; GET_PC; ADD; JUMP]

let jumpRelLength = jumpRel 0 |> List.length

// Pop arg from stack and jump to one of two relative positions
// depending on whether the value is 0.
let jumpRelIf offset1 offset2 =
    List.concat [
        [IS_ZERO]
        multiplyC (offset1 - offset2)
        addC (offset2 + 2)
        [GET_PC; ADD; JUMP]
    ]

let jumpRelIfLength = jumpRelIf 0 0 |> List.length

// Repeat until 0.
// Pops top of stack after each iteration. Uses relative jump.
let doWhileNotZero block =
    let n = List.length block + jumpRelIfLength
    block @ jumpRelIf 0 -n

// The (pop and) check is done between 'before' and 'after'.
let loopUntilZero before after =
    let m = List.length after + jumpRelLength
    let n = List.length before + jumpRelIfLength + m
    List.concat [
        before
        jumpRelIf m 0
        after
        jumpRel -n
    ]

// Pop the top of the stack and check if it is 0. If not, execute block and repeat.
let whileNotZero block = loopUntilZero [] block

// Get "off the ground", starting from
// <prog> 0 <args> 0 0
// with stack counter after the last 0.
let obtainProperStack words =
    List.concat [
        [ADD; ADD] // Essentially pop the last two zeros, so that we do not cause stack overflow.
        [PUSH; words; ALLOCATE] // Allocate stack with space for 'words' 32-bit words.
        copy 0 @ [GET_STACK] @ subtractC 2 @ [STORE] // Push current argument pointer to new stack
        addC 1 @ [SET_STACK]
    ]

// Initial stack: target_start(2) source_stop(1) source_start(0)
let copyRange =
    let loop = loopUntilZero
                    (copy 1  @ copy 1 @ subtract)
                    (List.concat [
                        copy 0 @ [LOAD] @ copy 3 @ [STORE]
                        copy 2 @ addC 1 @ replace 3
                        addC 1
                    ])
    loop @ removeTop 3

// Store list of words in memory location on top of stack.
let storeLiterally data =
    let m = List.length copyRange + jumpRelLength
    let n = List.length data
    List.concat [
        [GET_PC; PUSH; 7+m+n; ADD]
        [GET_PC; PUSH; 3+m; ADD]
        copyRange
        jumpRel n
        data
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
            doWhileNotZero <| subtractC 1 @ copy 0 @ [LOAD]
            addC 1
            swap 0 1
            [OUTPUT; EXIT]
        ]
    runExample program [1; 22; 333; 4444] ""
