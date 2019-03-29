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
let AND = 14

[<Literal>]
let NOT = 15

[<Literal>]
let IS_ZERO = 16


type Machine(program, input, output) =

    inherit BaseMachine(program, input, output)

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
        | AND -> m.Pop () &&& m.Pop () |> m.Push
        | NOT -> ~~~ (m.Pop ()) |> m.Push
        | IS_ZERO -> (if m.Pop() = 0u then 1 else 0) |> uint32 |> m.Push
        | _ -> raise UndefinedException



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
let switchJump offset1 offset2 =
    List.concat [
        [IS_ZERO]
        multiplyC (offset1 - offset2)
        addC (offset2 + 2)
        [GET_PC; ADD; JUMP]
    ]

let switchJumpLength = switchJump 0 0 |> List.length

// Repeat until 0.
// Pops top of stack after each iteration. Uses relative jump.
let doWhileNotZero block =
    let n = List.length block + switchJumpLength
    block @ switchJump 0 -n

// The (pop and) check is done between 'before' and 'after'.
let loopUntilZero before after =
    let m = List.length after + jumpRelLength
    let n = List.length before + switchJumpLength + m
    List.concat [
        before
        switchJump m 0
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
        copy 0 @ [GET_STACK] @ subtractC 2 @ [STORE] // Push current argument pointer to new stack.
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

// Store list of words in memory (starting at location on top of stack).
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
