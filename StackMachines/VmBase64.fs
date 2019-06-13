module VmBase64

open System

exception AccessException
exception UndefinedException

[<Literal>]
let DEBUG = false

let flip f x y = f y x // Useful since arguments are popped.

// n=1: From 8 to 64 bits
// n=2: From 16 to 64 bits
// n=4: From 32 to 64 bits
// n=8: No effect
let signExtend n x =
    let s = 1UL <<< (n*8 - 1)
    (x &&& (s - 1UL)) ||| uint64 -(int64 (x &&& s))

// Little-endian encoding, n = 1,2,4,8
let fromBytes bytes = bytes |> Seq.mapi (fun i x -> uint64 x <<< i*8) |> Seq.sum
let toBytes n x = [|0..n-1|] |> Seq.map (fun i -> x >>> i*8 |> byte)

[<AbstractClass>]
type BaseMachine(initialMemory: byte seq, input: byte seq, output: byte seq -> unit) =

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

    // Stack now grows downwards!
    member val StackPointer = nextUnused with get, set // stack pointer (next location)

    // Little-endian, n = 1,2,4,8, not sign-extended
    member m.LoadN n location =
        [| location .. location + uint64(n-1) |]
        |> Seq.map load
        |> fromBytes

    // Little-endian, n = 1,2,4,8
    member m.StoreN n location (value: uint64) =
        toBytes n value
        |> Seq.iteri (fun i x -> store (location + uint64 i) x)

    member m.NextOp n =
        let op = m.LoadN n m.ProgramCounter
        m.ProgramCounter <- m.ProgramCounter + uint64 n
        op

    // Only 64-bit values on stack!
    member m.Pop () =
        let result = m.LoadN 8 m.StackPointer
        m.StackPointer <- m.StackPointer + 8UL
        result

    member m.Push value =
        m.StackPointer <- m.StackPointer - 8UL
        m.StoreN 8 m.StackPointer value

    member m.Output start stop =
        Seq.map load { start .. stop - 1UL} |> output

    member m.Input start stop =
        let mutable counter = 0
        for (i, data) in Seq.zip {start .. stop - 1UL} input do
            counter <- counter + 1
            store i data
        start + uint64 counter

    abstract member Step: unit -> unit

    member m.Run () =
        if DEBUG
        then printfn "Initial memory"
             Seq.iteri (fun i x -> printfn "%2d: %2X %3d" i x x) initialMemory

        while not m.Terminated do

            if DEBUG
            then printfn "Stack (%2d:%2d): %s"
                 <| m.ProgramCounter
                 <| m.LoadN 1 m.ProgramCounter
                 <| String.Join(", ", m.Stack 20)

            m.Step ()


    // For testing:

    member m.Allocated = List.rev [ for (start, arr) in arrays ->
                                    (int start, int start + Array.length arr) ]

    member m.Stack n : int seq =
        let safeLoad addr =
            try
                m.LoadN 8 addr |> Some
            with
                | AccessException -> None
        [0 .. n-1]
        |> Seq.map (fun i -> m.StackPointer + uint64 (i*8) |> safeLoad)
        |> Seq.takeWhile Option.isSome
        |> Seq.map (Option.get >> int)
        |> Seq.rev


type OpSeq = int list
let opLen (os: OpSeq) = List.length os

[<AbstractClass>]
type BaseArchitecture() =

    abstract member CreateCore: initialMemory: byte seq -> input: byte seq -> output: (byte seq -> unit) -> BaseMachine
    abstract member Create: program: int seq -> arguments: int seq -> input: int seq -> output: (byte seq -> unit) -> BaseMachine
    default a.Create program arguments input output =
        a.CreateCore (seq [program; seq [0]; arguments; Seq.replicate (4*8) 0] |> Seq.concat |> Seq.map byte)
                     (Seq.map byte input)
                     output

    abstract member Exit: OpSeq
    abstract member Input: OpSeq
    abstract member Output: OpSeq
    abstract member Push: int -> OpSeq // Push signed 32-bit integer

    abstract member Allocate: OpSeq
    abstract member AllocateC: int -> OpSeq
    default a.AllocateC n = a.Push n @ a.Allocate

    abstract member Deallocate: OpSeq

    abstract member Add: OpSeq

    (* Add constant. For subtraction, just add a negative number. *)
    abstract member AddC: int -> OpSeq
    default a.AddC n = a.Push n @ a.Add

    (* Flip all bits*)
    abstract member Not: OpSeq
    abstract member Subtract: OpSeq
    default a.Subtract = a.Not @ a.AddC 1 @ a.Add

    abstract member Multiply: OpSeq
    abstract member MultiplyC: int -> OpSeq
    default a.MultiplyC n = a.Push n @ a.Multiply

    abstract member SignN: int -> OpSeq
    abstract member LoadUN: int -> OpSeq
    abstract member LoadSN: int -> OpSeq
    default a.LoadSN n =
        let u = a.LoadUN n
        if n = 8 then u
        else u @ a.SignN n

    abstract member StoreN: int -> OpSeq
    abstract member SetStack: OpSeq

    // Push address of i'th stack element, counting from 0.
    abstract member Addr: int -> OpSeq

    // Push value of i'th stack element, which becomes (i+1)th element.
    abstract member Get: int -> OpSeq
    default a.Get i = a. Addr i @ a.LoadUN 8

    // Replace i'th stack element, which becomes the (i-1)th element.
    abstract member Set: int -> OpSeq
    default a.Set i = a.Addr i @ a.StoreN 8

    // Swap two stack elements.
    abstract member Swap: int -> int -> OpSeq
    default a.Swap i j = a.Get i @ a.Get (j + 1) @ a.Set (i + 2) @ a.Set (j + 1)

    // Pop n elements from stack.
    abstract member Pop: int -> OpSeq
    default a.Pop n = a.Addr (n-1) @ a.SetStack

    // Jump to relative address.
    abstract member Jump: int -> OpSeq
    member a.JumpL = a.Jump 0 |> opLen

    abstract member JumpIfZero: int -> OpSeq
    member a.JumpIfZeroL = a.JumpIfZero 0 |> opLen

    abstract member JumpIfNotZero: int -> OpSeq
    member a.JumpIfNotZeroL = a.JumpIfNotZero 0 |> opLen

    abstract member WhileNotZero: beforeCheck:OpSeq -> afterCheck:OpSeq -> OpSeq
    default a.WhileNotZero beforeCheck afterCheck =
        if List.isEmpty afterCheck
        then
            // Minor optimization
            beforeCheck @ a.JumpIfNotZero -(opLen beforeCheck + a.JumpIfNotZeroL)
        else
            let m = opLen afterCheck + a.JumpL
            let n = opLen beforeCheck + a.JumpIfZeroL + m
            List.concat [
                beforeCheck
                a.JumpIfZero m
                afterCheck
                a.Jump -n
            ]

    // Insert "breaks" between consecutive sequences.
    member a.Loop (blocks: OpSeq list) : OpSeq =
        let n = List.length blocks
        let positions = blocks |> Seq.scan (fun pos b -> pos + opLen b + a.JumpL) 0 |> Seq.toList
        let dest i = if i < n-1 then positions.[n] - positions.[i] else -positions.[n]
        let blockAndJump i b = b @ a.Jump (dest i)
        blocks |> Seq.mapi blockAndJump |> List.concat

    // In order to get "off the ground".
    abstract member ObtainProperStack: stackSize:int -> OpSeq
    // Get "off the ground", starting from
    // <prog> 0y <args> 0L 0L 0L 0L
    // with stack pointer after the last 0.
    default a.ObtainProperStack stackSize =
        List.concat [
            a.AllocateC 256 @ a.Deallocate // Make sure we do not rely on consecutive memory
            a.AllocateC stackSize // Top: Start of allocated memory
            a.Push (stackSize-8) @ a.Add // Top: End of allocated memory minus 8. This will be the new SP.
            a.Addr -3 @ a.Get 1 @ a.StoreN 8 // Push current argument pointer to new stack.
            a.SetStack
        ]

    // Initial stack (top first): source_start(0) source_stop(1) target_start(2)
    member a.CopyRange: OpSeq =
        let loop = a.WhileNotZero
                       (a.Get 1  @ a.Get 1 @ a.Subtract)
                       (List.concat [
                            a.Get 0 @ a.LoadUN 1 @ a.Get 3 @ a.StoreN 1
                            a.Get 2 @ a.AddC 1 @ a.Set 3
                            a.AddC 1
                        ])
        loop @ a.Pop 3

    // Store data in memory (starting at location on top of stack).
    abstract member StoreLiterally: data: int list -> OpSeq

    // ------------

    member a.RunExample program arguments (inputString: string) =
        let output (s: byte seq) = for x in s do printfn "Output: %d" x
        let machine = a.Create program arguments (Seq.map int inputString) output
        try machine.Run () with
        | AccessException -> printfn "Access violation!"
        | UndefinedException -> printfn "Undefined operation!"


module Examples =

    let example1 (a: BaseArchitecture) =
        let program =
            List.concat [
                a.ObtainProperStack (1<<<16) // 64 KiB
                a.Get 0
                a.WhileNotZero (a.AddC -1 @ a.Get 0 @ a.LoadUN 1) []
                a.AddC 1
                a.Swap 0 1
                a.Output
                a.Exit
            ]
        a.RunExample program [1; 22; 111; 222] ""
