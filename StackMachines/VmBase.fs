module VmBase

open System

exception AccessException
exception UndefinedException

let flip f x y = f y x // Useful since arguments are popped.

[<AbstractClass>]
type BaseMachine(initialMemory: uint32 seq, input: uint32 seq, output: uint32 seq -> unit) =

    // Reverse ordering
    let mutable arrays = [ (uint32 0, Seq.toArray initialMemory) ]

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


type OpSeq = int list
let opLen (os: OpSeq) = List.length os

[<AbstractClass>]
type BaseArchitecture() =

    abstract member CreateCore: initialMemory: uint32 seq -> input: uint32 seq -> output: (uint32 seq -> unit) -> BaseMachine
    abstract member Create: program: int seq -> arguments: int seq -> input: int seq -> output: (uint32 seq -> unit) -> BaseMachine
    default a.Create program arguments input output =
        a.CreateCore (seq [program; seq [0]; arguments; seq [0; 0; 0; 0]] |> Seq.concat |> Seq.map uint32)
                     (Seq.map uint32 input)
                     output

    abstract member Exit: OpSeq
    abstract member Input: OpSeq
    abstract member Output: OpSeq
    abstract member Push: int -> OpSeq

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

    abstract member Load: OpSeq
    abstract member Store: OpSeq
    abstract member SetStack: OpSeq

    // Push address of i'th stack element, counting from 0.
    abstract member Addr: int -> OpSeq

    // Push value of i'th stack element, which becomes (i+1)th element.
    abstract member Get: int -> OpSeq
    default a.Get i = a. Addr i @ a.Load

    // Replace i'th stack element, which becomes the (i-1)th element.
    abstract member Set: int -> OpSeq
    default a.Set i = a.Addr i @ a.Store

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
    // <prog> 0 <args> 0 0 0 0
    // with stack counter after the last 0.
    default a.ObtainProperStack stackSize =
        List.concat [
            a.Add @ a.Add @ a.Add @ a.Add // Essentially pop the last four zeros, so that we do not cause stack overflow.
            a.AllocateC 256 @ a.Deallocate // Make sure we do not rely on consecutive memory
            a.AllocateC stackSize
            a.Addr 0 @ a.Get 1 @ a.Store // Push current argument pointer to new stack.
            a.AddC 1 @ a.SetStack
        ]

    // Initial stack: target_start(2) source_stop(1) source_start(0)
    member a.CopyRange: OpSeq =
        let loop = a.WhileNotZero
                       (a.Get 1  @ a.Get 1 @ a.Subtract)
                       (List.concat [
                            a.Get 0 @ a.Load @ a.Get 3 @ a.Store
                            a.Get 2 @ a.AddC 1 @ a.Set 3
                            a.AddC 1
                        ])
        loop @ a.Pop 3

    // Store data in memory (starting at location on top of stack).
    abstract member StoreLiterally: data: int list -> OpSeq

    // ------------

    member a.RunExample program arguments (inputString: string) =
        let output (s: seq<uint32>) = for x in s do printfn "Output: %d" x
        let machine = a.Create program arguments (Seq.map int inputString) output
        try machine.Run () with
        | AccessException -> printfn "Access violation!"
        | UndefinedException -> printfn "Undefined operation!"


module Examples =

    let example1 (a: BaseArchitecture) =
        let program =
            List.concat [
                a.ObtainProperStack (1<<<14)
                a.Get 0
                a.WhileNotZero (a.AddC -1 @ a.Get 0 @ a.Load) []
                a.AddC 1
                a.Swap 0 1
                a.Output
                a.Exit
            ]
        a.RunExample program [1; 22; 333; 4444] ""
