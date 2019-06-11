module VM0Compiler

open System.Collections.Generic

open Basics
open Ast
open VmBase

exception CompilationException of string

(* Calling conventions

* Before a call the following is pushed to the stack (in this order):
    - A return value place holder unless the function is void.
    - The return address (of the program counter).
    - The pointer to the memory holding the "store" (i.e. global state).
    - The arguments (for 64 bit arguments we have to decide on the order).

* The function will also store its local variables on the stack.

* When the function is done, it will store the return values and clean up the stack.
*)

(* Known at compile time:
- Memory max size (max uint32 if not specified)
- Table max size (max uint32 if not specified)
- ...
*)

(* The "store" (global state).
0. Memory start
1. Memory stop
2. Table start
3. Table stop
4... Global (i.e. module instance) variables

Questions regarding "table":

a) How do we handle references to "system functions" (e.g. imported from VM0).
Simple solution: Wrap every such function in a compiled function.

b) Is table max_size ever needed?
There does not seem to be any way for it to grow dynamically.
*)

(* We will ignore all type declarations for now. *)

[<Literal>]
let PAGE_WORDS = 32768u

let limitsToWords ({min=min; max=max}) : uint32 * (uint32 option)=
    let f x = x * PAGE_WORDS
    (f min, Option.map f max)

type SystemFunction =
    | SysInput
    | SysOutput

let allSystemFunctions = [SysInput; SysOutput] // Is there a better way?

let systemFunctionReferences imports : Map<SystemFunction, int> =
    let vmImports = imports |> Seq.map (fun imp -> imp.it)
                            |> Seq.filter (fun imp -> imp.moduleName = "VM")
                            |> Seq.map (fun imp -> imp.itemName, imp.idesc)
                            |> Map.ofSeq
    let lookup func =
        match vmImports.TryFind(string func) with
        | Some {at=_; it=FuncImport {at=_; it=i}} -> Some <| (func, int i)
        | _ -> None
    allSystemFunctions
    |> Seq.choose lookup
    |> Map.ofSeq

let vtWords valueType: int =
    match valueType with
    | I32Type | F32Type -> 1
    | _ -> 2

type Compiler (a: BaseArchitecture) =

    let compile (wasm : byte array) (stackSize : int): uint32 array =
        let wa = (Decode.decode wasm).it

        let program = new List<int>() // We'll mostly work with signed integers, for convenience.
        let (res: OpSeq -> unit) = program.AddRange
        let overwriteBefore (location: int) (ops: OpSeq) : unit =
            let f i x = program.[location - i - 1] <- x
            Seq.iteri f ops

        stackSize |> a.ObtainProperStack |> res

        let sysRefs = systemFunctionReferences wa.imports
        let (minMem, maxMem) = match wa.memories.[0].it.mtype with MemoryType lim -> limitsToWords lim
        let (minTab, maxTab) = match wa.tables.[0].it.ttype with TableType (lim, _) -> limitsToWords lim

        let typeWords i : int*int =
            let sum = List.map vtWords >> List.sum
            match wa.types.[i].it with FuncType (ins, outs) -> sum ins, sum outs

        let globalOffsets =
            wa.globals
            |> Seq.map (fun g -> match g.it.gtype with GlobalType (t, _) -> vtWords t)
            |> Seq.scan (+) 4 // Offset of first global
            |> Seq.toArray

        // Allocate space for the store
        Array.last globalOffsets |> a.AllocateC |> res

        // Allocate space for the memory
        a.AllocateC (int minMem) |> res
        a.Get 0 @ a.Get 2 @ a.Store |> res             // store[0] := memory start
        a.AddC (int minMem) @ a.Get 1 @ a.Store |> res // store[1] := memory stop

        // Keep track of the stack index holding the store pointer.
        let mutable storePointer = 0

        // Keep track of the sizes of the elements on the stack.
        let mutable stackSizes : int list = [1] // The store pointer is one word.

        let functionStarts = new System.Collections.Generic.Dictionary<int, int>()
        // (function number, reference address)
        let compilationQueue = new System.Collections.Generic.Queue<int * int>()
        let mutable isDone = false

        // -----------------------

        let pushValue (v: Value) =
            // assert (System.Bitconverter.IsLittleEndian)
            let words = match v with
                        | I32 x -> [x]
                        | F32 x -> [System.BitConverter.SingleToInt32Bits x]
                        | I64 x -> let bytes = System.BitConverter.GetBytes x
                                   [System.BitConverter.ToInt32(bytes, 0);
                                    System.BitConverter.ToInt32(bytes, 2)]
                        | F64 x -> let bytes = System.BitConverter.GetBytes x
                                   [System.BitConverter.ToInt32(bytes, 0);
                                    System.BitConverter.ToInt32(bytes, 2)]
            for w in words do
                a.Push w |> res

        let insertFunction fn =
            let f = wa.funcs.[fn].it
            // Local variables
            let localOffsets = f.locals
                               |> Seq.map vtWords
                               |> Seq.scan (+) 0
                               |> Seq.toArray
            // Make room on stack
            // Notice that this does not zero out the stack!
            let locSize = Array.last localOffsets
            -locSize |> a.Pop |> res
            storePointer <- storePointer + locSize

            for o in f.body |> Seq.map (fun o -> o.it) do
                match o with
                | Unreachable -> raise <| CompilationException "Unreachable"
                | Nop -> ()

                // TODO: What if the value on top of the stack uses two words?
                // -> We need to keep track of the types of the elements on the stack!
                | Drop -> a.Pop 1 |> res

                | Select -> ()
                | Block (st, lst) -> ()
                | Loop (st, lst) -> ()
                | If (st, lst1, lst2) -> ()
                | Br {at=_; it=v} -> ()
                | BrIf {at=_; it=v} -> ()
                | BrTable (vl, v) -> ()
                | Return -> ()
                | Call {at=_; it=v} -> ()
                | CallIndirect {at=_; it=v} -> ()
                | LocalGet {at=_; it=v} -> ()
                | LocalSet {at=_; it=v} -> ()
                | LocalTee {at=_; it=v} -> ()
                | GlobalGet {at=_; it=v} -> ()
                | GlobalSet {at=_; it=v} -> ()
                | Load loadop -> ()
                | Store storeop -> ()
                | MemorySize -> ()
                | MemoryGrow -> ()
                | Const {at=_; it=v} -> pushValue v
                | Test testop -> ()
                | Compare relop -> ()
                | Unary unop -> ()
                | Binary binop -> ()
                | Convert cvop -> ()




            // TODO
            res <| []

        // TODO: Treat main method differently?
        insertFunction 0 // Insert main method

        while not isDone do
            let (more, next) = compilationQueue.TryDequeue()
            if not more
            then
                isDone <- true
            else
                let (fn, loc) = next
                let mutable addr = 0
                if not <| functionStarts.TryGetValue(fn, ref addr)
                then addr <- program.Count
                     functionStarts.[fn] <- addr
                     insertFunction fn
                overwriteBefore loc <| a.Jump (addr - loc)


        // -----------------------

        program |> Seq.map uint32 |> Seq.toArray
