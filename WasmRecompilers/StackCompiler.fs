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
        let pos = program.Count

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

        // -----------------------

        program |> Seq.map uint32 |> Seq.toArray
