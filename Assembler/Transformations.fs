module Assembler.Transformations

open Assembler.Ast

// One of many passes
let mergePushLabelJumps (prog: Statement seq): Statement seq =
    let p = prog.GetEnumerator ()
    let mutable pending : int option = None
    let flush replacement =
        let prev = pending
        pending <- replacement
        match prev with
        | Some label -> [SPush (ELabel label)]
        | None -> []
    seq {
        while p.MoveNext() do
            match p.Current with
            | SJump None -> yield (SJump pending); pending <- None
            | SJumpZero None -> yield (SJumpZero pending); pending <- None
            | SJumpNotZero None -> yield (SJumpNotZero pending); pending <- None
            | SPush (ELabel label) -> yield! flush <| Some label
            | _ -> yield! flush None; yield p.Current

        // It would be strange to end the program with a push, though.
        yield! flush None
    }
