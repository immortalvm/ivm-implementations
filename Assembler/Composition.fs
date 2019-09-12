module Assembler.Composition

open Assembler.Target


[<Literal>]
let private ATTEMPTS_BEFORE_MONOTINICITY = 3;

// 'nops n' must return a nop sequence of at least n signed bytes.
let compose
        (nops: int -> int8 list)
        (prog: Intermediate list) : uint8 list * int[] * ((int * uint64) list) =
    let maxLabel = List.max <| 0 :: [
                       for x in prog do
                       match x with
                       | Label i -> yield i
                       | _ -> ()
                   ]
    // positions[0] will refer to the length/end of the file.
    // This is used for "linking".
    let positions = Array.create (maxLabel + 1) 0
    let spaces = Array.create (maxLabel + 1) 0UL

    let pLength = List.length prog
    let starts = Array.create pLength 0
    let codes : (int8 list)[] = Array.create pLength []

    let stable = Array.create pLength false
    let mutable allStable = false

    // (from statement, to label) -> distance
    let mutable replies = Map.empty

    let mutable attempts = 0

    while not allStable do
        let mutable position = 0

        let updateIfNecessary num inter =
            let lookup i =
                let res = positions.[i] - position
                replies <- replies.Add ((num, i), res)
                res

            starts.[num] <- position
            match inter with
            | Label i -> positions.[i] <- position
            | Spacer (i, size) ->
                if not stable.[num]
                then spaces.[i] <- uint64 <| size lookup
            | Fragment frag ->
                if not stable.[num]
                then let c = frag lookup
                     // Avoid infinite loop by enforcing monotonicity.
                     codes.[num] <- if attempts < ATTEMPTS_BEFORE_MONOTINICITY
                                    then c
                                    else let l0 = opLen codes.[num]
                                         let l1 = opLen c
                                         if l1 >= l0 then c
                                         else c @ nops (l0 - l1)
                position <- position + opLen codes.[num]

        List.iteri updateIfNecessary prog
        positions.[0] <- position // End of file

        // Check all replies (not only the recently recalculated).
        allStable <- true
        Array.fill stable 0 pLength true
        for keyValue in replies do
            let (num, label) = keyValue.Key
            if not (positions.[label] - starts.[num] = keyValue.Value)
            then stable.[num] <- false
                 allStable <- false

        attempts <- attempts + 1

    let codeList =
        Seq.concat codes |> Seq.map uint8 |> Seq.toList

    let spaceList =
        let f i size = if size = 0UL then [] else [(positions.[i], size)]
        spaces |> Array.toSeq |> Seq.mapi f |> Seq.concat |> Seq.toList

    codeList, positions, spaceList

let assemble program =
    program
    |> Seq.toList
    |> intermediates
    |> Seq.toList
    |> compose nopsFor

open Ast

let spacerAllocations (spacers: seq<int * uint64>) =
    let folder (offset: uint64) (pos: int, size: uint64) =
        let stmts =
            if size = 0UL then []
            else
                [
                    SPush <| ESum [ELoad8 (EStack <| ENum 0L); ENum <| int64 offset]
                    SPush <| ESum [ELabel 0; ENum <| int64 pos]
                    SStore8
                ]
        stmts, offset + size

    let (stmtLists, tot) = Seq.mapFold folder 0UL spacers
    if tot = 0UL then []
    else
        let prog =
            [SPush <| ENum (int64 tot); SAlloc]
            @ List.concat stmtLists
            @ [SPush (ELabel 0); SJumpZero] // Pop
        let bin, _, _ = assemble prog
        bin
