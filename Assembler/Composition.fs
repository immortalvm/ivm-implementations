module Assembler.Composition

open Assembler.Target
open Machine.Executor

[<Literal>]
let private ATTEMPTS_BEFORE_MONOTINICITY = 3;

// 'nops n' must return a nop sequence of at least n signed bytes.
let compose
        (nops: int -> int8 list)
        (prog: Intermediate list) : uint8 list * int[] * ((int * uint64) list) * (int list) =
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

    let relativize = Array.create pLength false

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
            | Relative -> relativize.[num] <- true
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

    let relativesList =
        let f num x = if x then [starts.[num]] else []
        relativize |> Array.toSeq |> Seq.mapi f |> Seq.concat |> Seq.toList

    codeList, positions, spaceList, relativesList

let assemble program =
    program
    |> Seq.toList
    |> intermediates
    |> Seq.toList
    |> compose nopsFor

open Ast

let rec deltas lst =
    match lst with
    | x :: ((y :: _) as rest) -> y - x :: deltas rest
    | _ -> []

let initialization (stackSize: int) (spacers: (int * uint64) list) (relatives: int list) =
    // Labels
    let main = 0
    let memoryPointer = 1
    let codePointer = 2
    let spaceLoop = 3
    let relLoop = 4

    let copy = int64 >> ENum >> EStack >> ELoad8 >> SPush
    let sumSpace = Seq.map snd spacers |> Seq.sum |> int64

    let statements =
        seq {
            yield! [
                // Store current SP - initialStackSize (i.e. end of "argument")
                SPush <| ESum [ENum 0L |> EStack; int64 -initialStackSize |> ENum]
                SPush <| ESum [ELabel main; ENum -8L]
                SStore8

                // Allocate
                SPush <| ENum (int64 stackSize + sumSpace)
                copy 0; SAlloc
            ]
            if sumSpace <> 0L then
                yield! [
                    copy 0
                    SPush <| ELabel memoryPointer
                    SStore8
                ]
            yield! [
                SAdd; SSetSp
            ]

            yield SPush <| ENum 0L // Marker

            if sumSpace <> 0L then
                for (d, s) in Seq.zip (deltas (0 :: (List.map fst spacers))) (Seq.map snd spacers) |> Seq.rev do
                    yield! [
                        SPush <| ENum (int64 s)
                        SPush <| ENum (int64 d)
                    ]
                yield! [
                    SPush <| ELabel main
                    SPush <| ELabel codePointer
                    SStore8
                    SLabel spaceLoop

                    // Adjust code pointer (consuming d)
                    SPush <| ELoad8 (ELabel codePointer); SAdd
                    SPush <| ELabel codePointer; SStore8

                    // Update code
                    SPush <| ELoad8 (ELabel memoryPointer)
                    SPush <| ELoad8 (ELabel codePointer)
                    SStore8

                    // Adjust memory pointer (consuming s)
                    SPush <| ELoad8 (ELabel memoryPointer); SAdd
                    SPush <| ELabel memoryPointer; SStore8
                ]
                if spacers.Length > 1 then
                    yield! [
                        copy 0; SPush <| ELabel spaceLoop; SJumpNotZero
                    ]

            if relatives <> [] then
                for d in deltas (0 :: relatives) |> Seq.rev do
                    yield SPush <| ENum (int64 d)
                yield! [
                    SPush <| ELabel main
                    SLabel relLoop
                    SAdd
                    copy 0; copy 0; SLoad8; SAdd
                    copy 1; SStore8
                ]
                if relatives.Length > 1 then
                    yield! [
                        copy 1; SPush <| ELabel relLoop; SJumpNotZero
                    ]
                yield! [
                    SPush <| EStack (ENum 1L); SSetSp // Pop
                ]

            yield! [
                // Pop 0 marker and jump to "main".
                ELabel main |> SPush
                SJumpZero
            ]

            if sumSpace <> 0L then
                yield! [
                    SLabel codePointer; SData8 (0L, ENum 0L)
                    SLabel memoryPointer; SData8 (0L, ENum 0L)]

            yield! [
                // ArgStop at main - 8:
                SData8 (0L, ENum 0L)
            ]

        }

    let bin, _, _, _ = assemble statements
    bin
