module Assembler.Composition


type Intermediate =
    | Label of int
    // Code given label -> relative position (from statement _start_)
    | Fragment of ((int -> int) -> byte list)


let compose (prog: Intermediate list): seq<byte> * int[] =
    let maxLabel = List.max [
        for x in prog do
            match x with
            | Label i -> yield i
            | _ -> ()
    ]
    // positions[0] will not be used
    let positions = Array.create (maxLabel + 1) 0

    let pLength = List.length prog
    let starts = Array.create pLength 0
    let codes = Array.create pLength []

    let stable = Array.create pLength false
    let mutable allStable = false

    // (from statement, to label) -> distance
    let mutable replies = new Map<int * int, int>([])

    // TODO: Currently there probably is a chance of an infinite loop.
    while not allStable do
        let mutable position = 0

        let updateIfNecessary num inter =
            let lookup i =
                let res = positions.[i] - position
                replies <- replies.Add ((num, i), res)
                res

            match inter with
            | Label i -> positions.[i] <- position
            | Fragment frag ->
                starts.[num] <- position
                if not stable.[num] then codes.[num] <- frag lookup
                position <- position + List.length codes.[num]

        List.iteri updateIfNecessary prog

        // Check all replies (not only the recently recalculated).
        allStable <- true
        Array.fill stable 0 pLength true
        for keyValue in replies do
            let (num, label) = keyValue.Key
            if not (positions.[label] - starts.[num] = keyValue.Value)
            then stable.[num] <- false
                 allStable <- false

    (Seq.concat codes, positions)
