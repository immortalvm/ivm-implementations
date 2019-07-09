module Assembler.Composition


[<Literal>]
let private ATTEMPTS_BEFORE_MONOTINICITY = 3;

type Intermediate =
    | Label of int
    // Code given label -> relative position (from statement _start_)
    | Fragment of ((int -> int) -> byte list)


// 'nops n' must return a nop sequence of at least n bytes.
let compose (prog: Intermediate list) (nops: int -> byte list): seq<byte> * int[] =
    let maxLabel = List.max [
                       for x in prog do
                       match x with
                       | Label i -> yield i
                       | _ -> ()
                   ]
    // positions[0] will refer to the beginning of the file.
    // This will be useful for absolute adressing (of initial program).
    let positions = Array.create (maxLabel + 1) 0

    let pLength = List.length prog
    let starts = Array.create pLength 0
    let codes = Array.create pLength []

    let stable = Array.create pLength false
    let mutable allStable = false

    // (from statement, to label) -> distance
    let mutable replies = new Map<int * int, int>([])

    let mutable attempts = 0

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
                if not stable.[num]
                then let c = frag lookup
                     // Avoid infinite loop by enforcing monotonicity.
                     codes.[num] <- if attempts < ATTEMPTS_BEFORE_MONOTINICITY
                                    then c
                                    else let l0 = List.length codes.[num]
                                         let l1 = List.length c
                                         if l1 >= l0 then c
                                         else c @ nops (l0 - l1)
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

        attempts <- attempts + 1

    (Seq.concat codes, positions)
