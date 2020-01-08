module Assembler.ConnectedComponents

// Loosely based on Tarjan's algorithm
// The list of components is topologically sorted;
// and each component is ordered according to a depth first search.
let findComponents(start: string, next: string -> seq<string>): string list list =
    let mutable indexAndLowLink = new Map<string, int * int>([])
    let mutable indexCounter = 0

    let mutable stack: string list = []
    let mutable result: string list list = []

    let rec connect(v: string): int =
        let index = indexCounter
        indexCounter <- indexCounter + 1
        let mutable lowLink = index + 1
        let lower x =
            if x < lowLink then
                lowLink <- x
                indexAndLowLink <- indexAndLowLink.Add (v, (index, lowLink))
        lower index
        stack <- v :: stack

        for w in next v do
            match indexAndLowLink.TryFind w with
            | None -> connect w |> lower
            | Some (ind, low) ->
                if List.contains w stack
                then lower ind

        if lowLink = index then
            let i = List.findIndex ((=) v) stack
            result <- stack.[0..i] :: result
            stack <- stack.[i+1..]

        lowLink

    connect start |> ignore
    result
