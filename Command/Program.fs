open Assembler.Checker

[<Literal>]
let LABELS_HEADING = "--Labels--"

let usage () =
    let ex = System.AppDomain.CurrentDomain.FriendlyName
    printfn "Usage:"
    printfn "  %s                                 -  Show this text" ex
    printfn "  %s as <source> <binary> <symbols>  -  Assemble" ex
    printfn "  %s run <binary>                    -  Run binary and print final stack" ex
    printfn "  %s trace <binary> <symbols>        -  Trace binary" ex
    printfn "  %s as-run <source>                 -  Assemble and run (no output files)" ex
    printfn "  %s as-trace <source>               -  Assemble and trace (no output files)" ex
    printfn "  %s check <source>                  -  Assemble, run, and check final stack" ex

let assem source binary symbols =
    let bytes, exported, labels = doAssemble source
    System.IO.File.WriteAllBytes (binary, bytes |> List.toArray)
    printfn "Binary written to: %s" binary
    let mapLines map = seq {
        for name, pos in Seq.sortBy fst map do
            yield sprintf "%s\t%d" name pos
    }
    let lines = seq {
        yield "--Size--"
        yield bytes.Length.ToString ()
        yield "--Relative--"
        yield! mapLines exported
        yield LABELS_HEADING
        yield! mapLines labels
    }
    System.IO.File.WriteAllLines (symbols, lines)
    printfn "Symbols written to: %s" symbols

let readTraceSyms symbols =
    let notHeading line = line <> LABELS_HEADING
    let lines = Seq.skipWhile notHeading <| System.IO.File.ReadLines symbols
    let readLine (line: string) = let arr = line.Split '\t'
                                  int arr.[1], arr.[0]
    new Map<int, string>(Seq.map readLine <| Seq.skip 1 lines)

let writeStack (endStack: seq<int64>) =
    printfn "End stack:"
    for x in endStack do
        printfn "0x..%05X %7d" (uint64 x &&& 0xfffffUL) x

let run binary (symbolsIfShouldTrace: string option) =
    let bytes = System.IO.File.ReadAllBytes binary
    let traceSyms =
        match symbolsIfShouldTrace with
        | Some name -> Some <| readTraceSyms name
        | None -> None
    let stack = doRun bytes traceSyms |> Seq.toList
    if symbolsIfShouldTrace.IsNone then writeStack stack

let asRun source shouldTrace =
    let bytes, exported, labels = doAssemble source

    if shouldTrace then
        for name, pos in Seq.sortBy fst exported do
            printfn "%20s %6d" name pos

    let traceSyms =
        if not shouldTrace then None
        else let flip (x, y) = (y, x)
             Some <| new Map<int, string> (Seq.map flip labels)

    let stack = doRun (List.toArray bytes) traceSyms |> Seq.toList
    if not shouldTrace then writeStack stack

let check source =
    printfn "%s" <| doCheck source

[<EntryPoint>]
let main argv =
    try
        printfn "iVM Assembler and VM, version 0.4"
        let n = Array.length argv
        if n = 0 then usage (); 0
        else
            match argv.[0] with
            | "as" when n = 4 -> assem argv.[1] argv.[2] argv.[3]; 0
            | "run" when n = 2 -> run argv.[1] None; 0
            | "trace" when n = 3 -> run argv.[1] (Some argv.[2]); 0
            | "as-run" when n = 2 -> asRun argv.[1] false; 0
            | "as-trace" when n = 2 -> asRun argv.[1] true; 0
            | "check" when n = 2 -> check argv.[1]; 0
            | _ -> usage (); 1
    with
        Failure msg -> printfn "%s" msg; 1
