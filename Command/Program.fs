open Assembler.Checker

let usage () =
    let ex = System.AppDomain.CurrentDomain.FriendlyName
    printfn "Usage:"
    printfn "  %s                       -  Show this text" ex
    printfn "  %s as <source> <binary>  -  Assemble" ex
    printfn "  %s run <binary>          -  Run binary and print final stack" ex
    printfn "  %s trace <binary>        -  Trace binary and print final stack" ex
    printfn "  %s as-run <source>       -  Assemble and run (no output file)" ex
    printfn "  %s as-trace <source>     -  Assemble and trace (no output file)" ex
    printfn "  %s check <source>        -  Assemble, run, and check final stack" ex

let assem source binary =
    let bytes = doAssemble source |> List.toArray
    System.IO.File.WriteAllBytes (binary, bytes)
    printfn "Output written to: %s" binary

let writeStack (endStack: seq<int64>) =
    printfn "End stack:"
    for x in endStack do
        printfn "%s" <| showValue x

let run fileName shouldTrace =
    let bytes = System.IO.File.ReadAllBytes fileName
    let stack = doRun bytes shouldTrace |> Seq.toList
    if shouldTrace then printfn ""
    writeStack stack

let asRun source shouldTrace =
    let bytes = doAssemble source |> List.toArray
    let stack = doRun bytes shouldTrace |> Seq.toList
    if shouldTrace then printfn ""
    writeStack stack

let check source =
    printfn "%s" <| doCheck source

[<EntryPoint>]
let main argv =
    try
        printfn "iVM Assembler and VM, version 0.1"
        let n = Array.length argv
        if n = 0 then usage (); 0
        else
            match argv.[0] with
            | "as" when n = 3 -> assem argv.[1] argv.[2]; 0
            | "run" when n = 2 -> run argv.[1] false; 0
            | "trace" when n = 2 -> run argv.[1] true; 0
            | "as-run" when n = 2 -> asRun argv.[1] false; 0
            | "as-trace" when n = 2 -> asRun argv.[1] true; 0
            | "check" when n = 2 -> check argv.[1]; 0
            | _ -> usage (); -1
    with
        Failure msg -> printfn "%s" msg; -1
