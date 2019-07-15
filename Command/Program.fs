open Assembler.Checker

let usage () =
    let ex = System.AppDomain.CurrentDomain.FriendlyName
    printfn "Usage:"
    printfn "  %s                                 -  Show this text" ex
    printfn "  %s as <source> <binary> <symbols>  -  Assemble" ex
    printfn "  %s run <binary>                    -  Run binary and print final stack" ex
    printfn "  %s trace <binary>                  -  Trace binary" ex
    printfn "  %s as-run <source>                 -  Assemble and run (no output files)" ex
    printfn "  %s as-trace <source>               -  Assemble and trace (no output files)" ex
    printfn "  %s check <source>                  -  Assemble, run, and check final stack" ex

let assem source binary symbols =
    let bytes, symbolList = doAssemble source
    System.IO.File.WriteAllBytes (binary, bytes |> List.toArray)
    printfn "Binary written to: %s" binary

    let syms = seq { for (name, pos) in symbolList -> sprintf "%s %d" name pos }
    System.IO.File.WriteAllLines (symbols, Seq.append ["--Relative--"] syms)
    printfn "Symbols written to: %s" symbols

let writeStack (endStack: seq<int64>) =
    printfn "End stack:"
    for x in endStack do
        printfn "0x..%05X %7d" (uint64 x &&& 0xfffffUL) x

let run fileName shouldTrace =
    let bytes = System.IO.File.ReadAllBytes fileName
    let stack = doRun bytes shouldTrace |> Seq.toList
    if not shouldTrace then writeStack stack

let asRun source shouldTrace =
    let bytes = doAssemble source |> fst |> List.toArray
    let stack = doRun bytes shouldTrace |> Seq.toList
    if not shouldTrace then writeStack stack

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
            | "as" when n = 4 -> assem argv.[1] argv.[2] argv.[3]; 0
            | "run" when n = 2 -> run argv.[1] false; 0
            | "trace" when n = 2 -> run argv.[1] true; 0
            | "as-run" when n = 2 -> asRun argv.[1] false; 0
            | "as-trace" when n = 2 -> asRun argv.[1] true; 0
            | "check" when n = 2 -> check argv.[1]; 0
            | _ -> usage (); 1
    with
        Failure msg -> printfn "%s" msg; 1
