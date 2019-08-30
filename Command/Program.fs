open System.IO

open Machine.Utils
open Assembler.Checker

[<Literal>]
let VERSION = "0.9" // How can we automatically get this from Git?

[<Literal>]
let LABELS_HEADING = "--Labels--"

let usage () =
    let ex = System.AppDomain.CurrentDomain.FriendlyName
    printfn "Usage:"
    printfn "  %s                                                  -  Show this text" ex
    printfn "  %s as <source> <binary> <symbols>                   -  Assemble" ex
    printfn "  %s run <binary>                                     -  Run binary and print final stack" ex
    printfn "  %s run <binary> <arg file>                          -  Run binary and print final stack" ex
    printfn "  %s run <binary> <arg file> <output dir>             -  Run binary and print final stack" ex
    printfn "  %s trace <binary> <symbols>                         -  Trace binary" ex
    printfn "  %s trace <binary> <symbols> <arg file>              -  Trace binary" ex
    printfn "  %s trace <binary> <symbols> <arg file> <output dir> -  Trace binary" ex
    printfn ""
    printfn "  %s as-run <source>                                  -  Assemble and run" ex
    printfn "  %s as-run <source> <arg file>                       -  Assemble and run" ex
    printfn "  %s as-run <source> <arg file> <output dir>          -  Assemble and run" ex
    printfn "  %s as-trace <source>                                -  Assemble and trace" ex
    printfn "  %s as-trace <source> <arg file>                     -  Assemble and trace" ex
    printfn "  %s as-trace <source> <arg file> <output dir>        -  Assemble and trace" ex
    printfn "  %s check <source>                                   -  Assemble, run, and check final stack" ex
    printfn ""
    printfn "  %s gen-proj <root dir> <goal>     -  Create prototype project (<goal>.proj)" ex
    // No incremental builds for now
    printfn "  %s build <project> <dest dir>     -  Assemble project" ex

let writeAssemblerOutput binaryFile symbolsFile bytes exported labels previous =
    File.WriteAllBytes (binaryFile, bytes |> List.toArray)
    printfn "Binary written to: %s" binaryFile
    let mapLines map = seq {
        for name, pos in Seq.sortBy fst map do
            yield sprintf "%s\t%d" name pos
    }
    let lines = seq {
        yield "--Previous--"
        yield valueOr "" previous
        yield "--Size--"
        yield bytes.Length.ToString ()
        yield "--Relative--"
        yield! mapLines exported
        yield LABELS_HEADING
        yield! mapLines labels
    }
    File.WriteAllLines (symbolsFile, lines)
    printfn "Symbols written to: %s" symbolsFile

let assem source binary symbols =
    let _, bytes, exported, labels = doAssemble source
    writeAssemblerOutput source binary bytes exported labels None

let readTraceSyms symbols =
    let notHeading line = line <> LABELS_HEADING
    let lines = Seq.skipWhile notHeading <| File.ReadLines symbols
    let readLine (line: string) = let arr = line.Split '\t'
                                  int arr.[1], arr.[0]
    new Map<int, string>(Seq.map readLine <| Seq.skip 1 lines)

let writeStack (endStack: seq<int64>) =
    printfn "End stack:"
    for x in endStack do
        printfn "0x..%05X %7d" (uint64 x &&& 0xfffffUL) x

let run binary (argFile: string option) (outputDir: string option) (symbolsIfShouldTrace: string option) =
    let bytes = File.ReadAllBytes binary
    let arg = match argFile with
              | Some name -> File.ReadAllBytes name
              | None -> Array.empty
    let traceSyms =
        match symbolsIfShouldTrace with
        | Some name -> Some <| readTraceSyms name
        | None -> None
    let stack = doRun bytes arg outputDir traceSyms |> Seq.toList
    if symbolsIfShouldTrace.IsNone then writeStack stack

let asRun source argFile outputDir shouldTrace =
    let _, bytes, exported, labels = doAssemble source

    if shouldTrace then
        for name, pos in Seq.sortBy fst exported do
            printfn "%20s %6d" name pos

    let traceSyms =
        if not shouldTrace then None
        else let flip (x, y) = (y, x)
             Some <| new Map<int, string> (Seq.map flip labels)

    let arg = match argFile with
              | Some name -> File.ReadAllBytes name
              | None -> Array.empty
    let stack = doRun (List.toArray bytes) arg outputDir traceSyms |> Seq.toList
    if not shouldTrace then writeStack stack

let check source =
    printfn "%s" <| doCheck source

let nodePath rootDir (node: string) extension =
    (Array.append [|rootDir|] (node.Split '.') |> Path.Combine) + extension

let genProj rootDir (goal: string) =
    let projectFile = goal + ".proj"
    if File.Exists projectFile
    then failwithf "File exists: %s" projectFile
    let lines = seq {
        yield "--Root--"
        yield Path.GetFullPath rootDir
        yield "--Relative--"
        yield! getBuildOrder rootDir goal
    }
    File.WriteAllLines (projectFile, lines)
    printfn "Project file created: %s" projectFile

let build projectFile destinationDir =
    let mutable lines = seq <| File.ReadAllLines projectFile
    let goto x = Seq.skip (1 + Seq.findIndex ((=) x) lines) lines
    let write node =
        let bFile = nodePath destinationDir node BINARY_EXTENSION
        let sFile = nodePath destinationDir node SYMBOLS_EXTENSION
        writeAssemblerOutput bFile sFile

    lines <- goto "--Root--"
    let rootDir = Seq.head lines
    lines <- Seq.skip 1 lines
    lines <- goto "--Relative--"
    let buildOrder = Seq.toList lines

    let tuples = doBuild rootDir buildOrder
    let withSaving = seq {
        let mutable previous = projectFile
        for node, bin, sym, lab in tuples do
            write node bin sym lab <| Some previous
            yield node, bin, sym, lab
            previous <- node
    }
    // This triggers the saving of individual binaries as a side effect.
    let node, bin, sym, lab = doCollect withSaving
    write (node + "$") bin sym lab None

[<EntryPoint>]
let main argv =
    try
        printfn "iVM Assembler and VM, version %s" VERSION
        let n = Array.length argv
        if n = 0 then usage (); 0
        else
            match argv.[0] with
            | "as" when n = 4 -> assem argv.[1] argv.[2] argv.[3]; 0
            | "run" when n = 2 -> run argv.[1] None None None; 0
            | "run" when n = 3 -> run argv.[1] (Some argv.[2]) None None; 0
            | "run" when n = 4 -> run argv.[1] (Some argv.[2]) (Some argv.[3]) None; 0
            | "trace" when n = 3 -> run argv.[1] None None (Some argv.[2]); 0
            | "trace" when n = 4 -> run argv.[1] (Some argv.[2]) None (Some argv.[3]); 0
            | "trace" when n = 5 -> run argv.[1] (Some argv.[2]) (Some argv.[4]) (Some argv.[3]); 0

            | "as-run" when n = 2 -> asRun argv.[1] None None false; 0
            | "as-run" when n = 3 -> asRun argv.[1] (Some argv.[2]) None false; 0
            | "as-run" when n = 4 -> asRun argv.[1] (Some argv.[2]) (Some argv.[3]) false; 0
            | "as-trace" when n = 2 -> asRun argv.[1] None None true; 0
            | "as-trace" when n = 3 -> asRun argv.[1] (Some argv.[2]) None true; 0
            | "as-trace" when n = 4 -> asRun argv.[1] (Some argv.[2]) (Some argv.[3]) true; 0

            | "check" when n = 2 -> check argv.[1]; 0
            | "gen-proj" when n = 3 -> genProj argv.[1] argv.[2]; 0
            | "build" when n = 3 -> build argv.[1] argv.[2]; 0
            | _ -> usage (); 1
    with
        Failure msg -> printfn "%s" msg; 1
