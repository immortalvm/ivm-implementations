open System.IO

open Machine.Utils
open Assembler.Checker

[<Literal>]
let VERSION = "0.9" // How can we automatically get this from Git?


// Symbols file headings

[<Literal>]
let PREVIOUS_HEADING = "--Previous--"

[<Literal>]
let SIZE_HEADING = "--Size--"

[<Literal>]
let RELATIVE_HEADING = "--Relative--"

[<Literal>]
let LABELS_HEADING = "--Labels--"

[<Literal>]
let SPACERS_HEADING = "--Spacers--"


// Project file headings

[<Literal>]
let ROOT_HEADING = "--Root--"

[<Literal>]
let RELATIVE_ORDER_HEADING = "--Relative--"


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
    printfn "  %s build <project> <dest dir>     -  Assemble project" ex
    printfn "  %s make <project> <dest dir>      -  Assemble project incrementally" ex

let writeAssemblerOutput binaryFile symbolsFile bytes exported labels spacers previous =
    File.WriteAllBytes (binaryFile, bytes |> Seq.toArray)
    printfn "Binary written to: %s" binaryFile
    let mapLines map = seq {
        for name, pos in Seq.sortBy fst map do
            yield sprintf "%O\t%O" name pos
    }
    let lines = seq {
        yield PREVIOUS_HEADING
        yield previous
        yield SIZE_HEADING
        yield Seq.length bytes |> string
        yield RELATIVE_HEADING
        yield! mapLines exported
        yield LABELS_HEADING
        yield! mapLines labels
        yield SPACERS_HEADING
        yield! mapLines spacers
    }
    File.WriteAllLines (symbolsFile, lines)
    printfn "Symbols written to: %s" symbolsFile

let splitFile file keys =
    let isHeading (line: string) = line.StartsWith "--"
    let mutable heading = ""
    let grouper line =
        if isHeading line then heading <- line
        heading
    let pairs =
        file
        |> File.ReadLines
        |> Seq.skipWhile (isHeading >> not)
        |> Seq.groupBy grouper
        |> Seq.map (fun (h, l) -> h, Seq.skip 1 l)
    let results: seq<string>[] = Array.create (List.length keys) <| seq []
    for head, lines in pairs do
        match List.tryFindIndex ((=) head) keys with
        | Some i -> results.[i] <- lines
        | _ -> ()
    Array.toList results

#nowarn "0025"
let parseSymbolsFile file =
    let readLine (line: string) = let arr = line.Split '\t'
                                  arr.[0], int arr.[1]
    let readSpacerLine (line: string) = let arr = line.Split '\t'
                                        int arr.[0], uint64 arr.[1]
    let [prev; size; relative; labels; spacers] =
        splitFile file [PREVIOUS_HEADING; SIZE_HEADING; RELATIVE_HEADING; LABELS_HEADING; SPACERS_HEADING]
    Seq.exactlyOne prev,
    Seq.exactlyOne size |> int,
    Seq.map readLine relative,
    Seq.map readLine labels,
    Seq.map readSpacerLine spacers

let assem source binary symbols =
    let ao = doAssemble source
    writeAssemblerOutput binary symbols ao.Binary ao.Exported ao.Labels ao.Spacers ""

let readTraceSyms file =
    let pairs = splitFile file [LABELS_HEADING]
                |> Seq.exactlyOne
                |> Seq.map (fun line -> let arr = line.Split '\t'
                                        int arr.[1], arr.[0])
    new Map<int, string>(pairs)

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
    let stack = doRun bytes arg outputDir traceSyms
    if symbolsIfShouldTrace.IsNone then writeStack stack

let asRun source argFile outputDir shouldTrace =
    let ao = doAssemble source
    if shouldTrace then
        for name, pos in Seq.sortBy fst ao.Exported do
            printfn "%20s %6d" name pos

    let traceSyms =
        if not shouldTrace then None
        else let flip (x, y) = (y, x)
             Some <| new Map<int, string> (Seq.map flip ao.Labels)

    let arg = match argFile with
              | Some name -> File.ReadAllBytes name
              | None -> Array.empty
    let stack = doRun ao.Binary arg outputDir traceSyms
    if not shouldTrace then writeStack stack

let check source =
    printfn "%s" <| doCheck source

let genProj rootDir (goal: string) =
    let projectFile = goal + ".proj"
    if File.Exists projectFile
    then failwithf "File exists: %s" projectFile
    let lines = seq {
        yield ROOT_HEADING
        yield Path.GetFullPath rootDir
        yield RELATIVE_ORDER_HEADING
        yield! getBuildOrder rootDir goal
    }
    File.WriteAllLines (projectFile, lines)
    printfn "Project file created: %s" projectFile

#nowarn "0025"
let parseProjectFile file =
    let [rootDir; relativeOrder] = splitFile file [ROOT_HEADING; RELATIVE_ORDER_HEADING]
    Seq.exactlyOne rootDir, Seq.toList relativeOrder

let build projectFile destinationDir incrementally =
    let rootDir, buildOrder = parseProjectFile projectFile

    let timestamp file = if File.Exists file then Some <| File.GetLastWriteTimeUtc file else None

    let notNewer (w1: System.DateTime option) w2 =
        match w1, w2 with
        | Some t1, Some t2 -> t1.CompareTo t2 <= 0
        | _ -> false

    let reused =
        if not incrementally
        then []
        else
            // Reuse the part of the buildOrder that does not have to be rebuilt.
            seq {
                    let mutable previous = ""
                    let mutable prevSymStamp : System.DateTime option = None
                    for node in buildOrder do
                        let sourceStamp = nodePath rootDir SOURCE_EXTENSION node |> timestamp
                        let symFile = nodePath destinationDir SYMBOLS_EXTENSION node
                        let symbolsStamp = timestamp symFile
                        let mutable res : AssemblerOutput option = None
                        if notNewer sourceStamp symbolsStamp
                           && (previous = "" || notNewer prevSymStamp symbolsStamp)
                        then
                            let oldPrevious, _, exported, labels, spacers = parseSymbolsFile symFile
                            if previous = oldPrevious
                            then
                                let binFile = nodePath destinationDir BINARY_EXTENSION node
                                if File.Exists binFile
                                then
                                    printfn "Up-to-date: %s" node
                                    res <- Some { Node=node;
                                                  Binary=File.ReadAllBytes binFile;
                                                  Exported=exported;
                                                  Labels=labels;
                                                  Spacers=spacers }
                        yield res
                        previous <- node
                        prevSymStamp <- symbolsStamp
                }
            |> Seq.takeWhile Option.isSome |> Seq.map Option.get |> Seq.toList

    let mustBuild = List.skip reused.Length buildOrder

    let write node =
        let bFile = nodePath destinationDir BINARY_EXTENSION node
        let sFile = nodePath destinationDir SYMBOLS_EXTENSION node
        writeAssemblerOutput bFile sFile

    let saveLinked rest =
        let ao = Seq.append reused rest |> doCollect
        write (ao.Node + "$") ao.Binary ao.Exported ao.Labels ao.Spacers ""

    if mustBuild = []
    then
        let node = Seq.last buildOrder
        let sFile = nodePath destinationDir SYMBOLS_EXTENSION node
        let lFile = nodePath destinationDir SYMBOLS_EXTENSION <| node + "$"
        if notNewer (timestamp sFile) (timestamp lFile)
        then
            printfn "Up-to-date: %s" <| node + "$"
        else
            saveLinked []
    else
        let outputs = doBuild rootDir reused mustBuild

        // This triggers the saving of individual binaries as a side effect.
        saveLinked <| seq {
            let mutable previous = Seq.tryLast reused |> Option.map (fun ao -> ao.Node) |> valueOr ""
            for ao in outputs do
                write ao.Node ao.Binary ao.Exported ao.Labels ao.Spacers previous
                previous <- ao.Node
                yield ao
        }

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
            | "build" when n = 3 -> build argv.[1] argv.[2] false; 0
            | "make" when n = 3 -> build argv.[1] argv.[2] true; 0
            | _ -> usage (); 1
    with
        Failure msg -> printfn "%s" msg; 1
