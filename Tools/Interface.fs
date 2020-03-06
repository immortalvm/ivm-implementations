module Tools.Interface

open System.IO
open Tools.Helpers
open Assembler.Integration

// Symbols file headings

[<Literal>]
let PREVIOUS_HEADING = "--Previous--"

[<Literal>]
let SIZE_HEADING = "--Size--"

[<Literal>]
let RELATIVE_HEADING = "--Relative--"

[<Literal>]
let CONSTANT_HEADING = "--Constant--"

[<Literal>]
let LABELS_HEADING = "--Labels--"

[<Literal>]
let SPACERS_HEADING = "--Spacers--"

[<Literal>]
let RELATIVES_HEADING = "--Relatives--"

// Project file headings

[<Literal>]
let ROOT_HEADING = "--Root--"

[<Literal>]
let RELATIVE_ORDER_HEADING = "--Relative--"

let writeAssemblerOutput binaryFile symbolsFile bytes exported constant labels spacers previous =
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
        yield CONSTANT_HEADING
        yield! mapLines constant
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
                                  arr.[0], int64 arr.[1]
    let [prev; size; relative; constant; labels; spacers; relatives] =
        splitFile file [PREVIOUS_HEADING; SIZE_HEADING; RELATIVE_HEADING; CONSTANT_HEADING; LABELS_HEADING; SPACERS_HEADING; RELATIVES_HEADING]
    Seq.exactlyOne prev,
    Seq.exactlyOne size |> int,
    Seq.map readLine relative,
    Seq.map readLine constant,
    Seq.map readLine labels,
    Seq.map readLine spacers |> Seq.map (fun (x, y) -> int x, y),
    Seq.map int relatives

let assem sources sourceRoot libs entry binary symbols noopt =
    let ao = doAssemble (src entry sources sourceRoot) (libraries sourceRoot libs) noopt
    let primary = List.head sources
    let b = match binary with
            | None -> Path.ChangeExtension(primary, BINARY_EXTENSION)
            | Some x -> x
    let s = match symbols with
            | None -> Path.ChangeExtension(primary, SYMBOLS_EXTENSION)
            | Some x -> x
    writeAssemblerOutput b s ao.Binary ao.Exported ao.Constants ao.Labels ao.Spacers ""

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

let processEndStack (shouldTrace: bool) (stack: seq<int64>) =
    let stack2 = if shouldTrace then stack
                 else let s = Seq.cache stack in writeStack s; s
    match Seq.tryExactlyOne stack2 with
    | Some status -> int status
    | None -> 2

let run memory binary (argFile: string option) (outputDir: string option) shouldTrace =
    let bytes = File.ReadAllBytes binary
    let arg = match argFile with
              | Some name -> File.ReadAllBytes name
              | None -> Array.empty
    let traceSyms =
        if shouldTrace
        then Path.ChangeExtension(binary, SYMBOLS_EXTENSION) |> readTraceSyms |> Some
        else None
    doRun memory bytes arg outputDir traceSyms |> processEndStack shouldTrace

let asRun sources sourceRoot libs entry memory argFile outputDir shouldTrace noopt =
    let ao = doAssemble (src entry sources sourceRoot) (libraries sourceRoot libs) noopt
    if shouldTrace then
        for name, pos in Seq.sortBy fst ao.Exported do
            printfn "%20s %6d" name pos
    let traceSyms =
        if not shouldTrace then None
        else let flip (x, y) = (int y, x)
             Some <| new Map<int, string> (Seq.map flip ao.Labels)

    let arg = match argFile with
              | Some name -> File.ReadAllBytes name
              | None -> Array.empty
    doRun memory ao.Binary arg outputDir traceSyms |> processEndStack shouldTrace

let createLibrary rootDirectory libraryFileName =
    dirToZipLib rootDirectory libraryFileName
    printfn "Library created: %s" libraryFileName
