open System.IO

open System
open System.CommandLine
open System.CommandLine.Invocation
open System.CommandLine.Builder

open Machine.Utils
open Assembler.Checker

[<Literal>]
let VERSION = "0.17" // How can we automatically get this from Git?


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

let assem source binary symbols =
    let ao = doAssemble source
    let b = match binary with
            | None -> Path.ChangeExtension(source, "b")
            | Some x -> x
    let s = match symbols with
            | None -> Path.ChangeExtension(source, "sym")
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

let run binary (argFile: string option) (outputDir: string option) shouldTrace =
    let bytes = File.ReadAllBytes binary
    let arg = match argFile with
              | Some name -> File.ReadAllBytes name
              | None -> Array.empty
    let traceSyms =
        if shouldTrace
        then Path.ChangeExtension(binary, "sym") |> readTraceSyms |> Some
        else None
    let stack = doRun bytes arg outputDir traceSyms
    if traceSyms.IsNone then writeStack stack

let asRun source argFile outputDir shouldTrace =
    let ao = doAssemble source
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
    let stack = doRun ao.Binary arg outputDir traceSyms
    if not shouldTrace then writeStack stack

let genProj rootDir (goal: string) =
    let projectFile = goal + ".proj"
    if File.Exists projectFile
    then failwithf "File exists: %s" projectFile
    let lines = seq {
        yield ROOT_HEADING
        yield Path.GetFullPath rootDir
        yield RELATIVE_ORDER_HEADING
        yield! getBuildOrder rootDir goal |> Seq.map (fun c -> System.String.Join('\t', c))
    }
    File.WriteAllLines (projectFile, lines)
    printfn "Project file created: %s" projectFile

#nowarn "0025"
let parseProjectFile file =
    let [rootDir; relativeOrder] =
        splitFile file [ROOT_HEADING; RELATIVE_ORDER_HEADING]
    Seq.exactlyOne rootDir,
    relativeOrder |> Seq.map (fun line -> line.Split('\t') |> Seq.toList) |> Seq.toList

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
                    for nodes in buildOrder do
                        let sourceStamp = nodes |> Seq.map (nodePath rootDir SOURCE_EXTENSION >> timestamp) |> Seq.max
                        let node = nodes.[0]
                        let symFile = nodePath destinationDir SYMBOLS_EXTENSION node
                        let symbolsStamp = timestamp symFile
                        let mutable res : AssemblerOutput option = None
                        if notNewer sourceStamp symbolsStamp
                           && (previous = "" || notNewer prevSymStamp symbolsStamp)
                        then
                            let oldPrevious, _, exported, constants, labels, spacers, relatives = parseSymbolsFile symFile
                            if previous = oldPrevious
                            then
                                let binFile = nodePath destinationDir BINARY_EXTENSION node
                                if File.Exists binFile
                                then
                                    printfn "Up-to-date: %s" node
                                    res <- Some { Node=node;
                                                  Binary=File.ReadAllBytes binFile;
                                                  Exported=exported;
                                                  Constants=constants
                                                  Labels=labels;
                                                  Spacers=spacers;
                                                  Relatives = relatives }
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
        write (ao.Node + "$") ao.Binary ao.Exported ao.Constants ao.Labels ao.Spacers ""

    if mustBuild = []
    then
        let node = (Seq.last buildOrder).[0]
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
                write ao.Node ao.Binary ao.Exported ao.Constants ao.Labels ao.Spacers previous
                previous <- ao.Node
                yield ao
        }

let _assemblyVersion =
    let assembly = System.Reflection.Assembly.GetExecutingAssembly() // ?? Assembly.GetExecutingAssembly();
    assembly.GetName().FullName //  .Version.ToString()


[<EntryPoint>]
let main argv =
    let extend (c: Command) (sl: Symbol List) : Command =
        for x in sl do c.Add(x)
        c

    let com (name: string) (descr: string) (sl: Symbol List) (handler: ICommandHandler) : Command =
        extend (Command(name, Description=descr, Handler=handler)) sl

    let strArg (name: string) (descr: string) = Argument<string>(name, Description=descr)
    let fileArg (name: string) (descr: string) = Argument<FileInfo>(name, Description=descr)
    let dirArg (name: string) (descr: string) = Argument<DirectoryInfo>(name, Description=descr)

    let opt (name: string) (descr: string) : Option =
        Option(name, Description=descr)

    let argOpt (name: string) (descr: string) (a: Argument<'t>): Option<'t> =
        assert name.StartsWith "--"
        Option<'t>(name, Argument=a, Description=descr)

    let alias(name: string) (opt: Option) =
        assert (name.StartsWith "-" && not(name.StartsWith "--"))
        opt.AddAlias(name)
        opt

    let source = (fileArg "source file" "Name of source file (<name>.s)").ExistingOnly()
    let trace = opt "--trace" "Turn on trace output" |> alias "-t"
    let arg = argOpt "--arg" "Specify argument file (default: none)" <| (fileArg "argument file" null).ExistingOnly()
    let out = argOpt "--out" "Specify output directory (default: none)" <| dirArg "output directory" null

    let fName (fsi: FileSystemInfo) : string = fsi.FullName
    let oName (fsi: FileSystemInfo) : string option =
        if fsi = null then None else Some fsi.FullName

    let root =
        extend (RootCommand("ivm", Description="iVM Assembler and VM")) [

            com "as" "Assemble source file" [
                (argOpt "--bin" "Specify output binary file (default: <name>.b)" <| fileArg "binary file" null)
                (argOpt "--sym" "Specify output symbol file (default: <name>.sym)" <| fileArg "symbol file" null)
                source
            ] <| CommandHandler.Create(fun bin sym ``source file`` ->
                    assem (fName ``source file``) (oName bin) (oName sym))

            com "run" "Execute binary" [
                trace; arg; out
                (fileArg "binary file" "Name of binary file").ExistingOnly()
            ] <| CommandHandler.Create(fun trace arg out ``binary file`` ->
                    run (fName ``binary file``) (oName arg) (oName out) trace)

            com "as-run" "Assemble and run" [
                trace; arg; out
                source
            ] <| CommandHandler.Create(fun trace arg out ``source file`` ->
                    asRun (fName ``source file``) (oName arg) (oName out) trace)

            com "check" "Assemble, run and check final stack" [
                source
            ] <| CommandHandler.Create(fun ``source file`` ->
                    fName ``source file`` |> doCheck |> printfn "%s")

            com "gen" "Generate project file" [
                (dirArg "root dir" "Source root directory").ExistingOnly()
                strArg "goal" "The goal (relative filename without suffix)"
            ] <| CommandHandler.Create(fun ``root dir`` goal ->
                genProj (fName ``root dir``) goal)

            com "build" "Build a project" [
                opt "--incrementally" "Only rebuild if necessary" |> alias "-i"
                (fileArg "project" "Project file").ExistingOnly()
                dirArg "dest dir" "Destination directory"
            ] <| CommandHandler.Create(fun incrementally project ``dest dir`` ->
                build (fName project) (fName ``dest dir``) incrementally)
        ]
    try
        root.Invoke(argv)
    with
        Failure msg -> printfn "%s" msg; 1
