module Assembler.Checker

open System.IO
open Assembler.Ast
open Assembler.Parser
open Assembler.Composition
open Assembler.Namespace
open Machine.Executor
open Machine.Utils
open FParsec
open System.Text.RegularExpressions

open Assembler.FullyConnectedComponents

// These constants should be annotated [<Literal>], but then they can no longer
// be made available through the signature file. This is (yet another) reason
// to stop using signature files.
let SOURCE_EXTENSION = ".s"
let BINARY_EXTENSION = ".b"
let SYMBOLS_EXTENSION = ".sym"

type AssemblerOutput = {
    Node: string
    Binary: seq<uint8>
    Exported: seq<string * int64>
    Constants: seq<string * int64>
    Labels: seq<string * int64>
    Spacers: seq<int * int64>
    Relatives: seq<int>
}

let getDependencies (fileName : string) : Set<string> =
    printfn "Analyzing dependencies in %s..." fileName
    use stream = File.OpenRead fileName
    try parseDependencies stream
    with ParseException(msg) -> failwith msg

let nodePath rootDir extension (node: string) =
    (Array.append [|rootDir|] (splitNodeString node) |> Path.Combine) + extension

let getBuildOrder (rootDir: string) (goal : string) : seq<string list> =
    let edges node = getDependencies <| nodePath rootDir SOURCE_EXTENSION node
    findComponents(goal, edges >> seq) |> Seq.rev |> Seq.map (Seq.rev >> Seq.toList)

let expectationHeading : Parser<unit, unit> =
    spaces >>. many1 (skipChar '#')
           >>. spaces
           >>. skipString "EXPECTED STACK:"
           >>. spaces
           >>. (eof <|> skipChar '#')

let isExpectationHeading line : bool =
    match runParserOnString expectationHeading () "" line with
    | Success (_) -> true
    | Failure (_) -> false

let numberParser : Parser<int64, unit> =
    let neg = (skipChar '-') >>. puint64 |>> (int64 >> (~-))
    let pos = puint64 |>> int64
    spaces >>. many1 (skipChar '#')
           >>. spaces
           >>. (neg <|> pos)
           .>> spaces
           .>> (eof <|> skipChar '#')

// From Expecto.Expect
let firstDiff s1 s2 =
  let s1 = Seq.append (Seq.map Some s1) (Seq.initInfinite (fun _ -> None))
  let s2 = Seq.append (Seq.map Some s2) (Seq.initInfinite (fun _ -> None))
  Seq.mapi2 (fun i s p -> i,s,p) s1 s2
  |> Seq.find (function | _ , Some s, Some p when s = p -> false | _ -> true)

let parseNumber line : int64 option =
    match runParserOnString numberParser () "" line with
    | Success (result, _, _) -> Some result
    | Failure (_) -> None

let showValue (x: int64) =
    let mutable hex = sprintf "%016X" x
    hex <- Regex.Replace(hex, "^000+", "..00")
    hex <- Regex.Replace(hex, "^FFF+", "..FF")
    sprintf "%d (0x%s)" x hex

// Build one (mutually dependent) component.
let buildOne rootDir (nodes: string list) (extSymbols: string -> (bool * int64) option) =
    try
        let pair node =
            let fileName = nodePath rootDir SOURCE_EXTENSION node
            printfn "Parsing %s..." fileName
            let f () : System.IO.Stream = upcast (File.OpenRead fileName)
            node, f
        let program, exported, labels = parseProgram (List.map pair nodes) extSymbols
        //for line in program do printfn "%O" line

        let bytes, symbols, spacers, relatives, exports = assemble program
        let exp = [ for i in exported -> labels.[i], exports.[i] ]
        {
            Node = nodes.[0]
            Binary = bytes
            Exported = [ for (l, (r, v)) in exp do if r then yield l, v ]
            Constants = [ for (l, (r, v)) in exp do if not r then yield l, v ]
            Labels = [
                for pair in labels do
                    let i = pair.Key
                    if i < symbols.Length then
                        let pos = symbols.[i]
                        if pos >= 0 then
                            yield pair.Value, int64 pos
            ]
            Spacers = spacers
            Relatives = relatives
        }
    with
        ParseException(msg) -> failwith msg

let doBuild (rootDir: string) (reused: seq<AssemblerOutput>) (buildOrder: seq<string list>) : seq<AssemblerOutput> =
    let mutable currentSize : int64 = 0L
    // Maps each qualified names to its distance from the end [0, currentSize].
    let mutable allSymbols : Map<string, bool * int64> = Map.empty

    let incorporate (ao: AssemblerOutput) =
        currentSize <- currentSize + int64 (Seq.length ao.Binary)
        for qn, pos in ao.Exported do
            allSymbols <- allSymbols.Add (qn, (true, currentSize - pos))
        for qn, value in ao.Constants do
            allSymbols <- allSymbols.Add (qn, (false, value))

    for ao in reused do
        incorporate ao

    let lookup qn =
                    match allSymbols.TryFind qn with
                    | None -> None
                    | Some (true, dist) -> Some (true, currentSize - dist)
                    | Some (false, value) -> Some (false, value)
    seq {
        for nodes in buildOrder do
            let ao = buildOne rootDir (Seq.toList nodes) lookup
            yield ao
            incorporate ao
    }

let doCollect (outputs: seq<AssemblerOutput>): AssemblerOutput =
    let rev = outputs |> Seq.cache |> Seq.rev
    let spacers = [
        let mutable offset = 0
        for ao in rev do
            for pos, size in ao.Spacers do
                yield pos + offset, size
            offset <- offset + Seq.length ao.Binary
    ]
    let mutable offset = 0
    let relatives = [
        for ao in rev do
            for pos in ao.Relatives do
                yield pos + offset
            offset <- offset + Seq.length ao.Binary
    ]
    let initBin = initialization offset spacers relatives
    let binary =
        Seq.append initBin
                   (rev |> Seq.map (fun ao -> ao.Binary) |> Seq.concat)
    let labels = seq {
        let mutable offset = int64 initBin.Length
        for ao in rev do
            for qn, pos in ao.Labels do
                yield qn, pos + offset
            offset <- offset + int64(Seq.length ao.Binary)
    }
    {
        Node=(Seq.head rev).Node
        Binary=Seq.cache binary
        Exported=[]
        Constants=[]
        Labels=Seq.cache labels
        Spacers=[]
        Relatives=[]
    }

let doAssemble (fileName: string) =
    let rootDir = Path.GetDirectoryName fileName
    let buildOrder = getBuildOrder rootDir <| Path.GetFileNameWithoutExtension fileName
    buildOrder |> doBuild rootDir [] |> doCollect // 64 KiB stack

let doRun binary arg outputDir traceSyms =
    try
        // Memory size: 16 MiB (for now)
        execute (1UL <<< 24) binary arg outputDir traceSyms |> Seq.map int64
    with
        | AccessException msg -> failwith "Access exception!"
        | UndefinedException msg -> failwith "Undefined instruction!"

let doCheck fileName =
    let mutable revOutput = []
    let output msg = revOutput <- msg :: revOutput
    let binary = (doAssemble fileName).Binary
    output <| sprintf "Binary size: %d" (Seq.length binary)

    let expectationsFound = File.ReadLines fileName
                            |> Seq.exists isExpectationHeading

    if not expectationsFound
    then output "Not executed since no expectations were found."
    else
        let actual = doRun binary Seq.empty None None

        let expected =
            File.ReadLines fileName
            |> Seq.skipWhile (isExpectationHeading >> not)
            |> Seq.append [""]
            |> Seq.skip 1 // Skip the heading
            |> Seq.map parseNumber
            |> Seq.map Option.toList
            |> Seq.concat

        match firstDiff actual expected with
        | i, None, None ->
            output <| sprintf "End stack as expected (size: %d)." i
        | i, Some a, Some e ->
            failwithf
                "Unexpected stack entry at position %i. Expected: %s, but got %s."
                i (showValue e) (showValue a)
        | i, None, Some e ->
            failwithf
                "Stack shorter than expected, at pos %i for expected %s."
                i (showValue e)
        | i, Some a, None ->
            failwithf
                "Stack longer than expected, at pos %i found %s."
                i (showValue a)
    System.String.Join("\n", List.rev revOutput)
