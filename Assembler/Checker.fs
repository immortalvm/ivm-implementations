module Assembler.Checker

open System.IO

open Assembler.Parser
open Assembler.Composition
open Machine.Executor
open FParsec
open System.Text.RegularExpressions

// These constants should be annotated [<Literal>], but then they can no longer
// be made available through the signature file. This is (yet another) reason
// to stop using signature files.
let SOURCE_EXTENSION = ".s"
let BINARY_EXTENSION = ".b"
let SYMBOLS_EXTENSION = ".sym"

type assemblerOutput = string * uint8 list * (string * int) list * (string * int) list

let getDependencies (fileName : string) : Set<string> =
    printfn "Analyzing dependencies in %s..." fileName
    use stream = File.OpenRead fileName
    try parseDependencies stream
    with ParseException(msg) -> failwith msg

let nodePath rootDir (node: string) extension =
    (Array.append [|rootDir|] (node.Split '.') |> Path.Combine) + extension

// Depth first topological sorting
let getBuildOrder (rootDir: string) (goal : string) : seq<string> =
    let mutable processed : Set<string> = set []
    let rec visit node (seen: Set<string>) =
        seq {
            if processed.Contains node then ()
            else if seen.Contains node then failwithf "Circular dependency: %s" node
            else
                let deps = getDependencies <| nodePath rootDir node SOURCE_EXTENSION
                if not deps.IsEmpty
                then
                    let s = seen.Add node
                    for d in deps do yield! visit d s
                processed <- processed.Add(node)
                yield node
        }
    visit goal <| set []

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

let buildOne (fileName: string) (relSymbols: string -> int) =
    printfn "Building %s..." fileName
    use stream = File.OpenRead fileName
    try
        let program, exported, labels = parseProgram (State.Init relSymbols) stream
        let bytes, symbols = assemble program
        let symList (map: Map<string, int>) =
            [for pair in map -> (pair.Key, symbols.[pair.Value])]
        bytes, symList exported, symList labels
    with
        ParseException(msg) -> failwith msg

let doBuild (rootDir: string) (buildOrder: seq<string>) : seq<assemblerOutput> =
    let mutable currentSize : int = 0
    // Distance from the end: [0, currentSize]
    let mutable allSymbols : Map<string, int> = Map.empty
    let lookup label = match allSymbols.TryFind label with
                       | None -> -1
                       | Some dist -> currentSize - dist // >= 0
    seq {
        for node in buildOrder do
            let sourceFile = nodePath rootDir node SOURCE_EXTENSION
            let bytes, exported, labels = buildOne sourceFile lookup
            yield node, bytes, exported, labels

            currentSize <- currentSize + bytes.Length
            for label, pos in exported do
                allSymbols <- allSymbols.Add (node + "." + label, currentSize - pos)
    }

let doCollect (outputs: seq<assemblerOutput>) : assemblerOutput =
    let tuples = outputs |> Seq.toList |> Seq.rev
    let bin = Seq.append (tuples |> Seq.map (fun (_,b,_,_) -> List.toArray b)) [Array.zeroCreate 16] |> Seq.concat |> Seq.toList
    let lab = [
        let mutable offset = 0
        for n, b, _, lst in tuples do
            let o = offset
            yield seq {for label, pos in lst -> n + "." + label, pos + o}
            offset <- offset + List.length b
    ]
    let lastNode = let n,_,_,_ = Seq.item 0 tuples in n
    lastNode, bin, [], lab |> Seq.concat |> Seq.toList

let doAssemble (fileName: string) =
    let rootDir = Path.GetDirectoryName fileName
    let buildOrder = getBuildOrder rootDir <| Path.GetFileNameWithoutExtension fileName
    buildOrder |> doBuild rootDir |> doCollect

let doRun binary traceSyms =
    try
        execute binary traceSyms |> Seq.map int64
    with
        | AccessException msg -> failwith "Access exception!"
        | UndefinedException msg -> failwith "Undefined instruction!"

let doCheck fileName =
    let mutable revOutput = []
    let output msg = revOutput <- msg :: revOutput
    let _, binary, _, _ = doAssemble fileName
    output <| sprintf "Binary size: %d" (List.length binary)

    let expectationsFound = File.ReadLines fileName
                            |> Seq.exists isExpectationHeading

    if not expectationsFound
    then output "Not executed since no expectations were found."
    else
        let actual = doRun binary None

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
