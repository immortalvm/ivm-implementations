module Tools.Checks

open System.IO
open Assembler.Integration
open Tools.Helpers

open Assembler.Ast
open Assembler.Parser

open FParsec


// We only support explicit imports with checks for now.
let private dirLib (directory: string): Library =
    let path node = nodePath directory SOURCE_EXTENSION node
    let contains (node: string) = File.Exists <| path node
    let exportedBy (_: string): string option = None
    let dependencies (node: string) : Set<string> =
        let filename = path node
        printfn "Analyzing dependencies in %s..." filename
        use stream = File.OpenRead filename
        try parseDependencies stream
        with ParseException(msg) -> failwith msg
    let get (node: string) : string list * Stream =
        [], upcast (path node |> File.OpenRead)
    {
        Contains = contains
        ExportedBy = exportedBy
        Dependencies = dependencies
        Get = get
    }

let private expectationHeading : Parser<unit, unit> =
    spaces >>. many1 (skipChar '#')
           >>. spaces
           >>. skipString "EXPECTED STACK:"
           >>. spaces
           >>. (eof <|> skipChar '#')

let private isExpectationHeading line : bool =
    match runParserOnString expectationHeading () "" line with
    | Success (_) -> true
    | Failure (_) -> false

let private numberParser : Parser<int64, unit> =
    let neg = (skipChar '-') >>. puint64 |>> (int64 >> (~-))
    let pos = puint64 |>> int64
    spaces >>. many1 (skipChar '#')
           >>. spaces
           >>. (neg <|> pos)
           .>> spaces
           .>> (eof <|> skipChar '#')

let private parseNumber line : int64 option =
    match runParserOnString numberParser () "" line with
    | Success (result, _, _) -> Some result
    | Failure (_) -> None

// Based on Expecto.Expect
let private firstDiff s1 s2 =
  let s1 = Seq.append (Seq.map Some s1) (Seq.initInfinite (fun _ -> None))
  let s2 = Seq.append (Seq.map Some s2) (Seq.initInfinite (fun _ -> None))
  Seq.mapi2 (fun i s p -> i,s,p) s1 s2
  |> Seq.find (function | _ , Some s, Some p when s = p -> false | _ -> true)

let doCheck fileName shouldTrace =
    let mutable revOutput = []
    let output msg = revOutput <- msg :: revOutput
    let binary = (doAssemble (src [fileName]) [dirLib (Path.GetDirectoryName fileName)]).Binary
    output <| sprintf "Binary size: %d" (Seq.length binary)

    let expectationsFound = File.ReadLines fileName
                            |> Seq.exists isExpectationHeading

    if not expectationsFound
    then output "Not executed since no expectations were found."
    else
        let actual = doRun binary Seq.empty None (if shouldTrace then Some Map.empty else None)

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
