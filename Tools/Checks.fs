﻿module Tools.Checks

open System.IO
open Assembler.Integration
open Tools.Helpers

open FParsec


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

let private numberParser : Parser<int64 list, unit> =
    let neg = (skipChar '-') >>. puint64 |>> (int64 >> (~-))
    let pos = puint64 |>> int64
    spaces >>. many1 (skipChar '#')
           >>. spaces
           >>. many1 ((neg <|> pos) .>> spaces)
           .>> (eof <|> skipChar '#')

let private parseNumbers line : int64 list =
    match runParserOnString numberParser () "" line with
    | Success (result, _, _) -> result
    | Failure (_) -> []

// Based on Expecto.Expect
let private firstDiff s1 s2 =
  let s1 = Seq.append (Seq.map Some s1) (Seq.initInfinite (fun _ -> None))
  let s2 = Seq.append (Seq.map Some s2) (Seq.initInfinite (fun _ -> None))
  Seq.mapi2 (fun i s p -> i,s,p) s1 s2
  |> Seq.find (function | _ , Some s, Some p when s = p -> false | _ -> true)

let doCheck filenames (sourceRoot: string option) libs entry memory shouldTrace loc noopt =
    let mutable revOutput = []
    let output msg = revOutput <- msg :: revOutput
    let ao = doAssemble (src entry filenames sourceRoot) (libraries sourceRoot libs) noopt
    let primary = List.head filenames
    let expectationsFound = File.ReadLines primary
                            |> Seq.exists isExpectationHeading

    if not expectationsFound
    then output "Not executed since no expectations were found."
    else
        let traceSyms =
            if not shouldTrace then None
            else ao.Labels |> Seq.map (fun (sym, pos) -> int pos, sym) |> Map |> Some

        let actual = doRun memory ao.Binary Seq.empty None None traceSyms loc
        let expected =
            File.ReadLines primary
            |> Seq.skipWhile (isExpectationHeading >> not)
            |> Seq.append [""]
            |> Seq.skip 1 // Skip the heading
            |> Seq.collect parseNumbers

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
