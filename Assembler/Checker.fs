module Assembler.Checker

open Assembler.Parser
open Assembler.Composition
open Machine.Executor
open FParsec
open System.Text.RegularExpressions

let expectationHeading : Parser<unit, unit> =
    spaces >>. skipString "# EXPECTED END STACK:"
           >>. spaces
           >>. (eof <|> skipChar '#')

let isNotExpectationHeading line : bool =
    match runParserOnString expectationHeading () "" line with
    | Success (_) -> false
    | Failure (_) -> true

let numberParser : Parser<int64, unit> =
    let neg = (skipChar '-') >>. puint64 |>> (int64 >> (~-))
    let pos = puint64 |>> int64
    spaces >>. skipChar '#' >>. spaces
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

let doAssemble fileName =
    use stream = System.IO.File.OpenRead fileName
    try 
        let program, labels = parseProgram stream
        let names = List.sort [for pair in labels -> pair.Key]
        let bytes, symbols = assemble program
        let symbolList = [for name in names -> (name, symbols.[labels.[name]])]
        bytes, symbolList
    with
        ParseException(msg) -> failwith msg

let doRun binary shouldTrace =
    try
        (if shouldTrace then trace else execute) binary
        |> Seq.map int64
    with
        | AccessException msg -> failwith "Access exception!"
        | UndefinedException msg -> failwith "Undefined instruction!"

let doCheck fileName =
    let binary = doAssemble fileName |> fst
    let actual = doRun binary false

    let expected =
        System.IO.File.ReadLines fileName
        |> Seq.skipWhile isNotExpectationHeading
        |> Seq.append [""]
        |> Seq.skip 1 // Skip the heading
        |> Seq.map parseNumber
        |> Seq.map Option.toList
        |> Seq.concat

    match firstDiff actual expected with
    | i, None, None ->
        sprintf "End stack as expected (size: %d)." i
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
