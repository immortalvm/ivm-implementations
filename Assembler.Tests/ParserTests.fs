module Assembler.Tests.ParserTests

open System
open Expecto
open Expecto.Impl
open FsCheck
open Swensen.Unquote

open FParsec
open Assembler.Ast
open Assembler.Parser

// NB. Opposite order of Expect.* (for convenience).
let success expected parser input =
    match run (parser .>> eof) input with
    | Success(result, _, _)   -> test <@ result = expected @>
    | Failure(errorMsg, _, _) -> raise <| new AssertException (sprintf "Failure: %s" errorMsg)

let failure pattern parser input =
    match run (parser .>> eof) input with
    | Success(result, _, _)   -> raise <| new AssertException (sprintf "Success: %O" result)
    | Failure(errorMsg, _, _) ->
        Expect.isMatch errorMsg pattern
            "Unexpected error message"

[<Tests>]
let BasicTests =
    let stopError = "Expecting: end of input or '#'"
    testList "Basics" [
        testCase "Comment" <| fun () -> success () comment "# abc\n"
        testCase "Whitespace" <| fun () -> success () whitespace " \r# abcd\n  \t #7"
        testCase "Identifier" <| fun () -> success "xY_123" identifier "xY_123  # comment"
        testCase "Numeral-dec" <| fun () -> success  1234L numeral "1234 # comment"
        testCase "Numeral-oct-neg" <| fun () -> success  -08L numeral "-0o10 # comment"
        testCase "Numeral-hex-big"<| fun () -> success -1L numeral <| "0x" + String.replicate 16 "f"
        testCase "Numeral-hex-small"<| fun () -> success 1L numeral <| "-0x" + String.replicate 16 "f"

        testCase "Comment-fail" <| fun () -> failure "Expecting: '#'" comment "not a comment"
        testCase "Whitespace-fail" <| fun () -> failure stopError whitespace "#\n777"
        testCase "Identifier-fail" <| fun () -> failure "Expecting: identifier" identifier "64"
        testCase "Numeral-dec-fail" <| fun () -> failure stopError numeral "123a"
        testCase "Numeral-oct-fail" <| fun () -> failure stopError numeral "0o18"
        testCase "Numeral-hex-fail" <| fun () -> failure "outside the allowable range" numeral <| "0x" + String.replicate 17 "f"
    ]

[<Tests>]
let ExpressionTests =
    testList "Expression" [
        testList "Leaf" [
            testCase "Numeral" <| fun () -> success (ENum 17L) expression "17"
            testCase "Label" <| fun () -> success (ELabel "u17_X_") expression "u17_X_"
            testCase "PC" <| fun () -> success EPc expression "$pc"
            testCase "Peek" <| fun () -> success (EPeek -94L) expression "$-94"
            testCase "Stack" <| fun () -> success (EStack 26L) expression "&0x1a"
        ]
        testList "Depth1" [
            testCase "Sum" <| fun () -> success (ESum[EPc;  ENum -3L]) expression "(+ $pc -3)"
            testCase "Conjunction" <| fun () -> success (EConj [EStack 8L;  ELabel "lab"]) expression "( & &0o10 lab)"
            testCase "Minus" <| fun () -> success (EMinus <| EPeek 16L) expression "-$0x10"
        ]
        testList "Depth2" [
            testCase "Product" <| fun () -> success (EProd[EPeek 3L;  ESum [ENum 16L; EPeek 0L]]) expression "(* $3 (+ 0x10 $0))"
            testCase "Disjunction" <| fun () -> success (EDisj [EPeek 8L;  ELabel "lab"]) expression "(|\t$0o10 lab)"
            testCase "Negation" <| fun () -> success (EPc |> ENeg |> ENeg) expression "~~$pc"
        ]
    ]