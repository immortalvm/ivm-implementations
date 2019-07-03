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
        testCase "Numeral-dec" <| fun () -> success  1234L positiveNumeral "1234 # comment"
        testCase "Numeral-oct" <| fun () -> success  08L positiveNumeral "0o10 # comment"
        testCase "Numeral-hex-big"<| fun () -> success -1L positiveNumeral <| "0x" + String.replicate 16 "f"

        testCase "Comment-fail" <| fun () -> failure "Expecting: '#'" comment "not a comment"
        testCase "Whitespace-fail" <| fun () -> failure stopError whitespace "#\n777"
        testCase "Identifier-fail" <| fun () -> failure "Expecting: identifier" identifier "64"
        testCase "Numeral-dec-fail" <| fun () -> failure stopError positiveNumeral "123a"
        testCase "Numeral-oct-fail" <| fun () -> failure stopError positiveNumeral "0o18"
        testCase "Numeral-hex-fail" <| fun () -> failure "outside the allowable range" positiveNumeral <| "0x" + String.replicate 17 "f"
    ]

[<Tests>]
let ExpressionTests =
    let x = ELabel "x"
    let y = ELabel "y"
    testList "Expression" [
        testList "Leaf" [
            testCase "Numeral" <| fun () -> success (ENum 17L) expression "17"
            testCase "Label" <| fun () -> success (ELabel "u17_X_") expression "u17_X_"
            testCase "PC" <| fun () -> success EPc expression "$pc"
        ]
        testList "Unary" [
            testCase "Minus" <| fun () -> success (EMinus <| ELabel "x") expression "-x"
            testCase "Negation" <| fun () -> success (ENeg  <| EPc) expression "~$pc"
            testCase "Minus-negation" <| fun () -> success (ENum 0xaL |> ENeg |> EMinus) expression "-~0xa"
            testCase "Peek" <| fun () -> success (ENum 0L |> EPeek) expression "$0"
            testCase "Stack" <| fun () -> success (ELabel "x" |> EStack ) expression "&x"
        ]
        testList "Binary" [
            testCase "Sum" <| fun () -> success (ESum[EPc;  ENum 1L]) expression "(+ $pc 1)"
            testCase "Conjunction" <| fun () -> success (EConj [ENum 0o10L |> EStack;  ELabel "lab"]) expression "( & &0o10 lab)"
            testCase "Product" <| fun () -> success (EProd[ENum 3L |> EPeek;  ESum [ENum 16L; ENum 0L |> EPeek]]) expression "(* $3 (+ 0x10 $0))"
            testCase "Disjunction" <| fun () -> success (EDisj [ENum 8L |> EPeek;  ELabel "lab"]) expression "(|\t$0o10 lab)"
        ]
        testList "Less" [
            testCase "Shift" <| fun () -> success (EShift (x, y)) expression "(<< x y)"
            testCase "Or equal" <| fun () -> success (ELtE (x, y)) expression "(<= x y)"
            testCase "Less than" <| fun () -> success (ELt (x, y)) expression "(< x y)"
        ]
        // TODO: This is more suitable for property based testing
    ]
