module Assembler.Tests.GenericTests

open System
open Expecto
open Expecto.Impl
open FsCheck
open Swensen.Unquote

open FParsec
open Assembler.Ast
open Assembler.Parser

// Opposite order of Expect.* (for convenience).
let success expected parser input () =
    match runParserOnString (parser .>> eof) State.Default "" input with
    | Success(result, _, _)   -> test <@ result = expected @>
    | Failure(errorMsg, _, _) -> failtestf "Failure: %s" errorMsg

let failure pattern parser input () =
    match runParserOnString (parser .>> eof) State.Default "" input with
    | Success(result, _, _)   -> failtestf "Success: %O" result
    | Failure(errorMsg, _, _) -> Expect.isMatch errorMsg pattern "Unexpected error message"

[<Tests>]
let BasicTests =
    let stopError = "Expecting: end of input or '#'"
    testList "Basics" [
        testCase "Comment" <| success () comment "# abc\n"
        testCase "Whitespace" <| success () whitespace " \r# abcd\n  \t #7"
        testCase "Identifier" <| success "xY_123" identifier "xY_123  # comment"
        testCase "Numeral-dec" <| success  1234L positiveNumeral "1234 # comment"
        testCase "Numeral-oct" <| success  08L positiveNumeral "0o10 # comment"
        testCase "Numeral-hex-big"<| success -1L positiveNumeral ("0x" + String.replicate 16 "f")

        testCase "Comment-fail" <| failure "Expecting: '#'" comment "not a comment"
        testCase "Whitespace-fail" <| failure stopError whitespace "#\n777"
        testCase "Identifier-fail" <| failure "Expecting: identifier" identifier "64"
        testCase "Numeral-dec-fail" <| failure stopError positiveNumeral "123a"
        testCase "Numeral-oct-fail" <| failure stopError positiveNumeral "0o18"
        testCase "Numeral-hex-fail" <| failure "outside the allowable range" positiveNumeral ("0x" + String.replicate 17 "f")
    ]

[<Tests>]
let ExpressionTests =
    let x = ELabel "x"
    let y = ELabel "y"
    let pc = ELabel "pc" |> EStack |> ELoad8
    testList "Expression" [
        testList "Leaf" [
            testCase "Numeral" <| success (ENum 17L) expression "17"
            testCase "Label" <| success (ELabel "u17_X_") expression "u17_X_"
            testCase "PC" <| success pc expression "$pc"
        ]
        testList "Unary" [
            testCase "Minus" <| success (EMinus <| ELabel "x") expression "-x"
            testCase "Negation" <| success (ENeg  <| pc) expression "~$pc"
            testCase "Minus-negation" <| success (ENum 0xaL |> ENeg |> EMinus) expression "-~0xa"
            testCase "Peek" <| success (ENum 0L |> EStack |> ELoad8) expression "$0"
            testCase "Stack" <| success (ELabel "x" |> EStack ) expression "&x"
        ]
        testList "Binary" [
            testCase "Sum" <| success (ESum[pc;  ENum 1L]) expression "(+ $pc 1)"
            testCase "Conjunction" <| success (EConj [ENum 0o10L |> EStack;  ELabel "lab"]) expression "( & &0o10 lab)"
            testCase "Product" <| success (EProd[ENum 3L|> EStack |> ELoad8;  ESum [ENum 16L; ENum 0L|> EStack |> ELoad8]]) expression "(* $3 (+ 0x10 $0))"
            testCase "Disjunction" <| success (EDisj [ENum 8L|> EStack |> ELoad8;  ELabel "lab"]) expression "(|\t$0o10 lab)"
        ]
        testList "Less" [
            testCase "Shift" <| success (EShift (x, y)) expression "(<< x y)"
            testCase "Or equal" <| success (ELtE (x, y)) expression "(<= x y)"
            testCase "Less than" <| success (ELt (x, y)) expression "(< x y)"
        ]
        // TODO: This is more suitable for property based testing
    ]
