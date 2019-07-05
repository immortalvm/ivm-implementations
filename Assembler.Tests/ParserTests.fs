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
    match runParserOnString(parser .>> eof) State.Default "" input with
    | Success(result, _, _)   -> test <@ result = expected @>
    | Failure(errorMsg, _, _) -> failwith <| sprintf "Failure: %s" errorMsg

let failure pattern parser input =
    match runParserOnString(parser .>> eof) State.Default "" input with
    | Success(result, _, _)   -> sprintf "Success: %O" result |> failwith
    | Failure(errorMsg, _, _) -> Expect.isMatch errorMsg pattern
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
    let pc = ELabel "pc" |> EPeek // Quick and dirty
    testList "Expression" [
        testList "Leaf" [
            testCase "Numeral" <| fun () -> success (ENum 17L) expression "17"
            testCase "Label" <| fun () -> success (ELabel "u17_X_") expression "u17_X_"
            testCase "PC" <| fun () -> success pc expression "$pc"
        ]
        testList "Unary" [
            testCase "Minus" <| fun () -> success (EMinus <| ELabel "x") expression "-x"
            testCase "Negation" <| fun () -> success (ENeg  <| pc) expression "~$pc"
            testCase "Minus-negation" <| fun () -> success (ENum 0xaL |> ENeg |> EMinus) expression "-~0xa"
            testCase "Peek" <| fun () -> success (ENum 0L |> EPeek) expression "$0"
            testCase "Stack" <| fun () -> success (ELabel "x" |> EStack ) expression "&x"
        ]
        testList "Binary" [
            testCase "Sum" <| fun () -> success (ESum[pc;  ENum 1L]) expression "(+ $pc 1)"
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

[<Tests>]
let StatementTests =
    let str = System.IO.File.ReadAllText "test_code/example1.s"
    let n = EMinus (ENum 13L)
    let expected = [
        SLabel "label1"
        SPush [ENum 1L]
        SPush [ENum 2L; ENum 3L]
        SPush [ELabel "label1"]

        SJump
        SPush [ELabel "label1"]; SJump
        SJumpZero
        SPush [ELabel "label1"]; SJumpZero
        SPush [EPeek (ENum 3L); ELabel "label1"]; SJumpZero

        SLabel "label2"
        SPush [EStack (ENum 0L)]
        SPush [EStack (ENum 1L)]
        SPush [EStack (EMinus (ENum 2L))]
        SPush [EPeek n]
        SAdd
        SPush [ENum 7L]; SAdd
        SLabel "label3"
        SData [0uy; 1uy; 0xffuy; 1uy]
    ]
    testList "Examples" [
        testCase "Example 1" <| fun () -> success expected program str
    ]

[<Tests>]
let IntroTests =
    let prime = ENum 982451653L
    let n = ENum 7L
    let xx = ENum 99L
    let yy = EMinus (ENum 13L)
    let xx2 = ENum 9L
    let expected = [
        SLabel "my_label"
        SData [0uy; 1uy; 254uy; 128uy; 1uy]

        SPush [ENum 13L]
        SPush [EMinus (ENum 1L)]
        SPush [ENum 0L; ENum 1L]
        SPush [ELabel "my_label"]
        SPush [prime]
        SPush [EStack n]
        SPush [EPeek n]
        SPush [ESum [ELabel "my_label"; EMinus (EPeek (ENum 0L))]]

        SJump
        SPush [ELabel "my_label"]; SJump
        SJumpZero
        SPush [ELabel "my_label"]; SJumpZero

        SPush [ESum [prime
                     EMinus (EPeek (ENum 4L))]
               ELabel "my_label"]
        SJumpZero
        SJumpNotZero

        SLoad1; SLoad2; SLoad4; SLoad8
        SPush [ELabel "my_label"]; SLoad4
        SSign4
        SPush [ENum 255L]; SSign1

        SStore1; SStore2; SStore4; SStore8
        SPush [ELabel "my_label"]; SStore4
        SPush [prime; ELabel "my_label"]; SStore8

        SAdd
        SPush [xx]; SAdd
        SPush [xx; yy]; SAdd
        SSub
        SPush [xx]; SSub
        SPush [xx; yy]; SSub
        SMult; SMinus
        SDivU; SDivS; SRemU; SRemS

        SAnd
        SPush [ENum 127L]; SAnd
        SPush [ENum 4095L; prime]; SAnd
        SOr; SXor; SNeg
        SPush [EMinus (ENum 4L)]; SShift
        SPush [EMinus (ENum 4L)]; SShiftS

        SEq
        SPush [ENum 7L]; SEq
        SPush [xx; yy]; SEq
        SLtU; SLtS; SLtEU; SLtES
        SGtU; SGtS; SGtEU; SGtES

        SAlloc
        SPush [prime]; SAlloc
        SDealloc
        SPush [EPeek (ENum 8L)]; SDealloc
        SSetSp
        SPush [xx2]; SSetSp
        SExit
    ]
    let str = System.IO.File.ReadAllText "test_code/assembly_language_intro.s"
    testList "Intro" [
        testCase "Intro" <| fun () -> success expected program str
    ]
