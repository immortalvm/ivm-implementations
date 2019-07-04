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
    let expected = [
        SLabel "label1"
        SPush []
        SPush [ENum 1L]
        SPush [ENum 2L; ENum 3L]
        SPush [ELabel "label1"]
        SJump None
        SJump (Some (ELabel "label1"))
        SJumpZero None
        SJumpZero (Some (ELabel "label1", None))
        SJumpZero (Some (EPeek (ENum 3L), Some (ELabel "label1")))
        SLabel "label2"
        SPush [EStack (ENum 0L)]
        SPush [EStack (ENum 1L)]
        SPush [EStack (EMinus (ENum 2L))]
        SDef ("n", EMinus (ENum 13L))
        SPush [EPeek (ELabel "n")]
        SAdd None
        SAdd (Some (ENum 7L, None))
        SLabel "label3"
        SData [ 0uy; 1uy; 0xffuy; 1uy ]
    ]
    testList "Examples" [
        testCase "Example 1" <| fun () -> success expected program str
    ]

[<Tests>]
let IntroTests =
    let str = System.IO.File.ReadAllText "test_code/assembly_language_intro.s"
    let expected = [
        SLabel "my_label"
        SDef ("prime_number",ENum 982451653L)
        SData [0uy; 1uy; 254uy; 128uy; 1uy]

        SPush [ENum 13L]
        SPush [EMinus (ENum 1L)]
        SPush [ENum 0L; ENum 1L]
        SPush []
        SPush [ELabel "my_label"]
        SPush [ELabel "prime_number"]
        SDef ("n",ENum 7L)
        SPush [EStack (ELabel "n")]
        SPush [EPeek (ELabel "n")]
        SPush [ESum [ELabel "my_label"; EMinus (EPeek (ENum 0L))]]

        SJump None
        SJump (Some (ELabel "my_label"))
        SJumpZero None
        SJumpZero (Some (ELabel "my_label", None))
        SJumpZero
          (Some
             (ESum [ELabel "myprime"; EMinus (EPeek (ENum 4L))],
              Some (ELabel "my_label")))
        SLoad1 None
        SLoad2 None
        SLoad4 None
        SLoad8 None
        SLoad4 (Some (ELabel "my_label"))
        SSign4 None
        SSign1 (Some (ENum 255L))
        SStore1 None
        SStore2 None
        SStore4 None
        SStore8 None
        SStore4 (Some (ELabel "my_label", None))
        SStore8 (Some (ELabel "prime_number", Some (ELabel "my_label")))

        SDef ("xx",ENum 99L)
        SDef ("yy",EMinus (ENum 13L))
        SAdd None
        SAdd (Some (ELabel "xx", None))
        SAdd (Some (ELabel "xx", Some (ELabel "yy")))
        SSub None
        SSub (Some (ELabel "xx", None))
        SSub (Some (ELabel "xx", Some (ELabel "yy")))
        SMult None
        SMinus None
        SDivU None
        SDivS None
        SRemU None
        SRemS None
        SAnd None
        SAnd (Some (ENum 127L, None))
        SAnd (Some (ENum 4095L, Some (ELabel "prime_number")))
        SOr None
        SXor None
        SNeg None
        SShift (Some (EMinus (ENum 4L), None))
        SShift (Some (EMinus (ENum 4L), None))
        SEq None
        SEq (Some (ENum 7L, None))
        SEq (Some (ELabel "xx", Some (ELabel "yy")))
        SLtU None
        SLtS None
        SLtEU None
        SLtES None
        SGtU None
        SGtS None
        SGtEU None
        SGtES None
        SAlloc None
        SAlloc (Some (ELabel "my_prime"))
        SDealloc None
        SDealloc (Some (EPeek (ENum 8L)))
        SSetSp None
        SSetSp (Some (ELabel "xx"))
        SExit
    ]
    testList "Intro" [
        testCase "Intro" <| fun () -> success expected program str
    ]
