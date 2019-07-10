module Assembler.Tests.ProgParserTests

open System
open Expecto
open Expecto.Impl
open FsCheck
open Swensen.Unquote

open FParsec
open Assembler.Ast
open Assembler.Parser


let parse (progString: string) =
    let bytes = System.Text.Encoding.UTF8.GetBytes(progString)
    use stream = new System.IO.MemoryStream(bytes)
    parseProgram stream

// Opposite order of Expect.* (for convenience).
let success expected progString () =
    let result = try parse progString with ParseException(msg) -> failtest msg
    Expect.sequenceEqual result expected "Unexpected parsing result"

let successFile expected filename =
    success expected <| System.IO.File.ReadAllText filename

let failure pattern progString () =
    try parse progString |> failtestf "Success: %O"
    with ParseException(msg) ->
         Expect.isMatch msg pattern "Unexpected error message"

let example1 =
    let n = ENum -13L
    [
        SLabel 1
        SPush <| ENum 1L
        SPush <| ENum 2L; SPush <| ENum 3L

        SPush <| ELabel 1; SJump

        SPush <| ELabel 1; SJump
        SJumpZero
        SPush <| ELabel 1; SJumpZero
        SPush <| ELoad8 (EStack (ENum 3L)); SPush <| ELabel 1; SJumpZero

        SLabel 2
        SPush <| EStack (ENum 0L)
        SPush <| EStack (ENum 1L)

        SPush <| ESum [
            ENum 7L
            EStack (ENum -2L)
            ELoad8 (EStack n)
        ]
        SLabel 3
        SData [0uy; 1uy; 0xffuy; 1uy]
    ]

let assemblyLanguageIntro =
    let prime = ENum 982451653L
    let n = ENum 7L
    let xx = ENum 99L
    let yy = EMinus (ENum 13L)
    let xx2 = ENum 9L
    [
        SLabel 1
        SData [0uy; 1uy; 254uy; 128uy; 1uy]

        SPush <| ENum 13L
        SPush <| ENum -1L
        SPush <| ENum 0L; SPush <| ENum 1L
        SPush <| ELabel 1
        SPush prime
        SPush <| EStack n
        SPush <| ELoad8 (EStack n)
        SPush <| ESum [ELabel 1; EMinus (ELoad8 (EStack (ENum 0L)))]

        SJump
        SPush <| ELabel 1; SJump
        SJumpZero
        SPush <| ELabel 1; SJumpZero

        SPush <| ESum [prime; EMinus (ELoad8 (EStack (ENum 4L)))]
        SPush <| ELabel 1; SJumpZero
        SJumpNotZero

        SLoad1; SLoad2; SLoad4; SLoad8
        ELabel 1 |> ELoad4 |> ESign4 |> SPush
        SPush <| ENum -1L

        SStore1; SStore2; SStore4; SStore8
        SPush <| ELabel 1; SStore4
        SPush prime; SPush <| ELabel 1; SStore8

        SAdd
        SPush xx; SAdd

        SPush <| ENum -(99L - 13L); SAdd
        SPush <| ENum -99L; SAdd
        SPush <| ENum (99L + 13L)
        SMult; SMinus
        SDivU; SDivS; SRemU; SRemS

        SAnd
        SPush <| ENum 127L; SAnd
        SPush <| ENum (982451653L &&& 4095L)
        SOr; SXor; SNeg;
        SPow2
        SPow2; SMult
        SPow2; SDivU
        SPow2; SDivS

        SEq
        SPush <| ENum 7L; SEq
        SPush <| ENum 0L // SPush xx; SPush yy; SEq
        SLtU; SLtS; SLtEU; SLtES
        SGtU; SGtS; SGtEU; SGtES

        SAlloc
        SPush prime; SAlloc
        SDealloc
        SPush <| ELoad8 (EStack (ENum 8L)); SDealloc
        SSetSp
        SPush xx2; SSetSp
        SExit
    ]

[<Tests>]
let ExampleFileTests =
    testList "Example files" [
        testCase "Example 1" <| successFile example1 "test_code/example1.s"
        testCase "Intro" <| successFile assemblyLanguageIntro "test_code/assembly_language_intro.s"
    ]

[<Tests>]
let ErrorMessageTests =
    testList "Error messages" [
        testCase "Undefined label" <| failure "^Label not found: xx$" "push! xx"
        // ...
    ]

[<Tests>]
let BasicTests =
    testList "Prog basics" [
        testCase "Offset num" <|
            success
                ([0..3] |> List.map (int64 >> ENum >> EStack >> ELoad8 >> SPush))
                "push!!!! $0 $0 $0 $0"
        testCase "Offset sum" <|
            success
                [
                    SLabel 1
                    ENum 0L |> SPush
                    [ENum 1L; ELabel 1; ELabel 1] |> ESum |> EStack |> SPush
                ]
                "x: push!! 0 &(+ x x)"
    ]

// -- TODO: Move to separate file

open Assembler.Composition

let compilesTo (program: Statement list) (binary: sbyte list) (positions: int list) =
    let b, p = program |> intermediates |> Seq.toList |> compose nopsFor
    Expect.sequenceEqual b binary "Unexpected binary"
    Expect.sequenceEqual p positions "Unexpected positions"


[<Tests>]
let interTests =
    testList "Inter" [
        testCase "Push number" <| fun () ->
            compilesTo [SPush <| ENum 8L]
                       [PUSH1; 8y]
                       [0]
        testCase "Push label after" <| fun () ->
            compilesTo [SPush <| ELabel 1; SLabel 1]
                       [GET_PC]
                       [0; 1]
        testCase "Push label before" <| fun () ->
            compilesTo [SLabel 1; SPush <| ELabel 1]
                       [GET_PC; PUSH0; NOT; ADD]
                       [0; 0]
        testCase "Push label difference" <| fun () ->
            compilesTo [
                            SLabel 1
                            SPush <| ESum [ELabel 2; EMinus <| ELabel 1]
                            SLabel 2
                       ]
                       [PUSH1; 2y]
                       [0; 0; 2]
        testCase "Push SP" <| fun () ->
            compilesTo [ENum 0L |> EStack |> SPush]
                       [GET_STACK]
                       [0]
        testCase "Push SP 3" <| fun () ->
            compilesTo [ENum 3L |> EStack |> SPush]
                       [GET_STACK; PUSH1; 3 * 8 |> sbyte; ADD]
                       [0]
        testCase "Push SP diff" <| fun () ->
            compilesTo [ESum [ENum 12L |> EStack; ENum 2L |> EStack |> EMinus] |> SPush]
                       [PUSH1; 80y]
                       [0]
        testCase "Tight inf loop" <| fun () ->
            compilesTo [SLabel 1; SPush <| ELabel 1; SJump]
                       [PUSH0; JUMP_IF_ZERO; -3y]
                       [0; 0]
        testCase "Small inf loop" <| fun () ->
            compilesTo [SLabel 1; SNeg; SPush <| ELabel 1; SJump]
                       [NOT; PUSH0; JUMP_IF_ZERO; -4y]
                       [0; 0]
    ]
