﻿module Assembler.Tests.ProgParserTests

open System.IO
open Expecto
open Expecto.Impl
open FsCheck
open Swensen.Unquote

open FParsec
open Assembler.Ast
open Assembler.Parser


[<Literal>]
let DIRECTORY = "test_code"

let parse (progString: string) =
    let lookup name = match name with
                      | "intro1_statements.my_label"
                      | "intro2_basics.x" -> 999
                      | _ -> -1
    let bytes = System.Text.Encoding.UTF8.GetBytes progString
    let bin, _, _ = parseProgram (State.Init lookup) (new MemoryStream(bytes))
    bin

// Opposite order of Expect.* (for convenience).
let success expected progString () =
    let result = try parse progString with ParseException(msg) -> failtest msg
    Expect.sequenceEqual result expected "Unexpected parsing result"

let failure pattern progString () =
    try parse progString |> failtestf "Success: %O"
    with ParseException(msg) ->
         Expect.isMatch msg pattern "Unexpected error message"

let example1 =
    [
        SExit
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
        SPush <| EStack (ENum -1L)

        SPush <| ESum [
            ENum 7L
            EStack (ENum 2L)
            ELoad8 (EStack (ENum 12L))
        ]
        SLabel 3
        SData [0y; 1y; -1y; 1y]
    ]

let assemblyLanguageIntro =
    let prime = ENum 982451653L
    let n = ENum 7L
    let xx = ENum 99L
    let yy = ENeg (ENum 13L)
    let xx2 = ENum 9L
    [
        SLabel 1
        SData [0y; 1y; -2y; -128y; 1y]

        SPush <| ENum 13L
        SPush <| ENum -1L
        SPush <| ENum 0L; SPush <| ENum 1L
        SPush <| ELabel 1
        SPush prime
        SPush <| EStack n
        SPush <| ELoad8 (EStack n)
        SPush <| ESum [ELabel 1; ENeg (ELoad8 (EStack (ENum 0L)))]

        SJump
        SPush <| ELabel 1; SJump
        SJumpZero
        SPush <| ELabel 1; SJumpZero

        SPush <| ESum [prime; ENeg (ELoad8 (EStack (ENum 4L)))]
        SPush <| ELabel 1; SJumpZero
        SJumpNotZero

        // Call, ...
        SPush <| ELabel 2
        SPush <| ELabel 1; SJump
        SLabel 2
        SJump

        SLoad1; SLoad2; SLoad4; SLoad8
        ELabel 1 |> ELoad4 |> ESigx4 |> SPush
        SPush <| ENum -1L

        SStore1; SStore2; SStore4; SStore8
        SPush <| ELabel 1; SStore4
        SPush prime; SPush <| ELabel 1; SStore8

        SAdd
        SPush xx; SAdd

        SPush <| ENum -(99L - 13L); SAdd
        SPush <| ENum -99L; SAdd
        SPush <| ENum (99L + 13L)
        SMult; SNeg
        SDivU; SDivS; SRemU; SRemS

        SAnd
        SPush <| ENum 127L; SAnd
        SPush <| ENum (982451653L &&& 4095L)
        SOr; SXor; SNot;
        SPow2
        SPow2; SMult
        SPow2; SDivU
        SPow2; SDivSU

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

let parseFile expected (name: string) =
    let caseName = Path.GetFileNameWithoutExtension name
    testCase caseName <| fun () ->
        let fileName = Path.Combine [|DIRECTORY; name|]
        let text = File.ReadAllText fileName
        success expected text ()

[<Tests>]
let ParseFileTests =
    testList "Parse files" [
        parseFile example1 "ex1_old.s"
        parseFile assemblyLanguageIntro "intro1_statements.s"
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
