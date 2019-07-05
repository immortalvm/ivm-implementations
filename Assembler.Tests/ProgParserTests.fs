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
    let n = EMinus (ENum 13L)
    [
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

let assemblyLanguageIntro =
    let prime = ENum 982451653L
    let n = ENum 7L
    let xx = ENum 99L
    let yy = EMinus (ENum 13L)
    let xx2 = ENum 9L
    [
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
