module Assembler.Tests.ProgParserTests

open System.IO
open Expecto
open Expecto.Impl
open Swensen.Unquote

open FParsec
open Assembler.Ast
open Assembler.Parser


[<Literal>]
let DIRECTORY = "test_code"

let parse (progString: string) =
    let lookup name = match name with
                      | "intro1_statements/my_label"
                      | "intro2_basics/x" -> Some (true, 999L)
                      | _ -> None
    let bytes = System.Text.Encoding.UTF8.GetBytes progString
    let bin, _, _ = parseProgram [("source", fun () -> [], upcast (new MemoryStream(bytes)))] lookup false
    bin

// Opposite order of Expect.* (for convenience).
let success expected progString () =
    let result = try parse progString with ParseException(msg) -> failtest msg
    Expect.sequenceEqual result expected "Unexpected parsing result"

let failure pattern progString () =
    try parse progString |> failtestf "Success: %O"
    with ParseException(msg) ->
         Expect.isMatch msg pattern "Unexpected error message"

let data1 line values = [for x in values -> SData1 (int64 line, ENum <| int64 x)]
let data2 line values = [for x in values -> SData2 (int64 line, ENum <| int64 x)]
let data4 line values = [for x in values -> SData4 (int64 line, ENum <| int64 x)]
let data8 line values = [for x in values -> SData8 (int64 line, ENum x)]

let example1 =
    [
        SExit
        SLabel 2
        SPush <| ENum 1L
        SPush <| ENum 2L; SPush <| ENum 3L

        SPush <| ELabel 2; SJump

        SPush <| ELabel 2; SJump
        SJumpZero
        SPush <| ELabel 2; SJumpZero
        SPush <| ELoad8 (EStack (ENum 3L)); SPush <| ELabel 2; SJumpZero

        SLabel 3
        SPush <| EStack (ENum 0L)
        SPush <| EStack (ENum -1L)

        SPush <| ESum [
            ENum 7L
            EStack (ENum 2L)
            ELoad8 (EStack (ENum 12L))
        ]
        SLabel 5
    ] @ data1 36 [0; 1; 255; -255]


let intro1 =
    let prime = ENum 982451653L
    let n = ENum 7L
    let pushSeven = [for i in 1..7 -> i |> int64 |> ENum |> SPush]

    let xx = ENum 99L
    let yy = ENeg (ENum 13L)
    let xx2 = ENum 9L
    [
        SExport (36L, 2, ELabel 2)
        SExport (55L, 4, ELabel 2)
        SLabel 2
    ]
    @ data1 55 [0; 1; -2; 128; -0x99ff]
    @ data2 59 [0x1000; 0x2000; 0x3000]
    @ data4 62 [0x40000000]
    @ data8 63 [-0x0123456789abcdefL]
    @ [ SLabel 5; SLabel 6; SSpacer (74L, ENum 1000L); SData8 (0L, ENum 0L)]
    @ [

        SPush <| ENum 13L
        SPush <| ENum -1L
        SPush <| ENum 0L; SPush <| ENum 1L
        SPush <| ELabel 2
        SPush prime
        SPush <| EStack n
        SPush <| ELoad8 (EStack n)
        SPush <| ESum [ELabel 2; ENeg (ELoad8 (EStack (ENum 0L)))]
    ]
    @ pushSeven
    @ pushSeven
    @ [
        SJump
        SPush <| ELabel 2; SJump
        SJumpZero
        SPush <| ELabel 2; SJumpZero

        SPush <| ESum [prime; ENeg (ELoad8 (EStack (ENum 4L)))]
        SPush <| ELabel 2; SJumpZero
        SJumpNotZero

        // Call, ...
        SPush <| ELabel 8
        SPush <| ELabel 2; SJump
        SLabel 8
        SJump

        SLoad1; SLoad2; SLoad4; SLoad8
        ELabel 2 |> ELoad4 |> ESigx4 |> SPush
        SPush <| ENum -1L

        SStore1; SStore2; SStore4; SStore8
        SPush <| ELabel 2; SStore4
        SPush prime; SPush <| ELabel 2; SStore8

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
        parseFile intro1 "intro1_statements.s"
    ]

[<Tests>]
let ErrorMessageTests =
    testList "Error messages" [
        testCase "Undefined label" <| failure "^Label not found: source/xx$" "push! xx"
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

        testCase "Check version" <|
            success
                [
                    Machine.Instructions.VERSION |> int64 |> ENum |> SPush
                    SCheck
                ]
                "check_version"
    ]
