module Assembler.Tests.IntegrationTests

open System.IO
open Expecto
open Expecto.Impl
open Swensen.Unquote

open Tools.Checks


[<Literal>]
let DIRECTORY = "test_code"

let noExpectations = [
    "ex4_short_video.s"
    "test_linking2.s"
    "test_linking3.s"
    "test_circular2.s"
]

let sourceFiles dir =
    Directory.EnumerateFiles dir
    |> Seq.filter (Path.GetExtension >> (=) ".s")
    |> Seq.map Path.GetFileName

let check fileNames =
    try
        let message = doCheck fileNames (Some DIRECTORY) [] None None false None false
        Expect.isNotMatch message "^Not executed" "Expectations not found"
    with
        | Failure(msg) -> failtest msg

[<Tests>]
let integrationTests =
    let case (name : string) =
        let caseName = Path.GetFileNameWithoutExtension name
        let fileName = Path.Combine [|DIRECTORY; name|]
        testCase caseName <| fun () -> check [fileName]
    sourceFiles DIRECTORY
    |> Seq.filter (fun name -> not <| List.contains name noExpectations)
    |> Seq.map case
    |> Seq.toList
    |> testList DIRECTORY

[<Literal>]
let IMPL_DIRECTORY = "test_code/Implicit_imports"

[<Tests>]
let implicitImportTests =
    let filenames = List.map (fun name -> Path.Combine (IMPL_DIRECTORY, name)) ["test_circular1.s"; "test_circular2.s"]
    testList IMPL_DIRECTORY [
        testCase "circular" <| fun () ->
            try
                let message = doCheck filenames None [] None None false None false
                Expect.isNotMatch message "^Not executed" "Expectations not found"
            with
                | Failure(msg) -> failtest msg
    ]

[<Literal>]
let ENTRY_DIRECTORY = "test_code/entry_points"

[<Tests>]
let entryPointTests =
    let case (name : string) =
        let caseName = Path.GetFileNameWithoutExtension name
        let fileName = Path.Combine [|ENTRY_DIRECTORY; name|]
        testCase caseName <| fun () ->
            try
                let message = doCheck [fileName] None [] (Some "main") None false None false
                Expect.isNotMatch message "^Not executed" "Expectations not found"
            with
                | Failure(msg) -> failtest msg
    sourceFiles ENTRY_DIRECTORY
    |> Seq.map case
    |> Seq.toList
    |> testList ENTRY_DIRECTORY
