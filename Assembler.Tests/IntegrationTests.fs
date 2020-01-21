module Assembler.Tests.IntegrationTests

open System.IO
open Expecto
open Expecto.Impl
open FsCheck
open Swensen.Unquote

open Tools.Checks


[<Literal>]
let DIRECTORY = "test_code"

let noExpectations = [
    "intro3_advanced.s"
    "ex4_short_video.s"
    "test_linking2.s"
    "test_linking3.s"
    "test_circular2.s"
]

let check fileName =
    try
        let message = doCheck [fileName] (Some <| Path.GetDirectoryName fileName) [] false
        Expect.isNotMatch message "^Not executed" "Expectations not found"
    with
        | Failure(msg) -> failtest msg

[<Tests>]
let integrationTests =
    let case (name : string) =
        let caseName = Path.GetFileNameWithoutExtension name
        let fileName = Path.Combine [|DIRECTORY; name|]
        testCase caseName <| fun () -> check fileName
    Directory.EnumerateFiles DIRECTORY
    |> Seq.map Path.GetFileName
    |> Seq.filter (fun name -> not <| List.contains name noExpectations)
    |> Seq.map case
    |> Seq.toList
    |> testList DIRECTORY
