module Assembler.Tests.IntegrationTests

open System.IO
open Expecto
open Expecto.Impl
open FsCheck
open Swensen.Unquote

open Tools.Checks


[<Literal>]
let DIRECTORY = "test_code"

let check fileName =
    try
        let message = doCheck <| fileName
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
    |> Seq.map case
    |> Seq.toList
    |> testList DIRECTORY
