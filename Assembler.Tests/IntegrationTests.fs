module Assembler.Tests.IntegrationTests

open System
open Expecto
open Expecto.Impl
open FsCheck
open Swensen.Unquote

open Assembler.Checker


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
    let shouldCheck name =
        System.Text.RegularExpressions.Regex.IsMatch(name, "^check_.*\\.s$")
    let case (name : string) =
        let caseName = System.IO.Path.GetFileNameWithoutExtension name
        let fileName = System.IO.Path.Combine([|DIRECTORY; name|])
        testCase caseName <| fun () -> check fileName
    System.IO.Directory.EnumerateFiles DIRECTORY
    |> Seq.map System.IO.Path.GetFileName
    |> Seq.filter shouldCheck
    |> Seq.map case
    |> Seq.toList
    |> testList "Integration"
