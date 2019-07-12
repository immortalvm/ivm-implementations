module Assembler.Tests.IntegrationTests

open System
open Expecto
open Expecto.Impl
open FsCheck
open Swensen.Unquote

open Assembler.Checker


let check fileName () =
    try
        let message = doCheck <| "test_code/" + fileName
        Expect.isNotMatch message "^Not executed" "Expectations not found"
    with
        | Failure(msg) -> failtest msg

[<Tests>]
let integrationTests =
    testList "Integration" [
        testCase "Push zero" <| check "check_push_0.s"
        testCase "Count down" <| check "check_count_down.s"
        testCase "Complex" <| check "check_complex.s"
    ]
