module Machine.Tests.ExecutorTests

open Expecto
open Machine.Instructions
open Machine.Executor


let endStack prog =
    // Memory size: 64 KiB
    execute (1UL <<< 16) (Seq.map uint8 prog) [] None None None None |> Seq.map int64

let expectEndStack prog expected () =
    Expect.sequenceEqual (endStack prog) expected "Unexpected end stack"

[<Tests>]
let basicTests =
    testList "Basics" [
        testCase "Push zero" <| expectEndStack [PUSH0; EXIT] [0L]
    ]