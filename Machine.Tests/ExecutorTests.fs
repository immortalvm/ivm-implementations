module Machine.Tests.ExecutorTests

open System
open Expecto
open Expecto.Impl
open Swensen.Unquote

open Machine.Instructions
open Machine.Executor


let random = System.Random ()

let endStack prog =
    // Memory size: 64 KiB
    execute (1UL <<< 16) (Seq.map uint8 prog) [] None None None None |> Seq.map int64

let expectEndStack prog expected () =
    Expect.sequenceEqual (endStack prog) expected "Unexpected end stack"

[<Tests>]
let basicTests =
    testList "Basics" [
        testCase "Push zero" <| expectEndStack [PUSH0; EXIT] [0L]
        testCase "Version 0 ok" <| expectEndStack [PUSH0; CHECK; EXIT] []
        testCase "Version 1 not ok" <| fun () ->
            Expect.throwsT<VersionException>
                (fun () -> endStack [PUSH1; 1y; CHECK; EXIT] |> ignore)
                "Version 1 was accepted"
    ]