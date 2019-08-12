module Machine.Tests.ExecutorTests

open System
open Expecto
open Expecto.Impl
open FsCheck
open Swensen.Unquote

open Machine.Instructions
open Machine.Executor


let random = System.Random ()

let endStack prog expected () =
    let stackSpace = Array.create 1000 0uy
    random.NextBytes (Span stackSpace)
    let actual = execute (Seq.map uint8 prog) stackSpace None |> Seq.map int64
    Expect.sequenceEqual actual expected "Unexpected end stack"

[<Tests>]
let basicTests =
    testList "Basics" [
        testCase "Push zero" <| endStack [PUSH0; EXIT] [0L]
    ]
