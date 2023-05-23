module Machine.Tests.ExecutorTests

open Expecto
open Assembler.Ast
open Assembler.Composition
open Machine.Instructions
open Machine.Executor


let endStack program =
    let bin, _, _, _, _ = assemble program
    // Memory size: 64 KiB
    execute (1UL <<< 16) bin [] None None None None |> Seq.map int64

let expectEndStack prog expected () =
    Expect.sequenceEqual (endStack prog) expected "Unexpected end stack"

[<Tests>]
let versionCheckTests =
    testList "Version check tests" [

        testCase "Current version is ok" <| expectEndStack
            [
                SPush (ENum <| int64 VERSION)
                SData1 (0L, ENum <| int64 CHECK)
                SExit
            ]
            []

        testCase "Next version is not ok" <| fun () ->
            Expect.throwsT<VersionException>
                (fun () -> ignore <| endStack [
                        SPush (ENum <| int64 (VERSION + 1UL))
                        SData1 (0L, ENum <| int64 CHECK)
                        SExit
                    ])
                "The next version should not be accepted"
]
