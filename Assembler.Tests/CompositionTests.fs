module Assembler.Tests.CompositionTests

open Expecto
open Expecto.Impl
open Swensen.Unquote

open Assembler.Ast
open Machine.Instructions
open Assembler.Composition

let compilesTo (program: Statement list) (binary: sbyte list) (positions: int list) =
    let b, p, _, _, _ = assemble program
    Expect.sequenceEqual b (Seq.map uint8 binary) "Unexpected binary"
    Expect.sequenceEqual p (Seq.append [List.length binary ] positions) "Unexpected positions"

[<Tests>]
let interTests =
    testList "Inter" [
        testCase "Push number" <| fun () ->
            compilesTo [SPush <| ENum 8L]
                       [PUSH1; 8y]
                       []
        testCase "Push label after" <| fun () ->
            compilesTo [SPush <| ELabel 1; SLabel 1]
                       [GET_PC]
                       [1]
        testCase "Push label before" <| fun () ->
            compilesTo [SLabel 1; SPush <| ELabel 1]
                       [GET_PC; PUSH0; NOT; ADD]
                       [0]
        testCase "Push label difference" <| fun () ->
            compilesTo [
                            SLabel 1
                            SPush <| ESum [ELabel 2; ENeg <| ELabel 1]
                            SLabel 2
                       ]
                       [PUSH1; 2y]
                       [0; 2]
        testCase "Push SP" <| fun () ->
            compilesTo [ENum 0L |> EStack |> SPush]
                       [GET_SP]
                       []
        testCase "Push SP 3" <| fun () ->
            compilesTo [ENum 3L |> EStack |> SPush]
                       [GET_SP; PUSH1; 3 * 8 |> sbyte; ADD]
                       []
        testCase "Push SP diff" <| fun () ->
            compilesTo [ESum [ENum 12L |> EStack; ENum 2L |> EStack |> ENeg] |> SPush]
                       [PUSH1; 80y]
                       []
        testCase "Tight inf loop" <| fun () ->
            compilesTo [SLabel 1; SPush <| ELabel 1; SJump]
                       [PUSH0; JUMP_ZERO'; 2y]
                       [0]
        testCase "Small inf loop" <| fun () ->
            compilesTo [SLabel 1; SNot; SPush <| ELabel 1; SJump]
                       [NOT; PUSH0; JUMP_ZERO'; 3y]
                       [0]
    ]
