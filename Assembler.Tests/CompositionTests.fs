module Assembler.Tests.CompositionTests

open System
open Expecto
open Expecto.Impl
open FsCheck
open Swensen.Unquote

open Assembler.Ast
open Machine.Instructions
open Assembler.Target
open Assembler.Composition

let compilesTo (program: Statement list) (binary: sbyte list) (positions: int list) =
    let b, p = assemble program
    Expect.sequenceEqual b (Seq.map uint8 binary) "Unexpected binary"
    Expect.sequenceEqual p positions "Unexpected positions"

[<Tests>]
let interTests =
    testList "Inter" [
        testCase "Push number" <| fun () ->
            compilesTo [SPush <| ENum 8L]
                       [PUSH1; 8y]
                       [0]
        testCase "Push label after" <| fun () ->
            compilesTo [SPush <| ELabel 1; SLabel 1]
                       [GET_PC]
                       [0; 1]
        testCase "Push label before" <| fun () ->
            compilesTo [SLabel 1; SPush <| ELabel 1]
                       [GET_PC; PUSH0; NOT; ADD]
                       [0; 0]
        testCase "Push label difference" <| fun () ->
            compilesTo [
                            SLabel 1
                            SPush <| ESum [ELabel 2; EMinus <| ELabel 1]
                            SLabel 2
                       ]
                       [PUSH1; 2y]
                       [0; 0; 2]
        testCase "Push SP" <| fun () ->
            compilesTo [ENum 0L |> EStack |> SPush]
                       [GET_STACK]
                       [0]
        testCase "Push SP 3" <| fun () ->
            compilesTo [ENum 3L |> EStack |> SPush]
                       [GET_STACK; PUSH1; 3 * 8 |> sbyte; ADD]
                       [0]
        testCase "Push SP diff" <| fun () ->
            compilesTo [ESum [ENum 12L |> EStack; ENum 2L |> EStack |> EMinus] |> SPush]
                       [PUSH1; 80y]
                       [0]
        testCase "Tight inf loop" <| fun () ->
            compilesTo [SLabel 1; SPush <| ELabel 1; SJump]
                       [PUSH0; JUMP_ZERO; -3y]
                       [0; 0]
        testCase "Small inf loop" <| fun () ->
            compilesTo [SLabel 1; SNeg; SPush <| ELabel 1; SJump]
                       [NOT; PUSH0; JUMP_ZERO; -4y]
                       [0; 0]
    ]
