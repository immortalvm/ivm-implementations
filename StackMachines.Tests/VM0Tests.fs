module VM0Tests

open System
open VM0
open Expecto

open Expecto.Impl
open FsCheck

open Swensen.Unquote

// This file is work in progress.

let runNoIO code stackContents freeStack : Machine * int list =
    let data = Seq.concat [code @ [EXIT] :> seq<int>; stackContents |> Seq.rev; {1..freeStack}]
               |> Seq.map uint32 |> Seq.toArray
    let stackBottom = List.length code + 1
    let input = seq { for i in 0 .. 0 do raise (Exception "Unexpected input")}
    let output = fun _ -> raise (Exception "Unexpected output")
    let mac = Machine(data, input, output)
    mac.StackPointer <- Array.length data - freeStack
    mac.Run()
    (mac, mac.StackPointer - stackBottom |> mac.Stack |> Seq.rev |> Seq.toList)

let runStack c s f = runNoIO c s f |> snd

let runIO (program: int list) (input: string) : string =
    let mutable output: string = ""

    let out (s: seq<uint32>) =
        output <- output + System.Text.UTF8Encoding().GetString(Seq.toArray (Seq.map uint8 s))

    Machine(
        program |> Seq.map uint32,
        Seq.map uint32 input,
        out).Run()

    output

let arraySwap (a: int array) i j =
    let x = a.[i]
    a.[i] <- a.[j]
    a.[j] <- x

[<Tests>]
let tests =
    testList "VM tests" [
        testProperty "Immediate exit" <| fun () -> test <@ runStack [] [] 0 |> List.isEmpty @>
        testProperty "Add constant" <| fun m n -> test <@ runStack (addC m) [n] 1 = [m + n] @>
        testProperty "Subtraction" <| fun m n -> test <@ runStack subtract [n; m] 1 = [m - n] @>
        testProperty "Swap" <| fun (stack : int list) ->
            stack <> [] ==> lazy (
                (0, List.length stack - 1) |> Gen.choose |> Gen.two |> Arb.fromGen |> Prop.forAll <| fun (i, j) ->
                    let a = List.toArray stack
                    arraySwap a i j
                    test <@ runStack (swap i j) stack 6 = Array.toList a @>)
    ]
