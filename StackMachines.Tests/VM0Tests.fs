module VM0Tests

open System
open Expecto
open Expecto.Impl
open FsCheck
open Swensen.Unquote

open VM0


let runNoIO code stackContents freeStack : Machine * int list =
    let data = Seq.concat [code @ [EXIT] :> seq<int>; stackContents |> Seq.rev; {1..freeStack}]
               |> Seq.map uint32 |> Seq.toArray
    let stackBottom = List.length code + 1
    let input = seq { for i in 0 .. 0 do raise (Exception "Unexpected input")}
    let output = fun _ -> raise (Exception "Unexpected output")
    let mac = Machine(data, input, output)
    mac.StackPointer <- uint32 <| Array.length data - freeStack
    mac.Run()
    (mac, int mac.StackPointer - stackBottom |> mac.Stack |> Seq.rev |> Seq.toList)

let runStack c s f = runNoIO c s f |> snd

// Not in use yet.
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

        testProperty "Swap" <| fun (stack: int list) ->
            stack <> [] ==> lazy (
                (0, List.length stack - 1) |> Gen.choose |> Gen.two |> Arb.fromGen |> Prop.forAll <| fun (i, j) ->
                    let a = List.toArray stack
                    arraySwap a i j
                    test <@ runStack (swap i j) stack 4 = Array.toList a @>)

        testProperty "switchJump" <| fun x ->
            test <@ runStack (switchJump 1 3 @ [EXIT; PUSH; 0; PUSH; 0]) [x] 3
                     |> List.length = if x=0 then 2 else 1 @>

        testProperty "storeLiterally" <| fun (data: int list) ->
            let n = List.length data
            let z = List.replicate n 0
            test <@ runStack (addr (n-1) @ storeLiterally data) z 6 = List.rev data @>
    ]
