module CommonVmTests

open System
open Expecto
open Expecto.Impl
open FsCheck
open Swensen.Unquote

open VmBase


let arraySwap (a: int array) i j =
    let x = a.[i]
    a.[i] <- a.[j]
    a.[j] <- x

let commonTests vmName (a: BaseArchitecture) =

    let runNoIO code stackContents freeStack : BaseMachine * int list =
        let data = Seq.concat [code @ a.Exit :> seq<int>; stackContents |> Seq.rev; {1..freeStack}]
                   |> Seq.map uint32 |> Seq.toArray
        let stackBottom = List.length code + 1
        let input = seq { for i in 0 .. 0 do raise (Exception "Unexpected input")}
        let output = fun _ -> raise (Exception "Unexpected output")
        let mac = a.CreateCore data input output
        mac.StackPointer <- uint32 <| Array.length data - freeStack
        mac.Run()
        (mac, int mac.StackPointer - stackBottom |> mac.Stack |> Seq.rev |> Seq.toList)

    let runStack c s f = runNoIO c s f |> snd

    testList (vmName + " tests") [
        testProperty "Immediate exit" <| fun () -> test <@ runStack [] [] 0 |> List.isEmpty @>
        testProperty "Add constant" <| fun m n -> test <@ runStack (a.AddC m) [n] 1 = [m + n] @>
        testProperty "Subtraction" <| fun m n -> test <@ runStack a.Subtract [n; m] 1 = [m - n] @>

        testProperty "Swap" <| fun (stack: int list) ->
            stack <> [] ==> lazy (
                (0, List.length stack - 1) |> Gen.choose |> Gen.two |> Arb.fromGen |> Prop.forAll <| fun (i, j) ->
                    let arr = List.toArray stack
                    arraySwap arr i j
                    test <@ runStack (a.Swap i j) stack 4 = Array.toList arr @>)

        testProperty "JumpIfZero" <| fun x ->
            let e = a.Exit
            let n = List.length e
            test <@ runStack (a.JumpIfZero n @ e @ a.Push 0) [x] 3
                     |> List.length = if x=0 then 1 else 0 @>

        testProperty "storeLiterally" <| fun (data: int list) ->
            let n = List.length data
            let z = List.replicate n 0
            test <@ runStack (a.Addr (n-1) @ a.StoreLiterally data) z 6 = List.rev data @>
    ]

[<Tests>]
let vm0Tests = commonTests "VM0" (VM0.Architecture() :> BaseArchitecture)

[<Tests>]
let vm1Tests = commonTests "VM1" (VM1.Architecture(25) :> BaseArchitecture)
