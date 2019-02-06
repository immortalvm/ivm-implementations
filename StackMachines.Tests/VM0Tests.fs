module VM0Tests

open System
open VM0
open Expecto

open Expecto.Impl
open FsCheck

open Swensen.Unquote

// This file is work in progress.

let setupNoIO code stackContents freeStack : Machine =
    let data = Seq.concat [code; [EXIT] :> seq<int>; stackContents; {1..freeStack}] |> Seq.map uint64 |> Seq.toArray
    let m = Machine(data,
                    seq { for i in 0 .. 10 do raise (Exception "Unexpected input")},
                    fun _ -> raise (Exception "Unexpected output"))
    m.StackPointer <- uint64 (Array.length data - freeStack)
    m

let run (program: int list) (input: string) : string =
    let mutable output: string = ""

    let out (s: seq<uint64>) =
        output <- output + System.Text.UTF8Encoding().GetString(Seq.toArray (Seq.map uint8 s))

    Machine(
        program |> Seq.map uint64,
        Seq.map uint64 input,
        out).Run()

    output

[<Tests>]
let tests =
    testList "VM tests" [
        testProperty "Immediate exit, no output" <| fun (NonNull input) -> run [EXIT] input = ""

        testProperty "Add constant" <| fun m n ->
            let machine = setupNoIO (addC m) [n] 1
            machine.Run()
            test <@ machine.Get (machine.StackPointer - 1UL) = uint64 (m + n) @>
    ]
