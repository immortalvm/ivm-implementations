open System

[<EntryPoint>]
let main argv =
    VM0.Architecture() |> VmBase.Examples.example1
    VM1.Architecture() |> VmBase.Examples.example1
    0 // return an integer exit code
