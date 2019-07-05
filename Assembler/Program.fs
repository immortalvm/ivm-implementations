open System
open FParsec
open Assembler.Parser

let test p str =
    match runParserOnString p State.Default "" str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

[<EntryPoint>]
let main argv =
    test statement "jump! xyz"
    0 // return an integer exit code
