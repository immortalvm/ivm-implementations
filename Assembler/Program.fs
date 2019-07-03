open System
open FParsec
open Assembler.Parser

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

[<EntryPoint>]
let main argv =
    //test (genWs .>> eof) " \t  # abcd\n  \t #7 \n a"
    test (numeral .>> eof) "17L"
    test numeral "0xffffffffffffffff"
    0 // return an integer exit code
