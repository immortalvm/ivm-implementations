module Assembler.Parser
open FParsec
open Assembler.Ast

let comment: Parser<unit, unit> = skipChar '#' >>. skipRestOfLine true
let whitespace = skipSepBy spaces comment

// Based on http://www.quanttec.com/fparsec/tutorial.html#parsing-string-data.
let identifier : Parser<string, unit> =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'
    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
    .>> whitespace // Skip trailing whitespace

let positiveNumeral: Parser<int64, unit> = puint64 .>> whitespace |>> int64

let strWs s = skipString s >>. whitespace

// Not used  yet. Postpone until the labels can also be evaluated.
let splitNum f g e = match e with ENum n -> f n |> ENum | _ -> g e
let splitNums f g lst =
    let num e = match e with ENum n -> Some n | _ -> None
    let o = List.map num lst
    if List.forall Option.isSome o
    then List.map Option.get o |> f |> ENum
    else g lst

let expression: Parser<Expression, unit> =
    let eParser, eParserRef = createParserForwardedToRef<Expression, unit>()
    // Why do we need 'do' here?
    do eParserRef := choice [
        identifier |>> ELabel
        positiveNumeral |>> ENum
        skipChar '-' >>. eParser |>> EMinus
        skipChar '~' >>. eParser |>> ENeg
        skipChar '$' >>. choice [
            stringReturn "pc" EPc .>> whitespace
            eParser |>> EPeek]
        skipChar '&' >>.  eParser |>> EStack
        skipChar '(' >>. whitespace >>. choice [
            // Notice that we allow zero arguments to these operators.
            // Use many1 to require at least one.
            strWs "+" >>. many eParser |>> ESum
            strWs "*" >>. many eParser |>> EProd
            strWs "&" >>. many eParser |>> EConj
            strWs "|" >>. many eParser |>> EDisj
        ] .>> whitespace .>> skipChar ')'
    ]
    eParser
