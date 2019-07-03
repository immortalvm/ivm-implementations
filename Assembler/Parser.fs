module Assembler.Parser
open FParsec
open Assembler.Ast

let comment: Parser<unit, unit> = skipChar '#' >>. skipRestOfLine true
let whitespace = skipSepBy spaces comment

// Based on http://www.quanttec.com/fparsec/tutorial.html#parsing-string-data.
let ident: Parser<string, unit> =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'
    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"

let identifier : Parser<string, unit> =
    ident .>> whitespace // Skip trailing whitespace

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

            strWs "=" >>. (eParser .>>. eParser) |>> EEq

            (skipString "load" >>. choice [
                    strWs "1" >>. eParser |>> ELoad1
                    strWs "2" >>. eParser |>> ELoad2
                    strWs "4" >>. eParser |>> ELoad4
                    strWs "8" >>. eParser |>> ELoad8])
            (skipString "sign" >>. choice [
                    strWs "1" >>. eParser |>> ESign1
                    strWs "2" >>. eParser |>> ESign2
                    strWs "4" >>. eParser |>> ESign4])
            (skipChar '<' >>. choice [
                strWs "<" >>. (eParser .>>. eParser) |>> EShift
                strWs "=" >>. (eParser .>>. eParser) |>> ELtE
                whitespace >>. (eParser .>>. eParser) |>> ELt])
            (skipChar '>' >>. choice [
                    strWs "=" >>. (eParser .>>. eParser) |>> EGtE
                    whitespace >>. (eParser .>>. eParser) |>> EGt
                ]
            )
        ] .>> whitespace .>> skipChar ')'
    ]
    eParser

//let statement: Parser<Statement, unit> =
    //let isLabel = charReturn ':' -1
    //let countArgs = many (skipChar '!') |>> List.length
    //let stmt (id, args) =
    //    if args = -1
    //    then SLabel id
    //    else match id with
    //         | "exit" -> SExit
    //         | _ -> raise 
    //ident .>>. (isLabel <|> countArgs) |>> stmt
