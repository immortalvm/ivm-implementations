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

let positiveNumeral: Parser<int64, unit> = puint64 |>> int64

// We allow both signed and unsigned values in full 64 bit range (with wraparound).
let numeral: Parser<int64, unit> =
    // This definition is more natural, but mentions '-' in the overflow error message:
    // let f s v = match s with Some _ -> - int64 v | None -> int64 v
    // pipe2 (opt (pchar '-' )) puint64 f .>> whitespace
    let n64 = skipChar '-' >>. positiveNumeral |>> (~-)
    (positiveNumeral <|> n64) .>> whitespace

let strWs s = skipString s >>. whitespace

let expression: Parser<Expression, unit> =
    // We simply purely constant expressions on the fly.
    let splitNum f g e = match e with ENum n -> f n |> ENum | _ -> g e

    let num e = match e with ENum n -> Some n | _ -> None
    let splitNums f g lst =
        let o = List.map num lst
        if List.forall Option.isSome o
        then List.map Option.get o |> f |> ENum
        else g lst

    let eParser, eParserRef = createParserForwardedToRef<Expression, unit>()
    // Why do we need 'do' here?
    do eParserRef := choice [
        skipChar '-' >>. eParser |>> splitNum (~-) EMinus
        skipChar '~' >>. eParser |>> splitNum (~~~) ENeg
        numeral |>> ENum
        identifier |>> ELabel
        skipChar '$' >>. choice [
            stringReturn "pc" EPc .>> whitespace
            numeral |>> EPeek]
        skipChar '&' >>. numeral |>> EStack
        skipChar '(' >>. whitespace >>. choice [
            strWs "+" >>. many1 eParser |>> splitNums List.sum ESum
            strWs "*" >>. many1 eParser |>> splitNums (List.fold (*) 1L) EProd
            strWs "&" >>. many1 eParser |>> splitNums (List.fold (&&&) -1L) EConj
            strWs "|" >>. many1 eParser |>> splitNums (List.fold (|||) 0L) EDisj
        ] .>> whitespace .>> skipChar ')'
    ]


    // do ePaserRef := 
    eParser

    //let expression: Parser<Expression, unit> =
    //let eParser, ePaserRef = createParserForwardedToRef<Expression, unit>()
    //let strWs s = skipString s >>. whitespace
    //// Why do we need 'do' here?
    //do ePaserRef := choice [
    //    numeral |>> ENum
    //    identifier |>> ELabel
    //    skipChar '$' >>. choice [
    //        stringReturn "pc" EPc .>> whitespace
    //        numeral |>> EPeek]
    //    skipChar '&' >>. numeral |>> EStack
    //    skipChar '(' >>. whitespace >>. choice [
    //        strWs "+" >>. many1 eParser |>> ESum
    //        strWs "*" >>. many1 eParser |>> EProd
    //        strWs "-" >>. eParser |>> EMinus

    //        strWs "&" >>. many1 eParser |>> EConj
    //        strWs "|" >>. many1 eParser |>> EDisj
    //        strWs "~" >>. eParser |>> ENeg

    //    ] .>> whitespace .>> skipChar ')'
    //]
    //eParser
