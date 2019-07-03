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
    let expr, exprRef = createParserForwardedToRef<Expression, unit>()
    let expr2 = expr .>>. expr
    // Why do we need 'do' here?
    do exprRef := choice [
        identifier |>> ELabel
        positiveNumeral |>> ENum
        skipChar '-' >>. expr |>> EMinus
        skipChar '~' >>. expr |>> ENeg
        skipChar '$' >>. choice [
            stringReturn "pc" EPc .>> whitespace
            expr |>> EPeek]
        skipChar '&' >>.  expr |>> EStack
        between (strWs "(") (strWs ")") <| choice [
            // Notice that we allow zero arguments to these operators.
            // Use many1 to require at least one.
            strWs "+" >>. many expr |>> ESum
            strWs "*" >>. many expr |>> EProd

            strWs "&" >>. many expr |>> EConj
            strWs "|" >>. many expr |>> EDisj
            strWs "^" >>. many expr |>> EXor

            strWs "=" >>. expr2 |>> EEq

            (skipString "load" >>. choice [
                strWs "1" >>. expr |>> ELoad1
                strWs "2" >>. expr |>> ELoad2
                strWs "4" >>. expr |>> ELoad4
                strWs "8" >>. expr |>> ELoad8
            ])
            (skipString "sign" >>. choice [
                strWs "1" >>. expr |>> ESign1
                strWs "2" >>. expr |>> ESign2
                strWs "4" >>. expr |>> ESign4
            ])
            (skipChar '<' >>. choice [
                strWs "<" >>. expr2 |>> EShift
                strWs "=" >>. expr2 |>> ELtE
                whitespace >>. expr2 |>> ELt
            ])
            (skipChar '>' >>. choice [
                strWs "=" >>. expr2 |>> EGtE
                whitespace >>. expr2 |>> EGt
            ])
        ]
    ]
    expr

let statement: Parser<Statement, unit> =
    let isLabel = charReturn ':' -1 .>> whitespace
    let isDef = charReturn '=' -2 .>> whitespace
    let countArgs = many (skipChar '!' .>> whitespace) |>> List.length

    let stmt (id, numArgs) =
        let args = parray numArgs expression
        let limArgs n = if numArgs <= n then args
                        else fun _ -> Reply (Error, unexpected "Too many arguments.")
        let args1 = limArgs 1 |>> Array.tryHead
        let args2 = limArgs 2 |>> fun a ->
            if Array.isEmpty a
            then None
            else Some (a.[0], Array.tryItem 1 a)

        if numArgs = -1 then preturn <| SLabel id
        else if numArgs = -2 then expression |>> fun e -> SDef (id, e)
        else match id with
             | "exit" -> limArgs 0 >>. preturn SExit
             | "push" -> args |>> (Array.toList >> SPush)
             | "set_sp" -> args1 |>> SSetSp
             | "jump" -> args1 |>> SJump
             | "jump_zero" -> args2 |>> SJumpZero
             | "jump_not_zero" -> args2 |>> SJumpNotZero

             | "load1" -> args1 |>> SLoad1
             | "load2" -> args1 |>> SLoad2
             | "load4" -> args1 |>> SLoad4
             | "load8" -> args1 |>> SLoad8
             | "sign1" -> args1 |>> SSign1
             | "sign2" -> args1 |>> SSign2
             | "sign4" -> args1 |>> SSign4
             | "store1" -> args2 |>> SStore1
             | "store2" -> args2 |>> SStore2
             | "store4" -> args2 |>> SStore4
             | "store8" -> args2 |>> SStore8

             | "add" -> args2 |>> SAdd
             | "mult" -> args2 |>> SMult
             | "minus" -> args1 |>> SMinus
             | "and" -> args2 |>> SAnd
             | "or" -> args2 |>> SOr
             | "xor" -> args2 |>> SXor
             | "neg" -> args1 |>> SNeg
             | "shift" -> args2 |>> SShift
             | "shiftS" -> args2 |>> SShift

             | "div_u" -> args2 |>> SDivU
             | "div_s" -> args2 |>> SDivS
             | "rem_u" -> args2 |>> SRemU
             | "rem_s" -> args2 |>> SRemS

             | "lt" -> args2 |>> SLt
             | "lte" -> args2 |>> SLtE
             | "eq" -> args2 |>> SEq
             | "gte" -> args2 |>> SGtE
             | "gt" -> args2 |>> SGt

             | "alloc" -> args1 |>> SAlloc
             | "dealloc" -> args1 |>> SDealloc

             // Better error message than simply 'fail'.
             | _ -> fun _ -> Reply (Error, expected "valid instruction")
    identifier .>>. (isLabel <|> isDef <|> countArgs) >>= stmt

let program = whitespace >>. many statement
