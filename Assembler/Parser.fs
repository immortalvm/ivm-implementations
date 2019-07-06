﻿module Assembler.Parser
open FParsec
open Assembler.Ast
open Assembler.Transformations


type State = {
        Defs: Map<string, Expression> // Definitions
        Missing: Set<string>          // Labels referenced but not yet defined
        Known: Set<string>            // Labels defined
        Offset: int                   // Stack offset
    }
    with
        static member Default = {
            Defs = new Map<string, Expression>([])
            Missing = set []
            Known = set []
            Offset = 0
        }

        static member TryExpand id (str: CharStream<State>) =
            let s = str.UserState
            match s.Defs.TryFind id with
            | Some e -> e
            | None ->
                if not <| s.Known.Contains id
                then str.UserState <- { s with Missing = s.Missing.Add id }
                ELabel id
            |> Reply

        static member ObsLabel id (str: CharStream<State>) =
            let s = str.UserState
            str.UserState <- {
                s with
                    Defs = s.Defs.Remove id
                    Missing = s.Missing.Remove id
                    Known = s.Known.Add id
            }
            Reply <| [SLabel id]

        static member AddDef id e =
            updateUserState<State> <|
            fun s -> { s with Defs = s.Defs.Add (id, e) }

        static member IncOffset =
            updateUserState<State> <| fun s -> { s with Offset = s.Offset + 1 }

        static member ResetOffset =
            updateUserState<State> <| fun s -> { s with Offset = 0 }


let comment: Parser<unit, State> = skipChar '#' >>. skipRestOfLine true
let whitespace = skipSepBy spaces comment

// Based on http://www.quanttec.com/fparsec/tutorial.html#parsing-string-data.
let identifier: Parser<string, State> =
    let first c = isLetter c || c = '_'
    let rest c = isLetter c || isDigit c || c = '_'
    many1Satisfy2L first rest "identifier" .>> whitespace

let positiveNumeral: Parser<int64, State> = puint64 .>> whitespace |>> int64

let strWs s = skipString s >>. whitespace

// Not used  yet. Postpone until the labels can also be evaluated.
let splitNum f g e = match e with ENum n -> f n |> ENum | _ -> g e
let splitNums f g lst =
    let num e = match e with ENum n -> Some n | _ -> None
    let o = List.map num lst
    if List.forall Option.isSome o
    then List.map Option.get o |> f |> ENum
    else g lst

let offset e (str: CharStream<State>) =
    let off = str.UserState.Offset |> int64
    if off = 0L then e
    else match e with
         | ENum n -> ENum <| off + n
         | ESum lst -> ESum <| [ENum off] @ lst
         // Everything above are optimizations.
         | _ -> ESum [ENum off; e]
    |> Reply

let expression: Parser<Expression, State> =
    let expr, exprRef = createParserForwardedToRef<Expression, State>()
    let expr2 = expr .>>. expr
    // Why do we need 'do' here?
    do exprRef := choice [
        identifier >>= State.TryExpand
        positiveNumeral |>> ENum
        skipChar '-' >>. expr |>> EMinus
        skipChar '~' >>. expr |>> ENeg
        skipChar '$' >>. expr >>= offset |>> (EStack >> ELoad8)
        skipChar '&' >>. expr >>= offset |>> EStack
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

let data: Parser<uint8 list, State> =
    let neg = skipChar '-' >>. puint8 |>> (int8 >> (~-) >> uint8)
    let either = (puint8 <|> neg) .>> whitespace
    between (strWs "[") (strWs "]") <| many either

let statement: Parser<Statement list, State> =
    let isLabel = charReturn ':' -1 .>> whitespace
    let isDef = charReturn '=' -2 .>> whitespace
    let countArgs = many (skipChar '!' .>> whitespace) |>> List.length

    let pushArgs numArgs = parray numArgs (expression .>> State.IncOffset)
                           .>> State.ResetOffset
                           |>> (Array.toList >> List.map SPush)
    // Computed once for efficiency
    let pushArgs0 = preturn []
    let pushArgs1 = pushArgs 1
    let pushArgs2 = pushArgs 2

    let stmt (id, numArgs) =
        if numArgs = -1
        then State.ObsLabel id

        else if numArgs = -2
        then expression >>=
             fun e -> preturn [] .>> State.AddDef id e
        else
            let pArgs = match numArgs with
                        | 0 -> pushArgs0
                        | 1 -> pushArgs1
                        | 2 -> pushArgs2
                        | _ -> pushArgs numArgs
            let argsOp op = pArgs |>> fun a -> a @ [op]
            let nArgs maxArgs op =
                if numArgs <= maxArgs then argsOp op
                else fun _ -> Reply (Error, unexpected "Too many arguments.")

            match id with
             | "data" -> data |>> (SData >> List.singleton)
             | "exit" -> nArgs 0 SExit
             | "push" -> pArgs
             | "set_sp" -> nArgs 1 SSetSp
             | "jump" -> nArgs 1 <| SJump None
             | "jump_zero" -> nArgs 2 <| SJumpZero None
             | "jump_not_zero" -> nArgs 2 <| SJumpNotZero None

             | "load1" -> nArgs 1 SLoad1
             | "load2" -> nArgs 1 SLoad2
             | "load4" -> nArgs 1 SLoad4
             | "load8" -> nArgs 1 SLoad8
             | "sign1" -> nArgs 1 SSign1
             | "sign2" -> nArgs 1 SSign2
             | "sign4" -> nArgs 1 SSign4
             | "store1" -> nArgs 2 SStore1
             | "store2" -> nArgs 2 SStore2
             | "store4" -> nArgs 2 SStore4
             | "store8" -> nArgs 2 SStore8

             | "add" -> nArgs 2 SAdd
             | "sub" -> nArgs 2 SSub
             | "mult" -> nArgs 2 SMult
             | "minus" -> nArgs 1 SMinus
             | "and" -> nArgs 2 SAnd
             | "or" -> nArgs 2 SOr
             | "xor" -> nArgs 2 SXor
             | "neg" -> nArgs 1 SNeg
             | "shift" -> nArgs 2 SShift
             | "shift_s" -> nArgs 2 SShiftS

             | "div_u" -> nArgs 2 SDivU
             | "div_s" -> nArgs 2 SDivS
             | "rem_u" -> nArgs 2 SRemU
             | "rem_s" -> nArgs 2 SRemS

             | "lt_u" -> nArgs 2 SLtU
             | "lt_s" -> nArgs 2 SLtS
             | "lte_u" -> nArgs 2 SLtEU
             | "lte_s" -> nArgs 2 SLtES
             | "eq" -> nArgs 2 SEq
             | "gte_u" -> nArgs 2 SGtEU
             | "gte_s" -> nArgs 2 SGtES
             | "gt_u" -> nArgs 2 SGtU
             | "gt_s" -> nArgs 2 SGtS

             | "alloc" -> nArgs 1 SAlloc
             | "dealloc" -> nArgs 1 SDealloc

             // Better error message than simply 'fail'.
             | _ -> fun _ -> Reply (Error, unexpectedString id)

    // Eliminating >>= might lead to better performance.
    identifier .>>. (isLabel <|> isDef <|> countArgs) >>= stmt


let program = whitespace >>. many statement |>> List.concat .>> eof


exception ParseException of string

let parseProgram (stream: System.IO.Stream): Statement seq =
    match runParserOnStream program State.Default "" stream System.Text.Encoding.UTF8 with
    | Success(result, s, _) ->
        let c = s.Missing.Count
        if c = 0 then result |> mergePushLabelJumps
        else sprintf "Label%s not found: %s"
                 (if c > 1 then "s" else "")
                 (System.String.Join(", ", s.Missing))
             |> ParseException |> raise
    | Failure(errorMsg, _, _) -> errorMsg |> ParseException |> raise
