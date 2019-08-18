module Assembler.Parser
open FParsec
open Assembler.Ast
open Assembler.Transformations


exception ParseException of string

// Observe that we must use "eta expansion" (fun str -> ... str) for F# to
// define generic parsers. We need this so that we can reuse the same parsers
// for analyzing dependencies as well as full parsing.
let comment: Parser<unit, _> = fun str -> (skipChar '#' >>. skipRestOfLine true) str
let whitespace = fun str -> skipSepBy spaces comment str

// Based on http://www.quanttec.com/fparsec/tutorial.html#parsing-string-data.
let identifierNoWhitespace: Parser<string, _> = fun str ->
    let first c = isLetter c || c = '_'
    let rest c = isLetter c || isDigit c || c = '_'
    many1Satisfy2L first rest "identifier" str

let importBase : Parser<string list, _> = fun str ->
    (skipString "IMPORT"
     >>? whitespace
     >>. (sepBy1 identifierNoWhitespace <| skipChar '.')) str

let importsOnly : Parser<string list, unit> =
    let node ids = System.String.Join('.', Seq.take (List.length ids - 1) ids)
    whitespace >>. many (importBase |>> node .>> whitespace)

let parseDependencies (stream: System.IO.Stream): Set<string> =
    match runParserOnStream importsOnly () "" stream System.Text.Encoding.UTF8 with
    | Success(result, _, _) -> set result
    | Failure(errorMsg, _, _) -> errorMsg |> ParseException |> raise


type State = {
        ExtSymbols: string -> int     // Positions after end of this file.
        Defs: Map<string, Expression> // Definitions
        Labels: Map<string, int>      // Label numbers
        Count: int                    // Labels defined/referenced so far
        Exported: Map<string, int>    // Label numbers of exported names
        // Label 0 will refer to the end/length of this (sub)binary.
        // Count other labels from 1 so that we can use negative numbers for
        // referenced but not yet defined labels.
    }
    with
        static member Init externalSymbols = {
            ExtSymbols = externalSymbols
            Defs = Map.empty
            Labels = Map.empty
            Count = 0
            Exported = Map.empty
        }

        static member ObsImport (ids: string list) (str: CharStream<State>) =
            let n = ids.Length
            if n < 2
            then Reply (Error, expected "Too few!")
            else
                let s = str.UserState
                let node = System.String.Join ('.', Seq.take (n - 1) ids)
                let id = List.last ids
                let qualifiedName = node + "." + id
                let offset = s.ExtSymbols qualifiedName
                if offset < 0
                then Reply (FatalError, messageError <| "Not found: " + qualifiedName)
                else
                    let e = ESum [ENum <| int64 offset; ELabel 0]
                    str.UserState <- { s with Defs = s.Defs.Add (id, e) }
                    Reply (())

        static member TryExpand id (str: CharStream<State>) =
            let s = str.UserState
            match s.Defs.TryFind id with
            | Some e -> e
            | None ->
                match s.Labels.TryFind id with
                | Some i -> abs i
                | None -> let i = s.Count + 1
                          str.UserState <- {
                            s with
                                Labels = s.Labels.Add (id, -i)
                                Count = i
                          }
                          i
                |> ELabel
            |> Reply

        static member Call (str: CharStream<State>) =
            let s = str.UserState
            let i = s.Count + 1
            str.UserState <- { s with Count = i }
            Reply [SCall i]

        static member ObsLabel id (str: CharStream<State>) =
            let s = str.UserState
            match s.Labels.TryFind id with
            | Some i -> if i < 0 then str.UserState <- {
                                        s with Labels = s.Labels.Add (id, abs i)
                                      }
                                      -i
                        else i
            | None -> let i = s.Count + 1
                      str.UserState <- {
                        s with
                            Defs = s.Defs.Remove id
                            Labels = s.Labels.Add (id, i)
                            Count = i
                      }
                      i
            |> SLabel |> List.singleton |> Reply

        static member Export id (str: CharStream<State>) =
            match (State.TryExpand id str).Result with
            | ELabel i ->
                let s = str.UserState
                str.UserState <- { s with Exported = s.Exported.Add (id, i) }
                Reply ([] : Statement list)
            | _ -> Reply (Error, unexpected "Not a label")

        static member AddDef id e =
            updateUserState<State> <|
            fun s -> { s with Defs = s.Defs.Add (id, e) }

let identifier: Parser<string, State> = identifierNoWhitespace .>> whitespace

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

let expression: Parser<Expression, State> =
    let expr, exprRef = createParserForwardedToRef<Expression, State>()
    let expr1 = whitespace >>. expr
    let expr2 = whitespace >>. expr .>>. expr
    let exprList = whitespace >>. many expr
    let first c = isLetter c || "+*&|^=<>/%".Contains c
    let rest c = first c || isDigit c

    let exprHead = many1Satisfy2 first rest .>> whitespace
    let innerExpr stream =
        let reply = exprHead stream
        if reply.Status <> Ok then Reply(reply.Status, reply.Error)
        else
            let p = match reply.Result with
                    | "+" -> exprList |>> ESum
                    | "*" -> exprList |>> EProd
                    | "&" -> exprList |>> EConj
                    | "|" -> exprList |>> EDisj
                    | "^" -> exprList |>> EXor

                    | "<u" -> expr2 |>> ELtU
                    | "<s" -> expr2 |>> ELtS
                    | "<=u" -> expr2 |>> ELtEU
                    | "<=s" -> expr2 |>> ELtES
                    | "=" -> expr2 |>> EEq
                    | ">u" -> expr2 |>> EGtU
                    | ">s" -> expr2 |>> EGtS
                    | ">=u" -> expr2 |>> EGtEU
                    | ">=s" -> expr2 |>> EGtES

                    | "<<" -> expr2 |>> fun (x, y) -> EProd [x; EPow2 y]
                    | ">>u" -> expr2 |>> fun (x, y) -> EDivU (x, EPow2 y)
                    | ">>s" -> expr2 |>> fun (x, y) -> EDivSU (x, EPow2 y)

                    | "/u" -> expr2 |>> EDivU
                    | "/s" -> expr2 |>> EDivS
                    | "/su" -> expr2 |>> EDivSU
                    | "%u" -> expr2 |>> ERemU
                    | "%s" -> expr2 |>> ERemS

                    | "load1" -> expr1 |>> ELoad1
                    | "load2" -> expr1 |>> ELoad2
                    | "load4" -> expr1 |>> ELoad4
                    | "load8" -> expr1 |>> ELoad8

                    | "sigx1" -> expr1 |>> ESigx1
                    | "sigx2" -> expr1 |>> ESigx2
                    | "sigx4" -> expr1 |>> ESigx4
                    | unknown -> fail <| sprintf "Not an expression keyword: %s" unknown
            p stream

    // Why do we need 'do' here?
    do exprRef := choice [
        identifier >>= State.TryExpand
        positiveNumeral |>> ENum
        skipChar '-' >>. expr |>> ENeg
        skipChar '~' >>. expr |>> ENot
        skipChar '$' >>. expr |>> (EStack >> ELoad8)
        skipChar '&' >>. expr |>> EStack
        between (strWs "(") (strWs ")") innerExpr
    ]
    expr

let data: Parser<int8 list, State> =
    let neg = skipChar '-' >>. puint8 |>> (int8 >> (~-))
    let either = ((puint8 |>> int8) <|> neg) .>> whitespace
    between (strWs "[") (strWs "]") <| many either

let statement: Parser<Statement list, State> =
    let isLabel = charReturn ':' -1 .>> whitespace
    let isDef = charReturn '=' -2 .>> whitespace
    let countArgs = many (skipChar '!' .>> whitespace) |>> List.length

    let push i x = SPush <| EOffset (i, x)
    let pushArgs numArgs = parray numArgs expression
                           |>> (Array.toList >> List.mapi push)
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
            let argsOp ops = pArgs |>> fun a -> a @ ops
            let nArgs maxArgs ops =
                if numArgs <= maxArgs then argsOp ops
                else fun _ -> Reply (Error, unexpected "Too many arguments.")

            match id with
            | "EXPORT" ->
                if numArgs <> 0
                then fun _ -> Reply (Error, unexpected "'!' does not make sense here.")
                else identifier >>= State.Export .>> whitespace
            | "data" -> data |>> (SData >> List.singleton)
            | "exit" -> nArgs 0 [SExit]
            | "push" -> pArgs
            | "set_sp" -> nArgs 1 [SSetSp]
            | "call" -> nArgs 1 [] .>>. State.Call |>> fun (x, y) -> x @ y
            | "return" -> nArgs 0 [SJump]
            | "jump" -> nArgs 1 [SJump]
            | "jump_zero" -> nArgs 2 [SJumpZero]
            | "jump_not_zero" -> nArgs 2 [SJumpNotZero]

            | "load1" -> nArgs 1 [SLoad1]
            | "load2" -> nArgs 1 [SLoad2]
            | "load4" -> nArgs 1 [SLoad4]
            | "load8" -> nArgs 1 [SLoad8]
            | "sigx1" -> nArgs 1 [SSigx1]
            | "sigx2" -> nArgs 1 [SSigx2]
            | "sigx4" -> nArgs 1 [SSigx4]
            | "store1" -> nArgs 2 [SStore1]
            | "store2" -> nArgs 2 [SStore2]
            | "store4" -> nArgs 2 [SStore4]
            | "store8" -> nArgs 2 [SStore8]

            | "add" -> nArgs 2 [SAdd]
            | "sub" -> nArgs 2 [SNeg; SAdd]
            | "mult" -> nArgs 2 [SMult]
            | "neg" -> nArgs 1 [SNeg]
            | "and" -> nArgs 2 [SAnd]
            | "or" -> nArgs 2 [SOr]
            | "xor" -> nArgs 2 [SXor]
            | "not" -> nArgs 1 [SNot]
            | "pow2" -> nArgs 1 [SPow2]
            | "shift_l" -> nArgs 2 [SPow2; SMult]
            | "shift_ru" -> nArgs 2 [SPow2; SDivU]
            // Use SDivUS to make sure that (-1L <<< 63) >>> 63 = -1 (not 1).
            | "shift_rs" -> nArgs 2 [SPow2; SDivSU]

            | "div_u" -> nArgs 2 [SDivU]
            | "div_s" -> nArgs 2 [SDivS]
            | "rem_u" -> nArgs 2 [SRemU]
            | "rem_s" -> nArgs 2 [SRemS]

            | "lt_u" -> nArgs 2 [SLtU]
            | "lt_s" -> nArgs 2 [SLtS]
            | "lte_u" -> nArgs 2 [SLtEU]
            | "lte_s" -> nArgs 2 [SLtES]
            | "eq" -> nArgs 2 [SEq]
            | "gte_u" -> nArgs 2 [SGtEU]
            | "gte_s" -> nArgs 2 [SGtES]
            | "gt_u" -> nArgs 2 [SGtU]
            | "gt_s" -> nArgs 2 [SGtS]

            | "allocate" -> nArgs 1 [SAlloc]
            | "deallocate" -> nArgs 1 [SDealloc]

            | "new_frame" -> nArgs 3 [SNewFrame]
            | "set_pixel" -> nArgs 5 [SSetPixel]
            | "add_sample" -> nArgs 3 [SAddSample]

            // Better error message than simply 'fail'.
            | _ -> fun _ -> Reply (Error, unexpectedString id)

    // Eliminating >>= might lead to better performance.
    identifier .>>. (isLabel <|> isDef <|> countArgs) >>= stmt

let program : Parser<Statement list, State> =
    whitespace
    >>. skipMany (importBase >>= State.ObsImport >>. whitespace)
    >>. many statement
    |>> List.concat
    .>> eof

let parseProgram (initState: State) (stream: System.IO.Stream)
    : seq<Statement> * Map<string, int> * Map<string, int> =
    match runParserOnStream program initState "" stream System.Text.Encoding.UTF8 with
    | Success(result, s, _) ->
        let missing = [
            for pair in s.Labels do
            if pair.Value < 0
            then yield pair.Key]
        if missing.IsEmpty
        then result |> pushReduction, s.Exported, s.Labels
        else sprintf "Label%s not found: %s"
                 (if missing.Length > 1 then "s" else "")
                 (System.String.Join(", ", missing))
             |> ParseException |> raise
    | Failure(errorMsg, _, _) -> errorMsg |> ParseException |> raise
