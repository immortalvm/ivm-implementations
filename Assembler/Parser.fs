module Assembler.Parser
open FParsec
open Machine.Utils
open Assembler.Ast
open Assembler.Abbreviations
open Assembler.Transformations
open Assembler.Namespace

// Observe that we must use "eta expansion" (fun str -> ... str) for F# to
// define generic parsers. We need this so that we can reuse the same parsers
// for analyzing dependencies as well as full parsing.
let comment: Parser<unit, _> = fun str -> (skipChar '#' >>. skipRestOfLine true) str
let whitespace = fun str -> skipSepBy spaces comment str

// Based on http://www.quanttec.com/fparsec/tutorial.html#parsing-string-data.
let identifierNoWhitespace: Parser<string, _> = fun str ->
    let first c = isLetter c || c = '_' || c = '.'
    let rest c = isLetter c || isDigit c || c = '_' || c = '.'
    many1Satisfy2L first rest "identifier" str

let importBase : Parser<string list, _> = fun str ->
    (skipString "IMPORT"
     >>? whitespace
     >>. (sepBy1 identifierNoWhitespace <| skipChar NODE_SEPARATOR)) str

let importsOnly : Parser<string list, unit> =
    whitespace >>. many (importBase |>> nodeString .>> whitespace)

let parseDependencies (stream: System.IO.Stream): Set<string> =
    match runParserOnStream importsOnly () "" stream System.Text.Encoding.UTF8 with
    | Success(result, _, _) -> set result
    | Failure(errorMsg, _, _) -> errorMsg |> ParseException |> raise


type State = {
        ExtSymbols: string -> (bool * int64) option // Positions after end of this file.
        Defs: Map<int, Expression>    // Abbreviations
        Labels: Map<string, int>      // Label numbers
        Count: int                    // Labels defined/referenced so far
        Exported: Set<int>            // Label numbers of exported names
        // Label 0 will refer to the end/length of this (sub)binary.
        // Count other labels from 1 so that we can use negative numbers for
        // referenced but not yet defined labels.

        Nodes: string list            // Nodes being processed as one
        NodeIndex: int                // Index of the current node
        ExportAwaited: Set<int>       // For imports within Nodes
    }
    with
        member s.Qualified id: string =
            nodeJoin [s.Nodes.[s.NodeIndex]; id]

        static member Init externalSymbols nodes nodeIndex: State = {
            ExtSymbols = externalSymbols
            Defs = Map.empty
            Labels = Map.empty
            Count = 0
            Exported = set []
            Nodes = nodes
            NodeIndex = nodeIndex
            ExportAwaited = set []
        }

        static member private GetQnIx qn (str: CharStream<State>) =
            let s = str.UserState
            match s.Labels.TryFind qn with
            | Some i -> abs i
            | None -> let i = s.Count + 1
                      str.UserState <- {
                        s with
                            Labels = s.Labels.Add (qn, -i)
                            Count = i
                      }
                      i

        static member private GetLabelIx id (str: CharStream<State>) =
            State.GetQnIx (str.UserState.Qualified id) str

        static member UseLabel (id: string) (str: CharStream<State>) =
            State.GetLabelIx id str |> ELabel |> Reply

        static member Export id (str: CharStream<State>) =
            let i = State.GetLabelIx id str // modifies str.UserState
            let s = str.UserState
            str.UserState <- { s with Exported = s.Exported.Add i }
            Reply [SExport(str.Line, i, ELabel i)]

        static member Call (str: CharStream<State>) =
            let s = str.UserState
            let i = s.Count + 1
            str.UserState <- { s with Count = i }
            Reply [SCall i]

        static member AnonymousLabel (str: CharStream<State>) =
            let s = str.UserState
            let i = s.Count + 1
            str.UserState <- { s with Count = i }
            Reply i

        static member DefLabel id (str: CharStream<State>) =
            let s = str.UserState
            let qn = s.Qualified id
            match s.Labels.TryFind qn with
            | Some i -> if i > 0 then Reply (Error, unexpected (sprintf "'%s' is already defined." id))
                        else
                            str.UserState <- {
                                s with Labels = s.Labels.Add (qn, abs i)
                            }
                            Reply -i
            | None -> let i = s.Count + 1
                      str.UserState <- {
                        s with
                            Labels = s.Labels.Add (qn, i)
                            Count = i
                      }
                      Reply i

        static member private RegisterImport ids i (str: CharStream<State>) =
            let node, id = nodeId ids
            let mutable s = str.UserState
            if List.contains node s.Nodes
            then
                let j = State.GetQnIx (nodeJoin ids) str
                s <- str.UserState
                str.UserState <- {
                    s with
                        Defs = s.Defs.Add (i, ELabel j)
                        ExportAwaited = s.ExportAwaited.Add j
                }
                Reply (())
            else
                let qualifiedName = nodeJoin [node; id]
                match s.ExtSymbols qualifiedName with
                | None -> Reply (FatalError, messageError <| "Not exported: " + qualifiedName)
                | Some (rel, x) ->
                    let e = if rel
                            then ESum [ENum <| int64 x; ELabel 0]
                            else ENum x
                    str.UserState <- { s with Defs = s.Defs.Add (i, e) }
                    Reply (())

        static member ObsImport (ids: string list) =
            if ids.Length < 2
            then fun _ -> Reply (Error, expected "Imports must be qualified.")
            else State.DefLabel (Seq.last ids) >>= State.RegisterImport ids

        static member ImplicitImports (imports: string list) =
            match imports with
            | [] -> preturn ()
            | imp::rest -> State.ObsImport (splitNodeString imp |> Array.toList) >>. State.ImplicitImports rest

        static member Abbrev (i, e) (str: CharStream<State>) =
            let s = str.UserState
            str.UserState <- { s with Defs = s.Defs.Add (i, e) }
            Reply ([] : Statement list)

        // Used after the parsing is done

        member s.ReverseLabels (): Map<int, string> =
            reverseMap s.Labels

        // Qualified names
        member s.Undefined = seq {
            for pair in s.Labels do
                if pair.Value < 0 then
                    yield pair.Key
        }

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
        identifier >>= State.UseLabel
        positiveNumeral |>> ENum
        skipChar '-' >>. expr |>> ENeg
        skipChar '~' >>. expr |>> ENot
        skipChar '$' >>. expr |>> (EStack >> ELoad8)
        skipChar '&' >>. expr |>> EStack
        between (strWs "(") (strWs ")") innerExpr
    ]
    expr

let lineNumAndExpr: Parser<int64 * Expression, State> =
    (fun s -> Reply s.Line) .>>. expression

let data: Parser<(int64 * Expression) list, State> =
    between (strWs "[") (strWs "]") <| many lineNumAndExpr
    .>>. (strWs "*" >>. positiveNumeral <|> preturn 1L)
    |>> fun (list, n) -> Seq.replicate (int n) list |> Seq.concat |> Seq.toList

let statement: Parser<Statement list, State> =
    let labelKey = -1
    let defKey = -2
    let listKey = -3

    let isLabel = charReturn ':' labelKey .>> whitespace
    let isDef = charReturn '=' defKey .>> whitespace
    let hasArgList = charReturn '*' listKey .>> whitespace
    let countArgs = many (skipChar '!' .>> whitespace) |>> List.length

    let push i x = SPush <| EOffset (i, x)
    let pushArgs numArgs = parray numArgs expression
                           |>> (Array.toList >> List.mapi push)
    // Computed once, for efficiency
    let pushArgs0 = preturn []
    let pushArgs1 = pushArgs 1
    let pushArgs2 = pushArgs 2

    let argList = between (strWs "[") (strWs "]") <| many expression
    let pushList = argList |>> List.mapi push
    let maxPushList maxArgs =
        let check lst =
            if List.length lst <= maxArgs then preturn lst
            else fun _ -> Reply (Error, unexpected "Too many arguments.")
        argList >>= check |>> List.mapi push

    let oneSpacer =
        lineNumAndExpr .>>. State.AnonymousLabel
        |>> fun ((ln, e), i) -> [SLabel i; SSpacer (ln, e); SData8 (0L, ENum 0L)]

    let stmt (id, numArgs) =
        if numArgs = labelKey
        then State.DefLabel id |>> (SLabel >> List.singleton)

        else if numArgs = defKey
        then State.DefLabel id .>>. expression >>= State.Abbrev
        else
            let pArgs () = match numArgs with
                           | 0 -> pushArgs0
                           | 1 -> pushArgs1
                           | 2 -> pushArgs2
                           | _ -> pushArgs numArgs
            let nArgs maxArgs ops =
                if numArgs = listKey then maxPushList maxArgs
                elif numArgs <= maxArgs then pArgs ()
                else fun _ -> Reply (Error, unexpected "Too many arguments.")
                |>> fun a -> a @ ops

            let numArgsZero =
                if numArgs <> 0
                then fun _ ->
                    let c = if numArgs = listKey then "'*'" else "'!'"
                    Reply (Error, unexpected <| c + " does not make sense here.")
                else
                    preturn ()

            match id with
            | "EXPORT" -> numArgsZero >>. identifier >>= State.Export .>> whitespace
            | "data1" -> numArgsZero >>. data |>> (List.map SData1)
            | "data2" -> numArgsZero >>. data |>> (List.map SData2)
            | "data4" -> numArgsZero >>. data |>> (List.map SData4)
            | "data8" -> numArgsZero >>. data |>> (List.map SData8)
            | "space" -> numArgsZero >>. oneSpacer

            | "exit" -> nArgs 0 [SExit]
            | "push" -> if numArgs = listKey then pushList else pArgs ()
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

            | "new_frame" -> nArgs 3 [SNewFrame]
            | "set_pixel" -> nArgs 5 [SSetPixel]
            | "add_sample" -> nArgs 3 [SAddSample]
            | "put_char" -> nArgs 1 [SPutChar]

            | "read_frame" -> nArgs 0 [SReadFrame]
            | "read_pixel" -> nArgs 2 [SReadPixel]

            // Better error message than simply 'fail'.
            | _ -> fun _ -> Reply (Error, unexpectedString id)

    // Eliminating >>= might lead to better performance.
    identifier .>>. (isLabel <|> isDef <|> hasArgList <|> countArgs) >>= stmt

let program : Parser<Statement list, State> =
    whitespace
    >>. skipMany (importBase >>= State.ObsImport >>. whitespace)
    >>. many statement
    |>> List.concat
    .>> eof

type AnalysisResult = {
    ImportsFrom: string list // nodes
    Exported: string list    // identifiers
    Undefined: string list   // identifiers (implicit imports)
}

open System.IO

let analyze (streamFun: unit -> Stream): AnalysisResult =
    let mutable imported : Set<string> = set []
    let extSym qn =
        imported <- imported.Add qn
        Some (true, 0L)
    let state = State.Init extSym ["#"] 0
    use stream = streamFun ()
    match runParserOnStream program state "" stream System.Text.Encoding.UTF8 with
    | Failure(errorMsg, _, _) -> errorMsg |> ParseException |> raise
    | Success(_, s, _) ->
        let revLabels = state.ReverseLabels ()
        let lab i = unqualify revLabels.[i]
        {
            ImportsFrom = imported |> Seq.map qnNode |> Seq.toList
            Exported = s.Exported |> Seq.map lab |> Seq.toList
            Undefined = state.Undefined |> Seq.map unqualify |> Seq.toList
        }

let parseProgram
    (comp: (string * (unit -> string list * Stream)) list)
    (extSymbols: string -> (bool * int64) option) : Statement list * Set<int> * Map<int, string> =
    let mutable state = State.Init extSymbols (List.map fst comp) 0

    let parse pairFun =
        let pair = pairFun ()
        use stream = snd pair
        let parser = State.ImplicitImports (fst pair) >>. program
        match runParserOnStream parser state "" stream System.Text.Encoding.UTF8 with
        | Success(result, s, _) ->
            state <- { s with NodeIndex = s.NodeIndex + 1 }
            result
        | Failure(errorMsg, _, _) -> errorMsg |> ParseException |> raise
    let result = comp |> Seq.map (snd >> parse) |> Seq.concat |> Seq.toList

    let missing = Seq.toList state.Undefined
    if not missing.IsEmpty then
        sprintf "Label%s not found: %s"
                 (if missing.Length > 1 then "s" else "")
                 (System.String.Join(", ", missing))
             |> ParseException |> raise

    let revLabels = state.ReverseLabels ()
    let notExported = [
        for i in state.ExportAwaited do
            if not (state.Exported.Contains i)
            then yield revLabels.[i]
    ]
    if not notExported.IsEmpty then
        sprintf "Label%s not exported: %s"
                (if notExported.Length > 1 then "s" else "")
                (System.String.Join(", ", notExported))
             |> ParseException |> raise

    let prog = result
               |> unfold state.Defs revLabels
               |> moveExportsFirst
               |> pushReduction
               |> Seq.toList
    prog, state.Exported, revLabels