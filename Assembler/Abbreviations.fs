module Assembler.Abbreviations

open Assembler.Ast

// Expand abbrevations, detecting circular dependencies.
let unfold (m: Map<int, Expression>) (l: Map<int, string>) (prog: Statement seq): Statement seq =
    let mutable cache = Map<int, Expression> []

    let lengthen (path: int list) (i: int) : int list =
        match List.tryFindIndex ((=) i) path with
        | None -> i :: path
        | Some j ->
            let circle =
                i :: (List.take (j + 1) path)
                |> Seq.map (fun z -> l.[z])
                |> Seq.rev
            let chain = System.String.Join(" -> ", circle)
            raise (ParseException(sprintf "Circular abbreviations: %s" chain))

    let rec expand (path: int list) (e: Expression): Expression =
        let exp = expand path
        match e with
        | ENum _ -> e
        | ELabel i ->
            match cache.TryFind i with
            | Some y -> y
            | None -> match m.TryFind i with
                      | None -> cache <- cache.Add (i, e); e
                      | Some x ->
                          let y = expand (lengthen path i) x
                          cache <- cache.Add (i, y)
                          y
        | EOffset (n, x) -> EOffset (n, exp x)
        | EStack x -> EStack (exp x)

        | ESum lst -> ESum (List.map exp lst)
        | EProd lst -> EProd (List.map exp lst)
        | ENeg x -> ENeg (exp x)
        | EPow2 x -> EPow2 (exp x)

        | EConj lst -> EConj (List.map exp lst)
        | EDisj lst -> EDisj (List.map exp lst)
        | EXor lst -> EXor (List.map exp lst)
        | ENot x -> ENot (exp x)

        | EDivU (x, y) -> EDivU (exp x, exp y)
        | EDivS (x, y) -> EDivS (exp x, exp y)
        | EDivSU (x, y) -> EDivSU (exp x, exp y)
        | ERemU (x, y) -> ERemU (exp x, exp y)
        | ERemS (x, y) -> ERemS (exp x, exp y)

        | ELtU (x, y) -> ELtU (exp x, exp y)
        | ELtS (x, y) -> ELtS (exp x, exp y)
        | ELtEU (x, y) -> ELtEU (exp x, exp y)
        | ELtES (x, y) -> ELtES (exp x, exp y)
        | EEq (x, y) -> EEq (exp x, exp y)
        | EGtEU (x, y) -> EGtEU (exp x, exp y)
        | EGtES (x, y) -> EGtES (exp x, exp y)
        | EGtU (x, y) -> EGtU (exp x, exp y)
        | EGtS (x, y) -> EGtS (exp x, exp y)

        | ELoad1 x -> ELoad1 (exp x)
        | ELoad2 x -> ELoad2 (exp x)
        | ELoad4 x -> ELoad4 (exp x)
        | ELoad8 x -> ELoad8 (exp x)
        | ESigx1 x -> ESigx1 (exp x)
        | ESigx2 x -> ESigx2 (exp x)
        | ESigx4 x -> ESigx4 (exp x)

    seq {
        for stmt in prog ->
            match stmt with
            | SExport (line, i, e) -> SExport (line, i, expand [] e)
            | SData1 (line, e) -> SData1 (line, expand [] e)
            | SData2 (line, e) -> SData2 (line, expand [] e)
            | SData4 (line, e) -> SData4 (line, expand [] e)
            | SData8 (line, e) -> SData8 (line, expand [] e)
            | SSpacer (line, e) -> SSpacer (line, expand [] e)
            | SPush e -> SPush (expand [] e)
            | _ -> stmt
    }
