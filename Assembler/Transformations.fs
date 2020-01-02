module Assembler.Transformations

open Assembler.Ast
open Machine.Utils


let moveExportsFirst (prog: Statement seq): Statement seq =
    let mutable exported: Set<int> = set []
    let mutable nonExports: Statement list = []
    seq {
        for stmt in prog do
            match stmt with
            | SExport (line, i, e) ->
                // Ignore duplicate export statements
                if not (exported.Contains i) then
                    exported <- exported.Add i
                yield stmt
            | _ -> nonExports <- stmt :: nonExports
        yield! Seq.rev nonExports
    }

let private signExtend n x =
    match n with
    | 1 -> uint64 x |> signExtend1 |> int64
    | 2 -> uint64 x |> signExtend2 |> int64
    | 4 -> uint64 x |> signExtend4 |> int64
    | _ -> failwithf "No such byte-width: %d" n

let private combineLists f x y =
    match x, y with
    | ENum m::xs, ENum n::ys -> f m n <| xs @ ys
    // Move constants to the front
    | xs, ENum n::ys -> ENum n :: (xs @ ys)
    | xs, ys -> xs @ ys


// More optimizations are certainly possible.
// We eliminate all occurrences of EOffset, but others may get introduced later.
let rec private optimize (offset: int64) (e: Expression): Expression =
    let o = offset // For brevity
    match e with
    | ENum _ -> e
    | ELabel _ -> e
    | ENeg x -> neg <| optimize o x
    | ENot x -> binNot <| optimize o x
    | EPow2 x -> pow2 <| optimize o x
    | EStack x -> EStack <| sum [optimize o x; ENum o]
    | ELoad1 x -> ELoad1 <| optimize o x
    | ELoad2 x -> ELoad2 <| optimize o x
    | ELoad4 x -> ELoad4 <| optimize o x
    | ELoad8 x -> ELoad8 <| optimize o x

    | ESum lst -> sum [for x in lst -> optimize o x]
    | EProd lst -> prod [for x in lst -> optimize o x]
    | EConj lst -> conj [for x in lst -> optimize o x]
    | EDisj lst -> disj [for x in lst -> optimize o x]
    | EXor lst -> xor [for x in lst -> optimize o x]
    | EDivU (x, y) -> divU (optimize o x) (optimize o y)
    | EDivS (x, y) -> divS (optimize o x) (optimize o y)
    | EDivSU (x, y) -> divSU (optimize o x) (optimize o y)
    | ERemU (x, y) -> remU (optimize o x) (optimize o y)
    | ERemS (x, y) -> remS (optimize o x) (optimize o y)
    | ESigx1 x -> sign 1 <| optimize o x
    | ESigx2 x -> sign 2 <| optimize o x
    | ESigx4 x -> sign 4 <| optimize o x

    | ELtU (x, y) -> liftU o ELtU (x, y) (<)
    | ELtS (x, y) -> liftS o ELtS (x, y) (<)
    | ELtEU (x, y) -> liftU o ELtEU (x, y) (<=)
    | ELtES (x, y) -> liftS o ELtES (x, y) (<=)
    | EEq (x, y) -> liftS o EEq (x, y) (=)
    | EGtEU (x, y) -> liftU o EGtEU (x, y) (>=)
    | EGtES (x, y) -> liftS o EGtES (x, y) (>=)
    | EGtU (x, y) -> liftU o EGtU (x, y) (>)
    | EGtS (x, y) -> liftS o EGtS (x, y) (>)

    | EOffset (m, x) -> optimize (o + int64 m) x

and private liftU o ctor (x, y) rel =
    match optimize o x, optimize o y with
    | ENum m, ENum n -> ENum (if rel (uint64 m) (uint64 n) then -1L else 0L)
    | xx, yy -> ctor (xx, yy)

and private liftS o ctor (x, y) rel =
    match optimize o x, optimize o y with
    | ENum m, ENum n -> ENum (if rel m n then -1L else 0L)
    | xx, yy -> ctor (xx, yy)

and private neg x =
    match x with
    | ENum n -> ENum -n
    | ENeg e -> e
    | ENot e -> sum [e; ENum 1L]
    | _ -> ENeg x

and private binNot x =
    match x with
    | ENum n -> ENum (n ^^^ -1L)
    | ENot e -> e
    | ENeg e -> sum [e; ENum -1L]
    | _ -> ENot x

and private pow2 x =
    match x with
    | ENum m -> if m > 63L || m < 0L then 0UL else 1UL <<< int m
                |> int64 |> ENum
    | _ -> EPow2 x

and private sum lst =
    let toList x = match x with ESum xs -> xs | ENum 0L -> [] | _ -> [x]
    let fromList xs = match xs with [] -> ENum 0L | [x] -> x | _ -> ESum xs
    let combine = combineLists <| fun m n r -> match m + n with 0L -> r | s -> ENum s::r
    lst |> List.map toList |> List.fold combine [] |> fromList

and private prod lst =
    let toList x =
        let f y = match y with EProd ys -> ys | ENum 1L -> [] | _ -> [y]
        match x with
        | ENeg y -> [ENum -1L] @ f y
        | _ -> f x
    let fromList xs = match xs with
                      | [] -> ENum 1L
                      | [x] -> x
                      | [ENum -1L; x] -> neg x
                      | ENum -1L :: xx -> ENeg <| EProd xx
                      | _ -> EProd xs
    let combine = combineLists <| fun m n r -> match m * n with
                                               | 0L -> []
                                               | 1L -> r
                                               | p -> ENum p :: r
    lst |> List.map toList |> List.fold combine [] |> fromList

and private conj lst =
    let toList x = match x with EConj xs -> xs | ENum 1L -> [] | _ -> [x]
    let fromList xs = match xs with [] -> ENum -1L | [x] -> x | _ -> EConj xs
    let combine = combineLists <| fun m n r -> match m &&& n with
                                               | 0L -> [ENum 0L]
                                               | -1L -> r
                                               | c -> ENum c :: r
    lst |> List.map toList |> List.fold combine [] |> fromList

and private disj lst =
    let toList x = match x with EDisj xs -> xs | ENum 0L -> [] | _ -> [x]
    let fromList xs = match xs with [] -> ENum 0L | [x] -> x | _ -> EDisj xs
    let combine = combineLists <| fun m n r -> match m ||| n with
                                               | 0L -> r
                                               | -1L -> [ENum -1L]
                                               | c -> ENum c :: r
    lst |> List.map toList |> List.fold combine [] |> fromList

and private xor lst =
    let toList x =
        let f y = match y with EXor ys -> ys | ENum 0L -> [] | _ -> [y]
        match x with
        | ENot y -> [ENum -1L] @ f y
        | _ -> f x
    let fromList xs = match xs with
                      | [] -> ENum 0L
                      | [x] -> x
                      | [ENum -1L; x] -> binNot x
                      | ENum -1L :: xx -> ENot <| EXor xx
                      | _ -> EXor xs
    let combine = combineLists <| fun m n r -> match m ^^^ n with
                                               | 0L -> r
                                               | p -> ENum p :: r
    lst |> List.map toList |> List.fold combine [] |> fromList

and private divU x y =
    match x, y with
    | _, ENum 0L -> ENum 0L // x / 0 = 0 !
    | ENum m, ENum n -> (uint64 m) / (uint64 n) |> int64 |> ENum
    | _, ENum 1L -> x
    | _, _ -> EDivU (x, y)

and private divS x y =
    match x, y with
    | _, ENum 0L -> ENum 0L // x / 0 = 0 !
    | ENum m, ENum n -> m / n |> ENum
    | _, ENum 1L -> x
    | _, ENum -1L -> neg x
    | _, _ -> EDivS (x, y)

and private divSU x y =
    match x, y with
    | _, ENum 0L -> ENum 0L // x / 0 = 0 !
    | ENum m, ENum n ->
        let sign = if m < 0L then -1L else 1L
        (m * sign |> uint64) / (uint64 n) |> int64 |> (*) sign |> ENum
    | _, ENum 1L -> x
    | _, _ -> EDivSU (x, y)

and private remU x y =
    match x, y with
    | _, ENum 0L -> ENum 0L // x % 0 = 0 !
    | ENum m, ENum n -> (uint64 m) % (uint64 n) |> int64 |> ENum
    | _, ENum 1L -> ENum 0L
    | _, ENum -1L -> ENum 0L
    | _, _ -> ERemU (x, y)

and private remS x y =
    match x, y with
    | _, ENum 0L -> ENum 0L // x % 0 = 0 !
    | ENum m, ENum n -> m % n |> ENum
    | _, ENum 1L -> ENum 0L
    | _, _ -> ERemS (x, y)

and private sign n x =
    match x with
    | ENum m -> signExtend n m |> ENum
    | _ -> match n with
           | 1 -> ESigx1 x
           | 2 -> ESigx2 x
           | 4 -> ESigx4 x
           | _ -> failwithf "No such byte-width: %d" n

// Whether it safe to make this expression the n'th argument (counting from 0)
// of a larger expression. In particular, it must refer to any of the n previous
// arguments. We are not trying to be too clever here.
let rec safeN n e =
    n < 1 || match e with
             | ENum _ | ELabel _ -> true
             | EOffset (k, e) -> safeN (n - k) e
             | EStack (ENum m)
             | ELoad8 (EStack (ENum m)) -> int64 n <= m
             | ESum lst
             | EProd lst
             | EConj lst
             | EDisj lst
             | EXor lst -> List.forall (safeN n) lst
             | ENeg x
             | EPow2 x
             | ENot x
             | ESigx1 x
             | ESigx2 x
             | ESigx4 x -> safeN n x
             | EDivU (x, y) | EDivS (x, y) | EDivSU (x, y)
             | ERemU (x, y) | ERemS (x, y)
             | ELtU (x, y) | ELtS (x, y) | ELtEU (x, y) | ELtES (x, y)
             | EEq (x, y)
             | EGtEU (x, y) | EGtES (x, y) | EGtU (x, y) | EGtS (x, y) ->
                 safeN n x && safeN n y
             | _ -> false

// One of many passes?
let pushReduction (prog: Statement seq): Statement seq =
    let p = prog.GetEnumerator ()
    let mutable pending : Expression list = []
    let flush coda = let result = pending
                                  |> Seq.rev
                                  |> Seq.map (optimize 0L >> SPush)
                                  |> Seq.toList
                     pending <- []
                     result @ coda

    let offset1 e = EOffset (-1, e)
    let safe1 = safeN 1
    seq {

        while p.MoveNext() do
            let s = p.Current
            match s, pending with
            | SPush e, p -> pending <- e :: p

            | SLoad1, x::r -> pending <- ELoad1 x :: r
            | SLoad2, x::r -> pending <- ELoad2 x :: r
            | SLoad4, x::r -> pending <- ELoad4 x :: r
            | SLoad8, x::r -> pending <- ELoad8 x :: r
            | SSigx1, x::r -> pending <- ESigx1 x :: r
            | SSigx2, x::r -> pending <- ESigx2 x :: r
            | SSigx4, x::r -> pending <- ESigx4 x :: r

            | SNeg, x::r -> pending <- ENeg x :: r
            | SNot, x::r -> pending <- ENot x :: r
            | SPow2, x::r -> pending <- EPow2 x :: r
            | SAdd, y::x::r when safe1 y -> pending <- ESum [x; offset1 y] :: r
            | SMult, y::x::r when safe1 y -> pending <- EProd [x; offset1 y] :: r
            | SAnd, y::x::r when safe1 y -> pending <- EConj [x; offset1 y] :: r
            | SOr, y::x::r when safe1 y -> pending <- EDisj [x; offset1 y] :: r
            | SXor, y::x::r when safe1 y -> pending <- EXor [x; offset1 y] :: r
            | SDivU, y::x::r when safe1 y -> pending <- EDivU (x, offset1 y) :: r
            | SDivS, y::x::r when safe1 y -> pending <- EDivS (x, offset1 y) :: r
            | SDivSU, y::x::r when safe1 y -> pending <- EDivSU (x, offset1 y) :: r
            | SRemU, y::x::r when safe1 y -> pending <- ERemU (x, offset1 y) :: r
            | SRemS, y::x::r when safe1 y -> pending <- ERemS (x, offset1 y) :: r

            | SLtU, y::x::r when safe1 y -> pending <- ELtU (x, offset1 y) :: r
            | SLtS, y::x::r when safe1 y -> pending <- ELtS (x, offset1 y) :: r
            | SLtEU, y::x::r when safe1 y -> pending <- ELtEU (x, offset1 y) :: r
            | SLtES, y::x::r when safe1 y -> pending <- ELtES (x, offset1 y) :: r
            | SEq, y::x::r when safe1 y -> pending <- EEq (x, offset1 y) :: r
            | SGtEU, y::x::r when safe1 y -> pending <- EGtEU (x, offset1 y) :: r
            | SGtES, y::x::r when safe1 y -> pending <- EGtES (x, offset1 y) :: r
            | SGtU, y::x::r when safe1 y -> pending <- EGtU (x, offset1 y) :: r
            | SGtS, y::x::r when safe1 y -> pending <- EGtS (x, offset1 y) :: r

            // This eliminates all instances of SCall.
            | SCall i, x::r ->
                pending <- EOffset (1, x) :: ELabel i :: r
                yield! flush [SJump; SLabel i]
            | SCall i, _ ->
                pending <- [ENum 2L |> EStack
                            ELabel i
                            ENum 0L |> EStack |> ELoad8]
                yield! flush [SStore8; SJump; SLabel i]

            | SData1 (line, p), _ -> yield! flush [SData1 (line, optimize 0L p)]
            | SData2 (line, p), _ -> yield! flush [SData2 (line, optimize 0L p)]
            | SData4 (line, p), _ -> yield! flush [SData4 (line, optimize 0L p)]
            | SData8 (line, p), _ -> yield! flush [SData8 (line, optimize 0L p)]
            | SSpacer (line, p), _ -> yield! flush [SSpacer (line, optimize 0L p)]

            // Not necessary to flush first.
            | SExport (line, i, x), r -> yield SExport(line, i, optimize 0L x)

            | _ -> yield! flush [s]
        // It would be strange to end the program with a push, though.
        yield! flush []
    }
