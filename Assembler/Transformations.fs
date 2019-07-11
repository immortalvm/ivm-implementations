module Assembler.Transformations

open Assembler.Ast


let signExtend n x =
    match n with
    | 1 -> x |> uint8 |> int8 |> int64
    | 2 -> x |> uint16 |> int16 |> int64
    | 4 -> x |> uint32 |> int32 |> int64
    | _ -> failwithf "No such byte-width: %d" n

let combineLists f x y =
    match x, y with
    | ENum m::xs, ENum n::ys -> f m n <| xs @ ys
    // Move constants to the front
    | xs, ENum n::ys -> ENum n :: (xs @ ys)
    | xs, ys -> xs @ ys


// More optimizations are certainly possible.
let rec optimize (e: Expression): Expression =
    match e with
    | ENum _ -> e
    | ELabel _ -> e
    | EMinus x -> minus <| optimize x
    | ENeg x -> neg <| optimize x
    | EPow2 x -> pow2 <| optimize x
    | EStack x -> EStack <| optimize x // TODO
    | ELoad1 x -> ELoad1 <| optimize x
    | ELoad2 x -> ELoad2 <| optimize x
    | ELoad4 x -> ELoad4 <| optimize x
    | ELoad8 x -> ELoad8 <| optimize x

    | ESum lst -> sum [for x in lst -> optimize x]
    | EProd lst -> prod [for x in lst -> optimize x]
    | EConj lst -> conj [for x in lst -> optimize x]
    | EDisj lst -> disj [for x in lst -> optimize x]
    | EXor lst -> xor [for x in lst -> optimize x]
    | EDivU (x, y) -> divU (optimize x) (optimize y)
    | EDivS (x, y) -> divS (optimize x) (optimize y)
    | EDivSU (x, y) -> divSU (optimize x) (optimize y)
    | ERemU (x, y) -> remU (optimize x) (optimize y)
    | ERemS (x, y) -> remS (optimize x) (optimize y)
    | ESign1 x -> sign 1 <| optimize x
    | ESign2 x -> sign 2 <| optimize x
    | ESign4 x -> sign 4 <| optimize x

    | ELtU (x, y) -> liftU ELtU (x, y) (<)
    | ELtS (x, y) -> liftS ELtS (x, y) (<)
    | ELtEU (x, y) -> liftU ELtEU (x, y) (<=)
    | ELtES (x, y) -> liftS ELtES (x, y) (<=)
    | EEq (x, y) -> liftS EEq (x, y) (=)
    | EGtEU (x, y) -> liftU EGtEU (x, y) (>=)
    | EGtES (x, y) -> liftS EGtES (x, y) (>=)
    | EGtU (x, y) -> liftU EGtU (x, y) (>)
    | EGtS (x, y) -> liftS EGtS (x, y) (>)

and private liftU ctor (x, y) rel =
    match optimize x, optimize y with
    | ENum m, ENum n -> ENum (if rel (uint64 m) (uint64 n) then -1L else 0L)
    | xx, yy -> ctor (xx, yy)

and private liftS ctor (x, y) rel =
    match optimize x, optimize y with
    | ENum m, ENum n -> ENum (if rel m n then -1L else 0L)
    | xx, yy -> ctor (xx, yy)

and private minus x =
    match x with
    | ENum n -> ENum -n
    | EMinus e -> e
    | ENeg e -> sum [e; ENum 1L]
    | _ -> EMinus x

and private neg x =
    match x with
    | ENum n -> ENum (n ^^^ -1L)
    | ENeg e -> e
    | EMinus e -> sum [e; ENum -1L]
    | _ -> ENeg x

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
        | EMinus y -> [ENum -1L] @ f y
        | _ -> f x
    let fromList xs = match xs with
                      | [] -> ENum 1L
                      | [x] -> x
                      | [ENum -1L; x] -> minus x
                      | ENum -1L :: xx -> EMinus <| EProd xx
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
        | ENeg y -> [ENum -1L] @ f y
        | _ -> f x
    let fromList xs = match xs with
                      | [] -> ENum 0L
                      | [x] -> x
                      | [ENum -1L; x] -> neg x
                      | ENum -1L :: xx -> ENeg <| EXor xx
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
    | _, ENum -1L -> minus x
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
           | 1 -> ESign1 x
           | 2 -> ESign2 x
           | 4 -> ESign4 x
           | _ -> failwithf "No such byte-width: %d" n


// One of many passes
let pushReduction (prog: Statement seq): Statement seq =
    let p = prog.GetEnumerator ()
    let mutable pending : Expression list = []
    let flush () = let result = pending
                                |> Seq.rev
                                |> Seq.map (optimize >> SPush)
                                |> Seq.toList
                   pending <- []
                   result
    seq {

        while p.MoveNext() do
            let s = p.Current
            match s, pending with
            | SPush e, p -> pending <- e :: p

            | SLoad1, x::r -> pending <- ELoad1 x :: r
            | SLoad2, x::r -> pending <- ELoad2 x :: r
            | SLoad4, x::r -> pending <- ELoad4 x :: r
            | SLoad8, x::r -> pending <- ELoad8 x :: r
            | SSign1, x::r -> pending <- ESign1 x :: r
            | SSign2, x::r -> pending <- ESign2 x :: r
            | SSign4, x::r -> pending <- ESign4 x :: r

            | SAdd, y::x::r -> pending <- ESum [x;y] :: r
            | SMult, y::x::r -> pending <- EProd [x;y] :: r
            | SMinus, x::r -> pending <- EMinus x :: r
            | SAnd, y::x::r -> pending <- EConj [x;y] :: r
            | SOr, y::x::r -> pending <- EDisj [x;y] :: r
            | SXor, y::x::r -> pending <- EXor [x;y] :: r
            | SNeg, x::r -> pending <- ENeg x :: r
            | SPow2, x::r -> pending <- EPow2 x :: r
            | SDivU, y::x::r -> pending <- EDivU (x, y) :: r
            | SDivS, y::x::r -> pending <- EDivS (x, y) :: r
            | SDivSU, y::x::r -> pending <- EDivSU (x, y) :: r
            | SRemU, y::x::r -> pending <- ERemU (x, y) :: r
            | SRemS, y::x::r -> pending <- ERemS (x, y) :: r

            | SLtU , y::x::r -> pending <- ELtU (x, y) :: r
            | SLtS , y::x::r -> pending <- ELtS (x, y) :: r
            | SLtEU , y::x::r -> pending <- ELtEU (x, y) :: r
            | SLtES , y::x::r -> pending <- ELtES (x, y) :: r
            | SEq , y::x::r -> pending <- EEq (x, y) :: r
            | SGtEU , y::x::r -> pending <- EGtEU (x, y) :: r
            | SGtES , y::x::r -> pending <- EGtES (x, y) :: r
            | SGtU , y::x::r -> pending <- EGtU (x, y) :: r
            | SGtS, y::x::r -> pending <- EGtS (x, y) :: r

            | _ -> yield! flush (); yield s
        // It would be strange to end the program with a push, though.
        yield! flush ()
    }
