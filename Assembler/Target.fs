module Assembler.Target

open Machine.Instructions
open Assembler.Ast
open Machine.Utils


let opLen (ops: int8 list) = List.length ops

// Ensure monotinicity by adding NOPs if necessary.
let nopsFor i = List.replicate i NOP

let powers = [|for i in 0 .. 63 -> 1UL <<< i|]

let pushNum (x: int64): int8 list =
    let n = uint64 x
    let nn = x ^^^ -1L |> uint64

    let b8 = 1UL <<< 8
    let b16 = 1UL <<< 16
    let b32 = 1UL <<< 32

    if n &&& (n - 1UL) = 0UL && n >= b16
    then
        let k = System.Array.BinarySearch (powers, n)
        [PUSH1; int8 k; POW2]                          // 3
    elif nn &&& (nn - 1UL) = 0UL && nn >= b16
    then
        let k = System.Array.BinarySearch (powers, nn)
        [PUSH1; int8 k; POW2; NOT]                     // 4
    else
        let bytes (n: int) (y: uint64) : int8 list =
            [|0..n-1|]
            |> Seq.map (fun i -> y >>> i*8 |> uint8 |> int8)
            |> Seq.toList
        if n = 0UL then [PUSH0]                         // 1
        elif nn = 0UL then [PUSH0; NOT]                 // 2 (pushTrue)
        elif n < b8 then [PUSH1; int8 n]                // 2
        elif nn < b8 then [PUSH1; int8 nn; NOT]         // 3
        elif n < b16 then [PUSH2] @ bytes 2 n           // 3
        elif nn < b16 then [PUSH2] @ bytes 2 nn @ [NOT] // 4
        elif n < b32 then [PUSH4] @ bytes 4 n           // 5
        elif nn < b32 then [PUSH4] @ bytes 4 nn @ [NOT] // 6
        else [PUSH8] @ bytes 8 n                        // 9

let pushInt = int64 >> pushNum

// If x = pushFix f, then x = pushInt (f (length x)) @ [NOP; ...].
let pushFix (f: int -> int) =
    let mutable currLen = 1
    let mutable result = None
    while result.IsNone do
        let currVal = f currLen
        let currRes = pushInt currVal
        let nextLen = opLen currRes
        if nextLen <= currLen
        then result <- Some <| currRes @ nopsFor (currLen - nextLen)
        else currLen <- nextLen
    result.Value

// Using this with negative n is probably not a good idea.
let pop n =
    let pop1 = [JUMP_ZERO; 0y]
    match n with
    | 0 -> []
    | 1 -> pop1
    | 2 -> pop1 @ pop1
    | n -> [GET_SP] @ pushNum (int64 n * 8L) @ [ADD; SET_SP]

let pushTrue = [PUSH0; NOT] // Push -1
let changeSign = pushTrue @ [MULT]
let isZero = [PUSH1; 1y; LT]

let addr n = [GET_SP; PUSH1; int8 (n * 8); ADD]
let get n = addr n @ [LOAD8]
let set n = addr n @ [STORE8]
let toSign =
    [
        PUSH1; 63y; POW2; LT // -1 if positive, 0 if negative
        PUSH1; 2y; MULT         // -2  or  0
        NOT                         //  1  or -1
    ]
let abs = get 0 @ toSign @ [MULT] // Keeps 2^63 unchanged
let divRemBasis unsignedOp =
    List.concat
        [
            // x :: y :: rest
            get 1 @ abs
            get 1 @ abs
            unsignedOp
            // d :: x :: y :: rest, where d = abs y <unsignedOp> abs x.
            get 2 @ toSign
            get 2 @ toSign
            [MULT]
            // s :: d :: ..., where s = 1 if x and y have the same sign, otherwise -1.
            [MULT]
            // d' :: x :: y :: rest, where d' = d or d' = -d.
            set 2
            // x :: 'd :: rest
            pop 1
        ]
let divS = divRemBasis [DIV]
let remS = divRemBasis [REM]
let divSU =
    List.concat
        [
            // x :: y :: rest
            get 1 @ abs
            get 1
            [DIV]
            get 2 @ toSign
            // sign(y) :: |y| / x :: x :: y :: rest
            [MULT]
            set 2
            pop 1
        ]


let offsetSign = pushNum (1L <<< 63) @ [ADD]
let offsetSign2 =
    let bit63 = 1L <<< 63
    offsetSign @ get 1 @ offsetSign @ set 2

let eq = [XOR] @ isZero

let ltU = [LT]
let gtU = get 1 @ ltU @ set 1
let lteU = gtU @ isZero
let gteU = ltU @ isZero

let ltS = offsetSign2 @ ltU
let lteS = offsetSign2 @ lteU
let gtS = offsetSign2 @ gtU
let gteS = offsetSign2 @ gteU


let byteDist x = -128 <= x && x <= 127

// NB. The delta is w.r.t. before the code.
let deltaJump delta =
    if delta = 0 then []
    elif byteDist <| delta - 3 then [PUSH0; JUMP_ZERO; int8 (delta - 3)]
    elif delta < 0 then [GET_PC] @ pushInt (delta - 1) @ [ADD; JUMP]
    else 
        let f pushLength = delta - pushLength - 1
        pushFix f @ [GET_PC; ADD; JUMP]

let deltaJumpZero delta =
    if byteDist <| delta - 2 then [JUMP_ZERO; int8 (delta - 2)]
    else
        let jump = deltaJump (delta - 5)
        [JUMP_ZERO; 3y; PUSH0; JUMP_ZERO; int8 (opLen jump)] @ jump

let deltaJumpNotZero delta = isZero @ deltaJumpZero (delta - opLen isZero)

let genericConditional interjection =
    [
        GET_SP; PUSH1; 8y; ADD; LOAD8 // a::x::r -> x::a::x::r
    ] @ interjection @ [
        JUMP_ZERO; 6y
        // If not zero:
        GET_SP; PUSH1; 8y; ADD; STORE8 // a::x::r -> a::r
        JUMP
    ] @ pop 2 // If zero

let genericJumpNotZero = genericConditional []
let genericJumpZero = genericConditional isZero

// Pair consisting of (i) a piece of code with the net effect of pushing a
// single number onto the stack and (ii) a number which should (at some point)
// be added to this number. I could not think of a good name for this type.
type Spes = int8 list * int64

let collapseSpes ((code, acc): Spes) : int8 list =
    match code, acc with
    | _, 0L -> code
    | [PUSH0], _ -> pushNum acc
    | _, _ -> code @ pushNum acc @ [ADD]

let zeroSpes: Spes = [PUSH0], 0L
let oneSpes: Spes = [PUSH0], 1L
let trueSpes: Spes = [PUSH0], -1L

let addSpes ((c1, a1): Spes) ((c2, a2): Spes) =
    let c = match c1, c2 with
            | _, [PUSH0] -> c1
            | [PUSH0], _ -> c2
            | _, _ -> c1 @ c2 @ [ADD]
    (c, a1 + a2)

let negSpes ((code, acc): Spes) : Spes =
    match code with
    | [PUSH0] -> [PUSH0], -acc
    | _ -> code @ changeSign, -acc

let multSpes (s1: Spes) (s2: Spes) : Spes =
    match s1, s2 with
    | ([PUSH0], m), ([PUSH0], n) -> ([PUSH0], m * n)
    | _, ([PUSH0], 0L) -> zeroSpes
    | ([PUSH0], 0L), _ -> zeroSpes
    | (c, m), ([PUSH0], n) -> c @ pushNum n @ [MULT], m * n
    | ([PUSH0], m), (c, n) -> c @ pushNum m @ [MULT], m * n
    | _, _ -> collapseSpes s1 @ collapseSpes s2 @ [MULT], 0L

let pow2Spes (spes: Spes) : Spes =
    match spes with
    | [PUSH0], n -> [PUSH0], if n < 0L || n > 63L then 0L else 1L <<< int n
    | _ -> collapseSpes spes @ [POW2], 0L

let andSpes (s1: Spes) (s2: Spes) : Spes =
    match s1, s2 with
    | ([PUSH0], m), ([PUSH0], n) -> [PUSH0], m &&& n
    | _, ([PUSH0], 0L) -> ([PUSH0], 0L)
    | ([PUSH0], 0L), _ -> ([PUSH0], 0L)
    | _ -> collapseSpes s1 @ collapseSpes s2 @ [AND], 0L

let orSpes (s1: Spes) (s2: Spes) : Spes =
    match s1, s2 with
    | ([PUSH0], m), ([PUSH0], n) -> [PUSH0], m ||| n
    | _, ([PUSH0], -1L)
    | ([PUSH0], -1L), _ -> ([PUSH0], -1L)
    | _ -> collapseSpes s1 @ collapseSpes s2 @ [OR], 0L

let xorSpes (s1: Spes) (s2: Spes) : Spes =
    match s1, s2 with
    | ([PUSH0], m), ([PUSH0], n) -> [PUSH0], m ^^^ n
    | _ -> collapseSpes s1 @ collapseSpes s2 @ [XOR], 0L

let notSpes ((code, acc): Spes) : Spes =
    match code with
    | [PUSH0] -> [PUSH0], acc ^^^ -1L
    | _ -> code @ [NOT], (acc ^^^ -1L) + 1L

let sigx1Spes (spes: Spes) : Spes =
    match spes with
    | [PUSH0], n -> [PUSH0], uint64 n |> signExtend1 |> int64
    | _ -> collapseSpes spes @ [SIGX1], 0L

let sigx2Spes (spes: Spes) : Spes =
    match spes with
    | [PUSH0], n -> [PUSH0], uint64 n |> signExtend2 |> int64
    | _ -> collapseSpes spes @ [SIGX2], 0L

let sigx4Spes (spes: Spes) : Spes =
    match spes with
    | [PUSH0], n -> [PUSH0], uint64 n |> signExtend4 |> int64
    | _ -> collapseSpes spes @ [SIGX4], 0L


let divUSpes (s1: Spes) (s2: Spes) : Spes =
    match s1, s2 with
    | _, ([PUSH0], 0L) -> zeroSpes // x / 0 = 0 !
    | ([PUSH0], m), ([PUSH0], n) -> [PUSH0], (uint64 m / uint64 n |> int64)
    | _, ([PUSH0], 1L) -> s1
    | ([PUSH0], 0L), _ -> zeroSpes
    | _, _ -> collapseSpes s1 @ collapseSpes s2 @ [DIV], 0L

let divSSpes (s1: Spes) (s2: Spes) : Spes =
    match s1, s2 with
    | _, ([PUSH0], 0L) -> zeroSpes // x / 0 = 0 !
    | ([PUSH0], m), ([PUSH0], n) -> [PUSH0], m / n
    | _, ([PUSH0], 1L) -> s1
    | ([PUSH0], 0L), _ -> zeroSpes
    | (c, m), ([PUSH0], -1L) -> c @ changeSign, -m
    | _, ([PUSH0], n) when n > 0L -> collapseSpes s1 @ pushNum n @ divSU, 0L
    | _, _ -> collapseSpes s1 @ collapseSpes s2 @ divS, 0L

let divSUSpes (s1: Spes) (s2: Spes) : Spes =
    match s1, s2 with
    | _, ([PUSH0], 0L) -> zeroSpes // x / 0 = 0 !
    | ([PUSH0], m), ([PUSH0], n) ->
        let sign = if m < 0L then -1L else 1L
        [PUSH0], ((m * sign |> uint64) / (uint64 n)) |> int64 |> (*) sign
    | _, ([PUSH0], 1L) -> s1
    | ([PUSH0], 0L), _ -> zeroSpes
    | _, _ -> collapseSpes s1 @ collapseSpes s2 @ divSU, 0L

let remUSpes (s1: Spes) (s2: Spes) : Spes =
    match s1, s2 with
    | _, ([PUSH0], 0L) -> zeroSpes // x % 0 = 0 !
    | ([PUSH0], m), ([PUSH0], n) -> [PUSH0], (uint64 m % uint64 n |> int64)
    | _, ([PUSH0], 1L) -> zeroSpes
    | ([PUSH0], 0L), _ -> zeroSpes
    | _, _ -> collapseSpes s1 @ collapseSpes s2 @ [REM], 0L

let remSSpes (s1: Spes) (s2: Spes) : Spes =
    match s1, s2 with
    | _, ([PUSH0], 0L) -> zeroSpes // x % 0 = 0 !
    | ([PUSH0], m), ([PUSH0], n) -> [PUSH0], m % n
    | _, ([PUSH0], 1L) -> zeroSpes
    | ([PUSH0], 0L), _ -> zeroSpes
    | _, ([PUSH0], -1L) -> zeroSpes
    | _, _ -> collapseSpes s1 @ collapseSpes s2 @ remS, 0L

let eqSpes (s1: Spes) (s2: Spes) : Spes =
    match s1, s2 with
    | ([PUSH0], m), ([PUSH0], n) -> [PUSH0], if m = n then -1L else 0L
    | _, _ -> collapseSpes s1 @ collapseSpes s2 @ eq, 0L

let ltUSpes (s1: Spes) (s2: Spes) : Spes =
    match s1, s2 with
    | ([PUSH0], m), ([PUSH0], n) -> [PUSH0], if uint64 m < uint64 n then -1L else 0L
    | _, _ -> collapseSpes s1 @ collapseSpes s2 @ ltU, 0L

let ltSSpes (s1: Spes) (s2: Spes) : Spes =
    match s1, s2 with
    | ([PUSH0], m), ([PUSH0], n) -> [PUSH0], if m < n then -1L else 0L
    | _, _ -> collapseSpes s1 @ collapseSpes s2 @ ltS, 0L

let lteUSpes (s1: Spes) (s2: Spes) : Spes =
    match s1, s2 with
    | ([PUSH0], m), ([PUSH0], n) -> [PUSH0], if uint64 m <= uint64 n then -1L else 0L
    | _, _ -> collapseSpes s1 @ collapseSpes s2 @ lteU, 0L

let lteSSpes (s1: Spes) (s2: Spes) : Spes =
    match s1, s2 with
    | ([PUSH0], m), ([PUSH0], n) -> [PUSH0], if m <= n then -1L else 0L
    | _, _ -> collapseSpes s1 @ collapseSpes s2 @ lteS, 0L

let gtUSpes (s1: Spes) (s2: Spes) : Spes =
    match s1, s2 with
    | ([PUSH0], m), ([PUSH0], n) -> [PUSH0], if uint64 m > uint64 n then -1L else 0L
    | _, _ -> collapseSpes s1 @ collapseSpes s2 @ gtU, 0L

let gtSSpes (s1: Spes) (s2: Spes) : Spes =
    match s1, s2 with
    | ([PUSH0], m), ([PUSH0], n) -> [PUSH0], if m > n then -1L else 0L
    | _, _ -> collapseSpes s1 @ collapseSpes s2 @ gtS, 0L

let gteUSpes (s1: Spes) (s2: Spes) : Spes =
    match s1, s2 with
    | ([PUSH0], m), ([PUSH0], n) -> [PUSH0], if uint64 m >= uint64 n then -1L else 0L
    | _, _ -> collapseSpes s1 @ collapseSpes s2 @ gteU, 0L

let gteSSpes (s1: Spes) (s2: Spes) : Spes =
    match s1, s2 with
    | ([PUSH0], m), ([PUSH0], n) -> [PUSH0], if m >= n then -1L else 0L
    | _, _ -> collapseSpes s1 @ collapseSpes s2 @ gteS, 0L


let exprPushCore (lookup: int -> int) =
    let rec epc (position: int) (depth: int) (expression: Expression) =

        let rec1 e = epc position depth e

        let rec1coll e = rec1 e |> collapseSpes

        let relRec s e =
            let c = collapseSpes s
            epc (position + List.length c) (depth + 1) e

        let rec2 f e1 e2 = let s = rec1 e1 in f s <| relRec s e2

        let optM f u x y = match x, y with
                           | ([PUSH0], m), _ when m = u -> y
                           | _, ([PUSH0], n) when n = u -> x
                           | _, _ -> f x y

        let foldM (f: Spes -> Spes -> Spes) (u: int64) (lst: Expression list) =
            let fu = optM f u
            let fe s e = match s with
                         | [PUSH0], _ -> fu (rec1 e) s
                         | _ -> fu s (relRec s e)
            List.fold fe ([PUSH0], u) lst

        match expression with
        | EOffset (n, e) -> epc position (depth + n) e
        | ENum n -> [PUSH0], n
        | ELabel _| EStack _ -> ESum [expression] |> rec1
        | ESum lst ->
            let tag e =
                match e with
                | ELabel _ -> 1 | ENeg (ELabel _) -> -1
                | EStack _ -> 2 | ENeg (EStack _) -> -2
                | _ -> 0
            let cnt = new Map<int,int>(List.countBy tag lst)
            let nPc = mapGet cnt 1 0 - mapGet cnt -1 0
            let nSp = mapGet cnt 2 0 - mapGet cnt -2 0

            let mutable spes = zeroSpes
            let multN n s = optM multSpes 1L s ([PUSH0], int64 n)
            spes <- addSpes spes <| multN nPc ([GET_PC], 0L)
            spes <- addSpes spes <| multN nSp ([GET_SP], depth * 8 |> int64)

            for ex in lst do
                let p, d = match spes with
                           | [PUSH0], _ -> position, depth
                           | code, _ -> position + opLen code, depth + 1
                let inner = epc p d
                let s = match ex with
                        | ELabel i -> [PUSH0], int64 (lookup i - p)
                        | ENeg (ELabel i) -> [PUSH0], int64 (p - lookup i)
                        | EStack e -> inner e |> multN 8
                        | ENeg (EStack e) -> inner e |> multN -8
                        | e -> inner e
                spes <- addSpes spes s
            // Finally return the result
            spes

        | ENeg e -> e |> rec1 |> negSpes
        | EPow2 e -> e |> rec1 |> pow2Spes
        | ENot e -> e |> rec1 |> notSpes
        | ESigx1 e -> e |> rec1 |> sigx1Spes
        | ESigx2 e -> e |> rec1 |> sigx2Spes
        | ESigx4 e -> e |> rec1 |> sigx4Spes
        | ELoad1 e -> rec1coll e @ [LOAD1], 0L
        | ELoad2 e -> rec1coll e @ [LOAD2], 0L
        | ELoad4 e -> rec1coll e @ [LOAD4], 0L
        | ELoad8 e -> rec1coll e @ [LOAD8], 0L

        | EProd lst -> foldM multSpes 1L lst
        | EConj lst -> foldM andSpes -1L lst
        | EDisj lst -> foldM orSpes 0L lst
        | EXor lst -> foldM xorSpes 0L lst

        | EDivU (e1, e2) -> rec2 divUSpes e1 e2
        | EDivS (e1, e2) -> rec2 divSSpes e1 e2
        | EDivSU (e1, e2) -> rec2 divSUSpes e1 e2
        | ERemU (e1, e2) -> rec2 remUSpes e1 e2
        | ERemS (e1, e2) -> rec2 remSSpes e1 e2

        | EEq (e1, e2) -> rec2 eqSpes e1 e2
        | ELtU (e1, e2) -> rec2 ltUSpes e1 e2
        | ELtS (e1, e2) -> rec2 ltSSpes e1 e2
        | ELtEU (e1, e2) -> rec2 lteUSpes e1 e2
        | ELtES (e1, e2) -> rec2 lteSSpes e1 e2
        | EGtU (e1, e2) -> rec2 gtUSpes e1 e2
        | EGtS (e1, e2) -> rec2 gtSSpes e1 e2
        | EGtEU (e1, e2) -> rec2 gteUSpes e1 e2
        | EGtES (e1, e2) -> rec2 gteSSpes e1 e2

    epc // Return the recursive function

let expressionPush e lookup = exprPushCore lookup 0 0 e |> collapseSpes

let expressionAdd e lookup =
    match exprPushCore lookup 0 0 e with
    | [PUSH0], 0L -> []
    | spes -> collapseSpes spes @ [ADD]

let expressionMult e lookup =
    match exprPushCore lookup 0 0 e with
    | [PUSH0], 1L -> []
    | [PUSH0], 0L -> pop 1 @ [PUSH0] // Pop value, push 0.
    | spes -> collapseSpes spes @ [MULT]

let expressionAnd e lookup =
    match exprPushCore lookup 0 0 e with
    | [PUSH0], -1L -> []
    | [PUSH0], 0L -> pop 1 @ [PUSH0] // Pop value, push 0.
    | spes -> collapseSpes spes @ [AND]

let expressionOr e lookup =
    match exprPushCore lookup 0 0 e with
    | [PUSH0], 0L -> []
    | [PUSH0], -1L -> pop 1 @ pushTrue // Pop value, push -1.
    | spes -> collapseSpes spes @ [OR]

let expressionXor e lookup =
    match exprPushCore lookup 0 0 e with
    | [PUSH0], 0L -> []
    | [PUSH0], -1L -> [NOT]
    | spes -> collapseSpes spes @ [XOR]


let expressionDivU e lookup =
    match exprPushCore lookup 0 0 e with
    | [PUSH0], 0L -> pop 1 @ [PUSH0] // NB: x / 0 = 0
    | [PUSH0], 1L -> []
    | spes -> collapseSpes spes @ [DIV]

let expressionRemU e lookup =
    match exprPushCore lookup 0 0 e with
    | [PUSH0], 0L -> pop 1 @ [PUSH0] // NB: x % 0 = 0
    | [PUSH0], 1L -> pop 1 @ [PUSH0] // Pop value, push 0.
    | spes -> collapseSpes spes @ [REM]

let expressionDivS e lookup =
    match exprPushCore lookup 0 0 e with
    | [PUSH0], 0L -> pop 1 @ [PUSH0] // NB: x / 0 = 0
    | [PUSH0], 1L -> []
    | [PUSH0], -1L -> changeSign
    | [PUSH0], n when n > 0L -> pushNum n @ divSU
    | spes -> collapseSpes spes @ divS

let expressionRemS e lookup =
    match exprPushCore lookup 0 0 e with
    | [PUSH0], 0L -> pop 1 @ [PUSH0] // NB: x % 0 = 0
    | [PUSH0], 1L -> pop 1 @ [PUSH0] // Pop value, push 0.
    | [PUSH0], -1L -> pop 1 @ [PUSH0] // Pop value, push 0.
    | spes -> collapseSpes spes @ remS

let expressionDivSU e lookup =
    match exprPushCore lookup 0 0 e with
    | [PUSH0], 0L -> pop 1 @ [PUSH0] // NB: x / 0 = 0
    | [PUSH0], 1L -> []
    | spes -> collapseSpes spes @ divSU

type FlexCode = (int -> int) -> int8 list

type Intermediate =
    | Label of int
    | Fragment of FlexCode

let intermediates (prog: Statement list) : seq<Intermediate> =
    let mutable rest = prog

    let fragment r f = rest <- r; [Fragment f]
    let frag r code = fragment r <| fun _ -> code
    let withDelta i r f = rest <- r; [Fragment <| fun lookup -> f (lookup i)]

    seq {
        while not rest.IsEmpty do
        yield!
            match rest with
            | SData lst :: r -> frag r lst
            | SExit :: r -> frag r [EXIT]
            | SNot :: r -> frag r [NOT]
            | SNeg :: r -> frag r changeSign
            | SPow2 :: r -> frag r [POW2]

            | SSetSp :: r -> frag r [SET_SP]
            | SAlloc :: r -> frag r [ALLOCATE]
            | SDealloc :: r -> frag r [DEALLOCATE]
            | SLoad1 :: r -> frag r [LOAD1]
            | SLoad2 :: r -> frag r [LOAD2]
            | SLoad4 :: r -> frag r [LOAD4]
            | SLoad8 :: r -> frag r [LOAD8]
            | SSigx1 :: r -> frag r [SIGX1]
            | SSigx2 :: r -> frag r [SIGX2]
            | SSigx4 :: r -> frag r [SIGX4]
            | SStore1 :: r -> frag r [STORE1]
            | SStore2 :: r -> frag r [STORE2]
            | SStore4 :: r -> frag r [STORE4]
            | SStore8 :: r -> frag r [STORE8]

            // Avoid optimizing away tight infinite loops.
            | SLabel i :: SPush (ELabel j) :: SJump :: r when i = j ->
                [Label i] @ frag r [PUSH0; JUMP_ZERO; -3y]

            | SLabel i :: r -> rest <- r; [Label i]

            | SPush (ELabel i) :: SJump :: r -> withDelta i r deltaJump
            | SPush (ELabel i) :: SJumpZero :: r -> withDelta i r deltaJumpZero
            | SPush (ELabel i) :: SJumpNotZero :: r -> withDelta i r deltaJumpNotZero
            | SPush e :: SAdd :: r -> fragment r (expressionAdd e)
            | SPush e :: SMult :: r -> fragment r (expressionMult e)
            | SPush e :: SAnd :: r -> fragment r (expressionAnd e)
            | SPush e :: SOr :: r -> fragment r (expressionOr e)
            | SPush e :: SXor :: r -> fragment r (expressionXor e)
            | SPush e :: SDivU :: r -> fragment r (expressionDivU e)
            | SPush e :: SRemU :: r -> fragment r (expressionRemU e)
            | SPush e :: SDivS :: r -> fragment r (expressionDivS e)
            | SPush e :: SRemS :: r -> fragment r (expressionRemS e)
            | SPush e :: SDivSU :: r -> fragment r (expressionDivSU e)

            | SJump :: r -> frag r [JUMP]
            | SJumpZero :: r -> frag r genericJumpZero
            | SJumpNotZero :: r -> frag r genericJumpNotZero
            | SAdd :: r -> frag r [ADD]
            | SMult :: r -> frag r [MULT]
            | SAnd :: r -> frag r [AND]
            | SOr :: r -> frag r [OR]
            | SXor :: r -> frag r [XOR]
            | SDivU :: r -> frag r [DIV]
            | SRemU :: r -> frag r [REM]
            | SDivS :: r -> frag r divS
            | SRemS :: r -> frag r remS
            | SDivSU :: r -> frag r divSU

            | SLtU :: r -> frag r ltU
            | SLtS :: r -> frag r ltS
            | SLtEU :: r -> frag r lteU
            | SLtES :: r -> frag r lteS
            | SEq :: r -> frag r eq
            | SGtEU :: r -> frag r gteU
            | SGtES :: r -> frag r gteS
            | SGtU :: r -> frag r gtU
            | SGtS :: r -> frag r gtS

            // Minor optimization.
            | SPush (ELabel i) :: SLabel j :: r when i = j ->
                frag (SLabel j :: r) [GET_PC]

            | SPush e :: r -> fragment r (expressionPush e)

            | [] -> failwith "Impossible case"
    }
