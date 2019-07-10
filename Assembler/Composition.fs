module Assembler.Composition


[<Literal>]
let private ATTEMPTS_BEFORE_MONOTINICITY = 3;

// Code given label -> relative position (from statement _start_)
type FlexCode = (int -> int) -> int8 list

type Intermediate =
    | Label of int
    | Fragment of FlexCode

let opLen : int8 list -> int = List.length

// 'nops n' must return a nop sequence of at least n signed bytes.
let compose (nops: int -> int8 list) (prog: Intermediate list) : seq<int8> * int[] =
    let maxLabel = List.max <| 0 :: [
                       for x in prog do
                       match x with
                       | Label i -> yield i
                       | _ -> ()
                   ]
    // positions[0] will refer to the beginning of the file.
    // This will be useful for absolute adressing (of initial program).
    let positions = Array.create (maxLabel + 1) 0

    let pLength = List.length prog
    let starts = Array.create pLength 0
    let codes : (int8 list)[] = Array.create pLength []

    let stable = Array.create pLength false
    let mutable allStable = false

    // (from statement, to label) -> distance
    let mutable replies = new Map<int * int, int>([])

    let mutable attempts = 0

    while not allStable do
        let mutable position = 0

        let updateIfNecessary num inter =
            let lookup i =
                let res = positions.[i] - position
                replies <- replies.Add ((num, i), res)
                res

            match inter with
            | Label i -> positions.[i] <- position
            | Fragment frag ->
                starts.[num] <- position
                if not stable.[num]
                then let c = frag lookup
                     // Avoid infinite loop by enforcing monotonicity.
                     codes.[num] <- if attempts < ATTEMPTS_BEFORE_MONOTINICITY
                                    then c
                                    else let l0 = opLen codes.[num]
                                         let l1 = opLen c
                                         if l1 >= l0 then c
                                         else c @ nops (l0 - l1)
                position <- position + opLen codes.[num]

        List.iteri updateIfNecessary prog

        // Check all replies (not only the recently recalculated).
        allStable <- true
        Array.fill stable 0 pLength true
        for keyValue in replies do
            let (num, label) = keyValue.Key
            if not (positions.[label] - starts.[num] = keyValue.Value)
            then stable.[num] <- false
                 allStable <- false

        attempts <- attempts + 1

    (Seq.concat codes, positions)


// -- TODO: Move rest to separate file -------------------------------------

[<Literal>]
let EXIT = 0y

[<Literal>]
let NOP = 1y

[<Literal>]
let JUMP = 2y

[<Literal>]
let JUMP_ZERO = 3y // Relative to next signed byte (immediate arg)

[<Literal>]
let SET_STACK = 4y

[<Literal>]
let GET_PC = 5y

[<Literal>]
let GET_STACK = 6y

// Is this worth including?
[<Literal>]
let PUSH0 = 7y

// Push the next byte(s) as a zero-padded 64-bit integer.
[<Literal>]
let PUSH1 = 8y

[<Literal>]
let PUSH2 = 9y

[<Literal>]
let PUSH4 = 10y

[<Literal>]
let PUSH8 = 11y


// Sign extension
[<Literal>]
let SIGN1 = 12y // From 8 to 64 bits

[<Literal>]
let SIGN2 = 13y // From 16 to 64 bits

[<Literal>]
let SIGN4 = 14y // From 32 to 64 bits


// 15: Unused

[<Literal>]
let LOAD1 = 16y

[<Literal>]
let LOAD2 = 17y

[<Literal>]
let LOAD4 = 18y

[<Literal>]
let LOAD8 = 19y


[<Literal>]
let STORE1 = 20y

[<Literal>]
let STORE2 = 21y

[<Literal>]
let STORE4 = 22y

[<Literal>]
let STORE8 = 23y


[<Literal>]
let ALLOCATE = 24y

[<Literal>]
let DEALLOCATE = 25y

[<Literal>]
let OUTPUT = 26y

[<Literal>]
let INPUT = 27y


// 28-31: Unused

[<Literal>]
let ADD = 32y

[<Literal>]
let MULTIPLY = 33y

[<Literal>]
let DIVIDE = 34y // Unsigned.

[<Literal>]
let REMAINDER = 35y // Unsigned.

[<Literal>]
let LESS_THAN = 36y // Unsigned. 0: false. FF...F: true.

// 36-39: Unused

[<Literal>]
let AND = 40y

[<Literal>]
let OR = 41y

[<Literal>]
let NOT = 42y

[<Literal>]
let XOR = 43y

[<Literal>]
let POW2 = 44y

// -------------------------------

// Ensure monotinicity by adding NOPs if necessary.
let nopsFor i = List.replicate i NOP

let powers = [|for i in 0 .. 63 -> 1L <<< i|]

let pushNum (x: int64): int8 list =
    let n = uint64 x
    let nn = x ^^^ -1L |> uint64

    let b8 = 1UL <<< 8
    let b16 = 1UL <<< 16
    let b32 = 1UL <<< 32

    if x &&& (x - 1L) = 0L && n >= b16 && nn >= b8
    then
        let k = System.Array.BinarySearch (powers, x)
        [PUSH0; int8 k; POW2]                          // 3
    else
        let bytes (n: int) (y: uint64) : int8 list =
            [|0..n-1|]
            |> Seq.map (fun i -> y >>> i*8 |> uint8 |> int8)
            |> Seq.toList
        if n = 0UL then [PUSH0]                         // 1
        elif nn = 0UL then [PUSH0; NOT]                 // 2
        elif n < b8 then [PUSH1; int8 n]               // 2
        elif nn < b8 then [PUSH1; int8 nn; NOT]        // 3
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
    | n -> [GET_STACK] @ pushNum (int64 n * 8L) @ [ADD; SET_STACK]

let changeSign = [NOT; PUSH1; 1y; ADD]
let isZero = [PUSH1; 1y; LESS_THAN]

module Helpers =
    let addr n = [GET_STACK; PUSH1; int8 (n * 8); ADD]
    let get n = addr n @ [LOAD8]
    let set n = addr n @ [STORE8]
    let isNegative = [PUSH1; 63y; DIVIDE]
    let abs = get 0 @ isNegative @ [JUMP_ZERO; 1y; NOT]
    let divS =
        List.concat
            [
                // x :: y :: rest
                get 1 @ abs
                get 1 @ abs
                [DIVIDE]
                // d :: x :: y :: rest, where d = abs y / abs x.
                get 2
                get 2
                [MULTIPLY]
                isNegative
                // s :: d :: x :: y :: rest, where s = 0 if x and y have the same sign.
                [JUMP_ZERO; int8 (opLen changeSign)]
                changeSign
                // d' :: x :: y :: rest, where d' = d or d' = -d.
                set 2
                // x :: 'd :: rest
                pop 1
            ]
    let remS =
        List.concat
            [
                // x :: y :: rest
                get 1 @ get 1 @ divS
                // y/x :: x :: y :: rest
                [MULTIPLY]
                // (y/x)*x :: y :: rest
                changeSign
                [ADD]
                // y - (y/x)*x :: rest
            ]

    // let pushTrue = [PUSH0; NOT]
    let offsetSign = pushNum (1L <<< 63) @ [ADD]
    let offsetSign2 =
        let bit63 = 1L <<< 63
        offsetSign @ get 1 @ offsetSign @ set 1

    let eq = [XOR] @ isZero

    let ltU = [LESS_THAN]
    let gtU = get 1 @ ltU @ set 1
    let lteU = gtU @ isZero
    let gteU = ltU @ isZero

    let ltS = offsetSign @ ltU
    let lteS = offsetSign @ lteU
    let gtS = offsetSign2 @ gtU
    let gteS = offsetSign2 @ gteU

open Assembler.Ast

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
        GET_STACK; PUSH1; 8y; ADD; LOAD8 // a::x::r -> x::a::x::r
    ] @ interjection @ [
        JUMP_ZERO; 6y
        // If not zero:
        GET_STACK; PUSH1; 8y; ADD; STORE8 // a::x::r -> a::r
        JUMP
    ] @ pop 2 // If zero

let genericJumpNotZero = genericConditional []
let genericJumpZero = genericConditional isZero

// TODO: There should be a built-in function like this.
let valueOr<'a> (def: 'a) (x: 'a option) = match x with Some z -> z | _ -> def
let mapGet<'a, 'b when 'a: comparison> (m: Map<'a,'b>) (x: 'a) (def: 'b) =
    m.TryFind x |> valueOr def

type Spes = int8 list * int64

let collapseSpes ((code, acc): Spes) : int8 list =
    match code, acc with
    | _, 0L -> code
    | [PUSH0], _ -> pushNum acc
    | _, _ -> code @ pushNum acc @ [ADD]

// NB: This only works for commutative operations.
let collapseSpes2 (s1: Spes) (s2: Spes) : int8 list =
    match s1 with
    // In this case, s2 expects to go first, see 'inner' in exprPushCore.
    | [PUSH0], _ -> collapseSpes s2 @ collapseSpes s1
    | _ -> collapseSpes s1 @ collapseSpes s2

let zeroSpes: Spes = [PUSH0], 0L
let oneSpes: Spes = [PUSH0], 1L
let trueSpes: Spes = [PUSH0], -1L

let addSpes ((c1, a1): Spes) ((c2, a2): Spes) =
    let c = match c1, c2 with
            | _, [PUSH0] -> c1
            | [PUSH0], _ -> c2
            | _, _ -> c1 @ c2 @ [ADD]
    (c, a1 + a2)

let minusSpes ((code, acc): Spes) : Spes =
    match code with
    | [PUSH0] -> [PUSH0], -acc
    | _ -> code @ changeSign, -acc

let multSpes (s1: Spes) (s2: Spes) : Spes =
    match s1, s2 with
    | ([PUSH0], m), ([PUSH0], n) -> ([PUSH0], m * n)
    | _, ([PUSH0], 0L) -> zeroSpes
    | ([PUSH0], 0L), _ -> zeroSpes
    | _, ([PUSH0], 1L) -> s1
    | ([PUSH0], 1L), _ -> s2
    | (c, m), ([PUSH0], n) -> c @ pushNum n @ [MULTIPLY], m * n
    | ([PUSH0], m), (c, n) -> c @ pushNum m @ [MULTIPLY], m * n
    | _, _ -> collapseSpes2 s1 s2 @ [MULTIPLY], 0L

let timesNSpes (n: int64) (s: Spes) : Spes = multSpes s ([PUSH0], n)

let pow2Spes (spes: Spes) : Spes =
    match spes with
    | [PUSH0], n -> [PUSH0], if n < 0L || n > 63L then 0L else 1L <<< int n
    | _ -> collapseSpes spes @ [POW2], 0L

let andSpes (s1: Spes) (s2: Spes) : Spes =
    match s1, s2 with
    | ([PUSH0], m), ([PUSH0], n) -> [PUSH0], m &&& n
    | _, ([PUSH0], 0L) -> ([PUSH0], 0L)
    | ([PUSH0], 0L), _ -> ([PUSH0], 0L)
    | _, ([PUSH0], -1L) -> s1
    | ([PUSH0], -1L), _ -> s2
    | _ -> collapseSpes2 s1 s2 @ [AND], 0L

let orSpes (s1: Spes) (s2: Spes) : Spes =
    match s1, s2 with
    | ([PUSH0], m), ([PUSH0], n) -> [PUSH0], m ||| n
    | _ -> collapseSpes2 s1 s2 @ [OR], 0L

let xorSpes (s1: Spes) (s2: Spes) : Spes =
    match s1, s2 with
    | ([PUSH0], m), ([PUSH0], n) -> [PUSH0], m ^^^ n
    | _ -> collapseSpes2 s1 s2 @ [XOR], 0L

let negSpes ((code, acc): Spes) : Spes =
    match code with
    | [PUSH0] -> [PUSH0], acc ^^^ -1L
    | _ -> code @ [NOT], acc ^^^ -1L

let sign1Spes (spes: Spes) : Spes =
    match spes with
    | [PUSH0], n -> [PUSH0], n |> uint8 |> int8 |> int64
    | _ -> collapseSpes spes @ [SIGN1], 0L

let sign2Spes (spes: Spes) : Spes =
    match spes with
    | [PUSH0], n -> [PUSH0], n |> uint16 |> int16 |> int64
    | _ -> collapseSpes spes @ [SIGN2], 0L

let sign4Spes (spes: Spes) : Spes =
    match spes with
    | [PUSH0], n -> [PUSH0], n |> uint32 |> int32 |> int64
    | _ -> collapseSpes spes @ [SIGN4], 0L


// Below, we always use s1 code if we use s2 code!
// This is important since these operations are not commutative.

let divUSpes (s1: Spes) (s2: Spes) : Spes =
    match s1, s2 with
    // Do not eliminate division by zero:
    | _, ([PUSH0], 0L) -> [PUSH0; PUSH0; DIVIDE], 0L
    | ([PUSH0], m), ([PUSH0], n) -> [PUSH0], (uint64 m / uint64 n |> int64)
    | _, ([PUSH0], 1L) -> s1
    | ([PUSH0], 0L), _ -> zeroSpes
    | _, _ -> collapseSpes s1 @ collapseSpes s2 @ [DIVIDE], 0L

let divSSpes (s1: Spes) (s2: Spes) : Spes =
    match s1, s2 with
    // Do not eliminate division by zero:
    | _, ([PUSH0], 0L) -> [PUSH0; PUSH0; DIVIDE], 0L
    | ([PUSH0], m), ([PUSH0], n) -> [PUSH0], m / n
    | _, ([PUSH0], 1L) -> s1
    | ([PUSH0], 0L), _ -> zeroSpes
    | (c, m), ([PUSH0], -1L) -> c @ changeSign, -m
    | _, _ -> collapseSpes s1 @ collapseSpes s2 @ Helpers.divS, 0L

let remUSpes (s1: Spes) (s2: Spes) : Spes =
    match s1, s2 with
    // Do not eliminate division by zero:
    | _, ([PUSH0], 0L) -> [PUSH0; PUSH0; REMAINDER], 0L
    | ([PUSH0], m), ([PUSH0], n) -> [PUSH0], (uint64 m % uint64 n |> int64)
    | _, ([PUSH0], 1L) -> zeroSpes
    | ([PUSH0], 0L), _ -> zeroSpes
    | _, _ -> collapseSpes s1 @ collapseSpes s2 @ [REMAINDER], 0L

let remSSpes (s1: Spes) (s2: Spes) : Spes =
    match s1, s2 with
    // Do not eliminate division by zero:
    | _, ([PUSH0], 0L) -> [PUSH0; PUSH0; REMAINDER], 0L
    | ([PUSH0], m), ([PUSH0], n) -> [PUSH0], m % n
    | _, ([PUSH0], 1L) -> zeroSpes
    | ([PUSH0], 0L), _ -> zeroSpes
    | _, ([PUSH0], -1L) -> zeroSpes
    | _, _ -> collapseSpes s1 @ collapseSpes s2 @ Helpers.remS, 0L

let eqSpes (s1: Spes) (s2: Spes) : Spes =
    match s1, s2 with
    | ([PUSH0], m), ([PUSH0], n) -> [PUSH0], if m = n then -1L else 0L
    | _, _ -> collapseSpes s1 @ collapseSpes s2 @ Helpers.eq, 0L

let ltUSpes (s1: Spes) (s2: Spes) : Spes =
    match s1, s2 with
    | ([PUSH0], m), ([PUSH0], n) -> [PUSH0], if uint64 m < uint64 n then -1L else 0L
    | _, _ -> collapseSpes s1 @ collapseSpes s2 @ Helpers.ltU, 0L

let ltSSpes (s1: Spes) (s2: Spes) : Spes =
    match s1, s2 with
    | ([PUSH0], m), ([PUSH0], n) -> [PUSH0], if m < n then -1L else 0L
    | _, _ -> collapseSpes s1 @ collapseSpes s2 @ Helpers.ltS, 0L

let lteUSpes (s1: Spes) (s2: Spes) : Spes =
    match s1, s2 with
    | ([PUSH0], m), ([PUSH0], n) -> [PUSH0], if uint64 m <= uint64 n then -1L else 0L
    | _, _ -> collapseSpes s1 @ collapseSpes s2 @ Helpers.lteU, 0L

let lteSSpes (s1: Spes) (s2: Spes) : Spes =
    match s1, s2 with
    | ([PUSH0], m), ([PUSH0], n) -> [PUSH0], if m <= n then -1L else 0L
    | _, _ -> collapseSpes s1 @ collapseSpes s2 @ Helpers.lteS, 0L

let gtUSpes (s1: Spes) (s2: Spes) : Spes =
    match s1, s2 with
    | ([PUSH0], m), ([PUSH0], n) -> [PUSH0], if uint64 m > uint64 n then -1L else 0L
    | _, _ -> collapseSpes s1 @ collapseSpes s2 @ Helpers.gtU, 0L

let gtSSpes (s1: Spes) (s2: Spes) : Spes =
    match s1, s2 with
    | ([PUSH0], m), ([PUSH0], n) -> [PUSH0], if m > n then -1L else 0L
    | _, _ -> collapseSpes s1 @ collapseSpes s2 @ Helpers.gtS, 0L

let gteUSpes (s1: Spes) (s2: Spes) : Spes =
    match s1, s2 with
    | ([PUSH0], m), ([PUSH0], n) -> [PUSH0], if uint64 m >= uint64 n then -1L else 0L
    | _, _ -> collapseSpes s1 @ collapseSpes s2 @ Helpers.gteU, 0L

let gteSSpes (s1: Spes) (s2: Spes) : Spes =
    match s1, s2 with
    | ([PUSH0], m), ([PUSH0], n) -> [PUSH0], if m >= n then -1L else 0L
    | _, _ -> collapseSpes s1 @ collapseSpes s2 @ Helpers.gteS, 0L


let rec exprPushCore
        (expression: Expression)
        (lookup: int -> int)
        (position: int)
        (depth: int) : Spes =
    let mutable spes = zeroSpes

    let process1 f e = spes <- f <| exprPushCore e lookup position depth
    let process2 f e1 e2 =
        spes <- exprPushCore e1 lookup position depth
        let p, d = position + opLen (fst spes), depth + 1
        spes <- f spes <| exprPushCore e1 lookup p d

    // Only use with commutative operations.
    let inner e =
        let (p, d) = match spes with
                     | [PUSH0], _ -> position, depth
                     | code, _ -> position + opLen code, depth + 1
        exprPushCore e lookup p d
    let processList f s lst =
        spes <- s
        for e in lst do spes <- f spes (inner e)

    match expression with
    | ENum n -> spes <- [PUSH0], n
    | ELabel i -> spes <- [GET_PC], int64 (lookup i - position - 1)

    | EStack e ->
        spes <- [GET_STACK], 0L
        let s = inner e |> timesNSpes 8L
        spes <- addSpes spes s

    | ESum lst ->
        let f e =
            match e with
            | ELabel _ -> 1 | EMinus (ELabel _) -> -1
            | EStack _ -> 2 | EMinus (EStack _) -> -2
            | _ -> 0
        let cnt = new Map<int,int>(List.countBy f lst)
        let nPc = mapGet cnt 1 0 - mapGet cnt -1 0
        let nSp = mapGet cnt 2 0 - mapGet cnt -2 0

        spes <- addSpes spes <| timesNSpes (int64 nPc) ([GET_PC], 0L)
        spes <- addSpes spes <| timesNSpes (int64 nSp) ([GET_STACK], 0L)

        for ex in lst do
            let s = match ex with
                    | ELabel i -> [PUSH0], int64 (lookup i - position)
                    | EMinus (ELabel i) -> [PUSH0], int64 (position - lookup i)
                    | EStack e -> inner e |> timesNSpes 8L
                    | EMinus (EStack e) -> inner e |> timesNSpes -8L
                    | e -> inner e
            spes <- addSpes spes s

    | EProd lst -> processList multSpes oneSpes lst
    | EMinus e -> process1 minusSpes e
    | EPow2 e -> spes <- pow2Spes <| inner e
    | EConj lst -> processList andSpes trueSpes lst
    | EDisj lst -> processList orSpes zeroSpes lst
    | EXor lst -> processList xorSpes zeroSpes lst
    | ENeg e -> process1 negSpes e
    | ESign1 e -> process1 sign1Spes e
    | ESign2 e -> process1 sign2Spes e
    | ESign4 e -> process1 sign4Spes e

    | EDivU (e1, e2) -> process2 divUSpes e1 e2
    | EDivS (e1, e2) -> process2 divSSpes e1 e2
    | ERemU (e1, e2) -> process2 remUSpes e1 e2
    | ERemS (e1, e2) -> process2 remSSpes e1 e2

    | EEq (e1, e2) -> process2 eqSpes e1 e2
    | ELtU (e1, e2) -> process2 ltUSpes e1 e2
    | ELtS (e1, e2) -> process2 ltSSpes e1 e2
    | ELtEU (e1, e2) -> process2 lteUSpes e1 e2
    | ELtES (e1, e2) -> process2 lteSSpes e1 e2
    | EGtU (e1, e2) -> process2 gtUSpes e1 e2
    | EGtS (e1, e2) -> process2 gtSSpes e1 e2
    | EGtEU (e1, e2) -> process2 gteUSpes e1 e2
    | EGtES (e1, e2) -> process2 gteSSpes e1 e2

    | ELoad1 e -> spes <- collapseSpes (inner e) @ [LOAD1], 0L
    | ELoad2 e -> spes <- collapseSpes (inner e) @ [LOAD2], 0L
    | ELoad4 e -> spes <- collapseSpes (inner e) @ [LOAD4], 0L
    | ELoad8 e -> spes <- collapseSpes (inner e) @ [LOAD8], 0L

    // Finally we are able to return the result:
    spes


let expressionPush e lookup = exprPushCore e lookup 0 0 |> collapseSpes

let expressionAdd e lookup =
    match exprPushCore e lookup 0 0 with
    | [PUSH0], 0L -> []
    | spes -> collapseSpes spes @ [ADD]

let expressionMult e lookup =
    match exprPushCore e lookup 0 0 with
    | [PUSH0], 1L -> []
    | [PUSH0], 0L -> pop 1 @ [PUSH0] // Pop value, push 0.
    | spes -> collapseSpes spes @ [MULTIPLY]

let expressionAnd e lookup =
    match exprPushCore e lookup 0 0 with
    | [PUSH0], -1L -> []
    | [PUSH0], 0L -> pop 1 @ [PUSH0] // Pop value, push 0.
    | spes -> collapseSpes spes @ [AND]

let expressionOr e lookup =
    match exprPushCore e lookup 0 0 with
    | [PUSH0], 0L -> []
    | [PUSH0], -1L -> pop 1 @ [PUSH0; NOT] // Pop value, push -1.
    | spes -> collapseSpes spes @ [OR]

let expressionXor e lookup =
    match exprPushCore e lookup 0 0 with
    | [PUSH0], 0L -> []
    | [PUSH0], -1L -> [NOT]
    | spes -> collapseSpes spes @ [XOR]


let expressionDivU e lookup =
    match exprPushCore e lookup 0 0 with
    | [PUSH0], 1L -> []
    | spes -> collapseSpes spes @ [DIVIDE]

let expressionRemU e lookup =
    match exprPushCore e lookup 0 0 with
    | [PUSH0], 1L -> pop 1 @ [PUSH0] // Pop value, push 0.
    | spes -> collapseSpes spes @ [REMAINDER]

let expressionDivS e lookup =
    match exprPushCore e lookup 0 0 with
    | [PUSH0], 1L -> []
    | [PUSH0], -1L -> changeSign
    | spes -> collapseSpes spes @ Helpers.divS

let expressionRemS e lookup =
    match exprPushCore e lookup 0 0 with
    | [PUSH0], 1L -> pop 1 @ [PUSH0] // Pop value, push 0.
    | [PUSH0], -1L -> pop 1 @ [PUSH0] // Pop value, push 0.
    | spes -> collapseSpes spes @ Helpers.remS


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
            | SNeg :: r -> frag r [NOT]
            | SMinus :: r -> frag r changeSign
            | SPow2 :: r -> frag r [POW2]

            | SSetSp :: r -> frag r [SET_STACK]
            | SAlloc :: r -> frag r [ALLOCATE]
            | SDealloc :: r -> frag r [DEALLOCATE]
            | SLoad1 :: r -> frag r [LOAD1]
            | SLoad2 :: r -> frag r [LOAD2]
            | SLoad4 :: r -> frag r [LOAD4]
            | SLoad8 :: r -> frag r [LOAD8]
            | SSign1 :: r -> frag r [SIGN1]
            | SSign2 :: r -> frag r [SIGN2]
            | SSign4 :: r -> frag r [SIGN4]
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

            | SJump :: r -> frag r [JUMP]
            | SJumpZero :: r -> frag r genericJumpZero
            | SJumpNotZero :: r -> frag r genericJumpNotZero
            | SAdd :: r -> frag r [ADD]
            | SMult :: r -> frag r [MULTIPLY]
            | SAnd :: r -> frag r [AND]
            | SOr :: r -> frag r [OR]
            | SXor :: r -> frag r [XOR]
            | SDivU :: r -> frag r [DIVIDE]
            | SRemU :: r -> frag r [REMAINDER]
            | SDivS :: r -> frag r Helpers.divS
            | SRemS :: r -> frag r Helpers.remS

            | SLtU :: r -> frag r Helpers.ltU
            | SLtS :: r -> frag r Helpers.ltS
            | SLtEU :: r -> frag r Helpers.lteU
            | SLtES :: r -> frag r Helpers.lteS
            | SEq :: r -> frag r Helpers.eq
            | SGtEU :: r -> frag r Helpers.gteU
            | SGtES :: r -> frag r Helpers.gteS
            | SGtU :: r -> frag r Helpers.gtU
            | SGtS :: r -> frag r Helpers.gtS

            // Minor optimization.
            | SPush (ELabel i) :: SLabel j :: r when i = j ->
                frag (SLabel j :: r) [GET_PC]

            | SPush e :: r -> fragment r (expressionPush e)

            | [] -> failwith "Impossible case"
    }
