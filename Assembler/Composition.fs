﻿module Assembler.Composition


[<Literal>]
let private ATTEMPTS_BEFORE_MONOTINICITY = 3;

// Code given label -> relative position (from statement _start_)
type FlexCode = (int -> int) -> int8 list

type Intermediate =
    | Label of int
    | Fragment of FlexCode


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
    let codes = Array.create pLength []

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
                                    else let l0 = List.length codes.[num]
                                         let l1 = List.length c
                                         if l1 >= l0 then c
                                         else c @ nops (l0 - l1)
                position <- position + List.length codes.[num]

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
let JUMP_IF_ZERO = 3y // Relative to next signed byte (immediate arg)

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
let SIGN1 = 12uy // From 8 to 64 bits

[<Literal>]
let SIGN2 = 13uy // From 16 to 64 bits

[<Literal>]
let SIGN4 = 14uy // From 32 to 64 bits


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
            |> Seq.map (fun i -> y >>> i*8 |> byte |> int8)
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
        let nextLen = List.length currRes
        if nextLen <= currLen
        then result <- Some <| currRes @ nopsFor (currLen - nextLen)
        else currLen <- nextLen
    result.Value


open Assembler.Ast

let byteDist x = -128 <= x && x <= 127

// NB. The delta is w.r.t. before the code.
let deltaJump delta =
    if delta = 0 then []
    elif byteDist <| delta - 3 then [PUSH0; JUMP_IF_ZERO; int8 (delta - 3)]
    elif delta < 0 then [GET_PC] @ pushInt (delta - 1) @ [ADD; JUMP]
    else 
        let f pushLength = delta - pushLength - 1
        pushFix f @ [GET_PC; ADD; JUMP]

let deltaJumpZero delta =
    if byteDist <| delta - 2 then [JUMP_IF_ZERO; int8 (delta - 2)]
    else
        let jump = deltaJump (delta - 5)
        [JUMP_IF_ZERO; 3y; PUSH0; JUMP_IF_ZERO; int8 (List.length jump)] @ jump 

let deltaJumpNotZero delta = [PUSH1; 1y; LESS_THAN] @ deltaJumpZero (delta - 3)

// TODO: There should be a built-in function like this.
let valueOr<'a> (def: 'a) (x: 'a option) = match x with Some z -> z | _ -> def
let mapGet<'a, 'b when 'a: comparison> (m: Map<'a,'b>) (x: 'a) (def: 'b) =
    m.TryFind x |> valueOr def

// TODO: Improve type name. Maybe a record would be better?
type Spes = int8 list * int64

let collapseSpes ((code, acc): Spes) : int8 list =
    match code, acc with
    | _, 0L -> code
    | [PUSH0], _ -> pushNum acc
    | _, _ -> code @ pushNum acc @ [ADD]

let zeroSpes: Spes = [PUSH0], 0L
let oneSpes: Spes = [PUSH0], 1L

let addSpes ((c1, a1): Spes) ((c2, a2): Spes) =
    let c = match c1, c2 with
            | _, [PUSH0] -> c1
            | [PUSH0], _ -> c2
            | _, _ -> c1 @ c2 @ [ADD]
    (c, a1 + a2)

let minusSpes ((code, acc): Spes) : Spes =
    match code with
    | [PUSH0] -> code, -acc
    | _ -> code @ [NOT; PUSH1; 1y; ADD], -acc

let multSpes (s1: Spes) (s2: Spes) : Spes =
    match s1, s2 with
    | ([PUSH0], m), ([PUSH0], n) -> ([PUSH0], m * n)
    | _, ([PUSH0], 0L) -> zeroSpes
    | ([PUSH0], 0L), _ -> zeroSpes
    | _, ([PUSH0], 1L) -> s1
    | ([PUSH0], 1L), _ -> s2
    | (c, m), ([PUSH0], n) -> c @ pushNum n @ [MULTIPLY], m * n
    | ([PUSH0], m), (c, n) -> c @ pushNum m @ [MULTIPLY], m * n
    | _, _ -> collapseSpes s1 @ collapseSpes s2 @ [MULTIPLY], 0L

let timesNSpes (n: int64) (s: Spes) : Spes = multSpes s ([PUSH0], n)

let rec exprPushCore
        (expression: Expression)
        (lookup: int -> int)
        (position: int)
        (depth: int) : Spes =
    let mutable spes = zeroSpes
    let inner e =
        let (p, d) = match spes with
                     | [PUSH0], _ -> position, depth
                     | code, _ -> position + List.length code, depth + 1
        exprPushCore e lookup p d

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

    | EProd lst ->
        spes <- oneSpes
        for e in lst do spes <- multSpes spes (inner e)

    // TODO
    | _ -> ()
    spes


let expressionPush e lookup = exprPushCore e lookup 0 0 |> collapseSpes

let expressionAdd e lookup =
    match exprPushCore e lookup 0 0 with
    | [PUSH0], 0L -> []
    | spes -> collapseSpes spes @ [ADD]

let expressionMult e lookup =
    match exprPushCore e lookup 0 0 with
    | [PUSH0], 1L -> []
    | [PUSH0], 0L -> [JUMP_IF_ZERO; 0y; PUSH0] // Pop value, push 0.
    | spes -> collapseSpes spes @ [MULTIPLY]

let intermediates (prog: Statement list) : seq<Intermediate> =
    let mutable rest = prog

    let fragment r f = rest <- r; [Fragment f]
    let frag r code = fragment r <| fun _ -> code
    let withDelta i r f = rest <- r; [Fragment <| fun lookup -> f (lookup i)]

    // We avoid recursion, just in case.
    seq {
        while not rest.IsEmpty do
        yield!
            match rest with

            | SExit :: r -> frag r [EXIT]
            | SNeg :: r -> frag r [NOT]
            | SMinus :: r -> frag r [NOT; PUSH1; 1y; ADD]
            | SSetSp :: r -> frag r [SET_STACK]
            | SData lst :: r -> frag r <| []

            // Avoid optimizing away tight infinite loop.
            | SLabel i :: SPush (ELabel j) :: SJump :: r when i = j ->
                [Label i] @ frag r [PUSH0; JUMP_IF_ZERO; -3y]

            | SLabel i :: r -> rest <- r; [Label i]

            | SPush (ELabel i) :: SJump :: r -> withDelta i r deltaJump
            | SPush (ELabel i) :: SJumpZero :: r -> withDelta i r deltaJumpZero
            | SPush (ELabel i) :: SJumpNotZero :: r -> withDelta i r deltaJumpNotZero

            | SPush e :: SAdd :: r -> fragment r (expressionAdd e)
            | SAdd :: r -> frag r [ADD]
            | SPush e :: SMult :: r -> fragment r (expressionMult e)
            | SMult :: r -> frag r [MULTIPLY]


            // Minor optimization.
            | SPush (ELabel i) :: SLabel j :: r when i = j ->
                frag (SLabel j :: r) [GET_PC]

            | SPush e :: r -> fragment r (expressionPush e)

            | _ -> failwithf "Unhandled statement: %O" rest.[0]
    }