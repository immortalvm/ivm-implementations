﻿module Assembler.Target

open Machine.Instructions
open Assembler.Ast
open Machine.Utils


let opLen (ops: int8 list) = List.length ops

// Ensure monotinicity by adding NOPs if necessary.
let nopsFor i = List.replicate i NOP

let powers = [|for i in 0 .. 63 -> 1UL <<< i|]

let bytes (n: int) (y: uint64) : int8 list =
    [|0..n-1|]
    |> Seq.map (fun i -> y >>> i*8 |> uint8 |> int8)
    |> Seq.toList

let (|Power2|_|) (n: uint64) : int option =
    if n &&& (n - 1UL) = 0UL
    then System.Array.BinarySearch (powers, n) |> Some
    else None

let pushNum (x: int64): int8 list =
    let n = uint64 x
    let nn = x ^^^ -1L |> uint64
    let b8 = 1UL <<< 8
    let b16 = 1UL <<< 16
    let b32 = 1UL <<< 32
    match n, nn with
    | 0UL, _ -> [PUSH0]                                  // 1
    | _, 0UL -> [PUSH0; NOT]                             // 2 (pushTrue)
    | _, _ when n < b8 -> [PUSH1; int8 n]                // 2
    | _, _ when nn < b8 -> [PUSH1; int8 nn; NOT]         // 3
    | _, _ when n < b16 -> [PUSH2] @ bytes 2 n           // 3
    | Power2(k), _ -> [PUSH1; int8 k; POW2]              // 3
    | _, _ when nn < b16 -> [PUSH2] @ bytes 2 nn @ [NOT] // 4
    | _, Power2(k) -> [PUSH1; int8 k; POW2; NOT]         // 4
    | _, _ when n < b32 -> [PUSH4] @ bytes 4 n           // 5
    | _, _ when nn < b32 -> [PUSH4] @ bytes 4 nn @ [NOT] // 6
    | _, _ -> [PUSH8] @ bytes 8 n                        // 9

let pushInt = int64 >> pushNum

// If x = pushFix f, then x = pushInt (f (length x)) @ [NOP; ...].
let pushFix (f: int -> int64) =
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

let addr n = [GET_SP] @ if n = 0 then [] else pushNum (n * 8 |> int64) @ [ADD]
let get n = addr n @ [LOAD8]
let set n = addr n @ [STORE8]

// Using this with negative n is probably not a good idea.
let pop n =
    let pop1 = [JZ_FWD; 0y]
    match n with
    | 0 -> []
    | 1 -> pop1
    | 2 -> pop1 @ pop1
    | n -> addr n @ [SET_SP]

let pushTrue = [PUSH0; NOT] // Push -1
let changeSign = pushTrue @ [MULT]
let isZero = [PUSH1; 1y; LT]

// -1 if positive or zero, 0 if negative
let isPositive = [PUSH1; 63y; POW2; LT]

// 1 or -1
let toSign = isPositive @ [ PUSH1; 2y; MULT; NOT ]
let absolute = get 0 @ toSign @ [MULT] // Keeps 2^63 unchanged
let divS =
    List.concat
        [
            // x :: y :: rest
            get 1 @ absolute
            get 1 @ absolute
            [DIV]
            // d :: x :: y :: rest, where d = abs y / abs x.
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
let remS =
    List.concat
        [
            // x :: y :: rest
            get 1 @ absolute
            get 1 @ absolute
            [REM]
            // d :: x :: y :: rest, where d = abs y % abs x.
            get 2 @ toSign
            // s :: d :: ..., where s = 1 if y is >= 0, otherwise -1.
            [MULT]
            // d' :: x :: y :: rest, where d' = d or d' = -d.
            set 2
            // x :: 'd :: rest
            pop 1
        ]

// Round towards zero
let oldDivSU =
    List.concat
        [
            // x :: y :: rest
            get 1 @ absolute
            get 1
            [DIV]
            get 2 @ toSign
            // sign(y) :: |y| / x :: x :: y :: rest
            [MULT]
            set 2
            pop 1
        ]

// Round towards negative infinity.
// Used for arithmetic shift right.
// Division by zero: arbitrary value.
let divSU =
    List.concat
        [
             // x :: y :: rest
            get 1 @ [PUSH1; 63y; POW2; LT; NOT]       // -1 if y<0, else 0
            get 0 @ [PUSH1; 2y; MULT; PUSH1; 1y; ADD] // -1 if y<0, else 1
            // (-1 or 1) :: (-1 or 0) :: x :: y :: rest
            get 0 @ get 4 @ [MULT]                    // absolute value
            get 2 @ [ADD]                             // subtract 1 if y<0
            get 3
            // x :: [y] :: (-1 or 1) :: (-1 or 0) :: x :: y :: rest
            [DIV; MULT; ADD]
            // result :: x :: y :: rest
            set 2
            pop 1
       ]


let offsetSign = pushNum (1L <<< 63) @ [ADD]
let offsetSign2 = offsetSign @ get 1 @ offsetSign @ set 2

let eq = [XOR] @ isZero

let ltU = [LT]
let gtU = get 1 @ ltU @ set 1
let lteU = gtU @ isZero
let gteU = ltU @ isZero

let ltS = offsetSign2 @ ltU
let lteS = offsetSign2 @ lteU
let gtS = offsetSign2 @ gtU
let gteS = offsetSign2 @ gteU


let byteDist x = -256L <= x && x <= 255L

let jumpZero (offset: int64) =
    if offset >= 0L
    then [JZ_FWD; int8 offset]
    else [JZ_BACK; int8 <| abs offset - 1L]


// NB. The delta is w.r.t. before the code.
let deltaJump delta =
    if delta = 0L then []
    elif byteDist <| delta - 3L then [PUSH0] @ jumpZero (delta - 3L)
    elif delta < 0L then [GET_PC] @ pushNum (delta - 1L) @ [ADD; JUMP]
    else 
        let f pushLength = delta - int64 pushLength - 1L
        pushFix f @ [GET_PC; ADD; JUMP]

let deltaJumpZero delta =
    if byteDist <| delta - 2L then jumpZero (delta - 2L)
    else
        let jump = deltaJump (delta - 5L)
        jumpZero 3L @ [PUSH0] @ jumpZero (opLen jump |> int64) @ jump

let deltaJumpNotZero delta = isZero @ deltaJumpZero (delta - int64 (opLen isZero))

let genericConditional transformer =
    let ifNotZero = set 1 @ [JUMP]
    List.concat [
        get 1
        transformer
        jumpZero (ifNotZero |> opLen |> int64)
        ifNotZero
        pop 2
    ]

let genericJumpNotZero = genericConditional []
let genericJumpZero = genericConditional isZero

let unsafeSigx b = get 0 @ pushNum (1L <<< b - 1) @ [AND] @ pushNum -1L @ [MULT; OR]
let sigx b = pushNum ((1L <<< b) - 1L) @ [AND] @ unsafeSigx b

let shiftrs (k: int) =
    if k <= 0 then []
    elif k > 63 then [PUSH0; MULT]
    elif k = 63 then isPositive @ [NOT]
    else // 0<k<63
        pushNum (int64 k) @ [POW2; DIV] @ unsafeSigx (64 - k)

// Pair consisting of (i) a piece of code with the net effect of pushing a single
// value onto the stack and (ii) a constant "offset" which should (at some point)
// be added to this value. I could not think of a good name for this type.
type Value = Val of (int8 list) * int64

let constant offset = Val([PUSH0], offset)
let noOffset code = Val(code, 0L)

let (|Const|_|) (v: Value) =
    match v with
    | Val([PUSH0], n) -> Some n
    | _ -> None

let (|Power2Const|_|) (v: Value) =
    match v with
    | Const(n) -> match uint64 n with
                  | Power2(k) -> Some k
                  | _ -> None
    | _ -> None

let (|NoOff|_|) (v: Value) =
    match v with
    | Val(c, 0L) -> Some c
    | _ -> None

let (|Rel|_|) (v: Value) =
    match v with
    | Val([GET_PC], n) -> Some n
    | _ -> None

let collapseValue (v: Value) : int8 list =
    match v with
    | NoOff(c) -> c
    | Const(n) -> pushNum n
    | Val(c, n) -> c @ pushNum n @ [ADD]

let zeroValue = constant 0L
let oneValue = constant 1L
let trueValue = constant -1L

let (|Zero|One|True|OtherValue|) (v: Value) =
    match v with
    | Const(0L) -> Zero
    | Const(1L) -> One
    | Const(-1L) -> True
    | _ -> OtherValue

let addValue v1 v2 =
    match v1, v2 with
    | Val(c1, m), Const(n) -> Val(c1, m + n)
    | Const(m), Val(c2, n) -> Val(c2, m + n)
    | Val(c1, m), Val(c2, n) -> Val(c1 @ c2 @ [ADD], m + n)

let negValue (v: Value) : Value =
    match v with
    | Const(n) -> constant -n
    | Val(c, n) -> Val(c @ changeSign, -n)

let multValue (v1: Value) (v2: Value) : Value =
    match v1, v2 with
    | Const(m), Const(n) -> constant (m * n)
    | _, Zero
    | Zero, _ -> zeroValue
    | Val(c, m), Const(n) -> Val(c @ pushNum n @ [MULT], m * n)
    | Const(m), Val(c, n) -> Val(c @ pushNum m @ [MULT], m * n)
    | _, _ -> noOffset <| collapseValue v1 @ collapseValue v2 @ [MULT]

let pow2Value (v: Value) : Value =
    match v with
    | Const(n) -> constant <| if n < 0L || n > 63L then 0L else 1L <<< int n
    | _ -> noOffset <| collapseValue v @ [POW2]

let andValue (v1: Value) (v2: Value) : Value =
    match v1, v2 with
    | Const(m), Const(n) -> constant (m &&& n)
    | _, Zero -> zeroValue
    | Zero, _ -> zeroValue
    | _ -> noOffset <| collapseValue v1 @ collapseValue v2 @ [AND]

let orValue (v1: Value) (v2: Value) : Value =
    match v1, v2 with
    | Const(m), Const(n) -> constant (m ||| n)
    | _, True
    | True, _ -> trueValue
    | _ -> noOffset <| collapseValue v1 @ collapseValue v2 @ [OR]

let xorValue (v1: Value) (v2: Value) : Value =
    match v1, v2 with
    | Const(m), Const(n) -> constant (m ^^^ n)
    | _ -> noOffset <| collapseValue v1 @ collapseValue v2 @ [XOR]

let notValue (v: Value) : Value =
    match v with
    | Const(n) -> constant (n ^^^ -1L)
    | Val(c, n) -> Val(c @ [NOT], (n ^^^ -1L) + 1L)

let sigx1Value (unsafe: bool) (v: Value) : Value =
    match v with
    | Const(n) -> constant (uint64 n |> signExtend1 |> int64)
    | _ -> noOffset <| collapseValue v @ if unsafe then unsafeSigx 8 else sigx 8

let sigx2Value (unsafe: bool) (v: Value) : Value =
    match v with
    | Const(n) -> constant (uint64 n |> signExtend2 |> int64)
    | _ -> noOffset <| collapseValue v @ if unsafe then unsafeSigx 16 else sigx 16

let sigx4Value (unsafe: bool)(v: Value) : Value =
    match v with
    | Const(n) -> constant (uint64 n |> signExtend4 |> int64)
    | _ -> noOffset <| collapseValue v @ if unsafe then unsafeSigx 32 else sigx 32


let divUValue (v1: Value) (v2: Value) : Value =
    match v1, v2 with
    | Zero, _
    | _, Zero -> zeroValue // x / 0 = 0 !
    | _, One -> v1
    | Const(m), Const(n) -> constant (uint64 m / uint64 n |> int64)
    | _, _ -> noOffset <| collapseValue v1 @ collapseValue v2 @ [DIV]

let divSValue (v1: Value) (v2: Value) : Value =
    match v1, v2 with
    | Zero, _
    | _, Zero -> zeroValue // x / 0 = 0 !
    | _, One -> v1
    | Const(m), Const(n) -> constant (m / n)
    | Val(c, m), True -> Val(c @ changeSign, -m)
    | _, Const(n) when n > 0L -> noOffset <| collapseValue v1 @ pushNum n @ oldDivSU
    | _, _ -> noOffset <| collapseValue v1 @ collapseValue v2 @ divS

let divSUValue (v1: Value) (v2: Value) : Value =
    match v1, v2 with
    | _, Zero // arbitrary
    | Zero, _
    | _, One
    | True, _ -> v1
    | Const(m), Const(n) ->
        let off, sign = if m < 0L then -1L, -1L else 0L, 1L
        uint64 (m * sign + off) / (uint64 n) |> int64 |> (*) sign |> (+) off |> constant
    | _, Power2Const(k) -> noOffset <| collapseValue v1 @ shiftrs k
    | _, _ -> noOffset <| collapseValue v1 @ collapseValue v2 @ divSU

let remUValue (v1: Value) (v2: Value) : Value =
    match v1, v2 with
    | _, One
    | Zero, _
    | _, Zero -> zeroValue // x % 0 = 0 !
    | Const(m), Const(n) -> constant ((uint64 m % uint64 n |> int64))
    | _, _ -> noOffset <| collapseValue v1 @ collapseValue v2 @ [REM]

let remSValue (v1: Value) (v2: Value) : Value =
    match v1, v2 with
    | _, One
    | Zero, _
    | _, True
    | _, Zero -> zeroValue // x % 0 = 0 !
    | Const(m), Const(n) -> constant (m % n)
    | _, _ -> noOffset <| collapseValue v1 @ collapseValue v2 @ remS

let eqValue (v1: Value) (v2: Value) : Value =
    match v1, v2 with
    | Const(m), Const(n) -> if m = n then trueValue else zeroValue
    | _, _ -> noOffset <| collapseValue v1 @ collapseValue v2 @ eq

let ltUValue (v1: Value) (v2: Value) : Value =
    match v1, v2 with
    | Const(m), Const(n) -> if uint64 m < uint64 n then trueValue else zeroValue
    | _, _ -> noOffset <| collapseValue v1 @ collapseValue v2 @ ltU

let ltSValue (v1: Value) (v2: Value) : Value =
    match v1, v2 with
    | Const(m), Const(n) -> if m < n then trueValue else zeroValue
    | _, _ -> noOffset <| collapseValue v1 @ collapseValue v2 @ ltS

let lteUValue (v1: Value) (v2: Value) : Value =
    match v1, v2 with
    | Const(m), Const(n) -> if uint64 m <= uint64 n then trueValue else zeroValue
    | _, _ -> noOffset <| collapseValue v1 @ collapseValue v2 @ lteU

let lteSValue (v1: Value) (v2: Value) : Value =
    match v1, v2 with
    | Const(m), Const(n) -> if m <= n then trueValue else zeroValue
    | _, _ -> noOffset <| collapseValue v1 @ collapseValue v2 @ lteS

let gtUValue (v1: Value) (v2: Value) : Value =
    match v1, v2 with
    | Const(m), Const(n) -> if uint64 m > uint64 n then trueValue else zeroValue
    | _, _ -> noOffset <| collapseValue v1 @ collapseValue v2 @ gtU

let gtSValue (v1: Value) (v2: Value) : Value =
    match v1, v2 with
    | Const(m), Const(n) -> if m > n then trueValue else zeroValue
    | _, _ -> noOffset <| collapseValue v1 @ collapseValue v2 @ gtS

let gteUValue (v1: Value) (v2: Value) : Value =
    match v1, v2 with
    | Const(m), Const(n) -> if uint64 m >= uint64 n then trueValue else zeroValue
    | _, _ -> noOffset <| collapseValue v1 @ collapseValue v2 @ gteU

let gteSValue (v1: Value) (v2: Value) : Value =
    match v1, v2 with
    | Const(m), Const(n) -> if m >= n then trueValue else zeroValue
    | _, _ -> noOffset <| collapseValue v1 @ collapseValue v2 @ gteS


let exprPushCore (lookup: int -> int) =
    let rec epc (position: int) (depth: int) (expression: Expression) =

        //printfn "%A %A %A" position depth expression
        let rec1 e = epc position depth e

        let rec1coll e = rec1 e |> collapseValue

        let relRec s e =
            let c = collapseValue s
            epc (position + List.length c) (depth + 1) e

        let rec2 f e1 e2 = let s = rec1 e1 in f s <| relRec s e2

        let optM f u x y = match x, y with
                           | Const(m), _ when m = u -> y
                           | _, Const(n) when n = u -> x
                           | _, _ -> f x y

        let foldM (f: Value -> Value -> Value) (u: int64) (lst: Expression list) =
            let fu = optM f u
            let fe s e = match s with
                         | Const(_) -> fu (rec1 e) s
                         | _ -> fu s (relRec s e)
            List.fold fe (constant u) lst

        match expression with
        | EOffset (n, e) -> epc position (depth + n) e
        | ENum n -> constant (n)
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

            let mutable value = zeroValue
            let multN n s = optM multValue 1L s <| constant (int64 n)
            value <- addValue value <| multN nPc (noOffset [GET_PC])
            let pcOffset = if nPc = 0 then 0 else 1
            let pcPos = position + pcOffset
            value <- addValue value <| multN nSp (Val([GET_SP], (depth + pcOffset) * 8 |> int64))

            for ex in lst do
                let p, d = match value with
                           | Const(_) -> position, depth
                           | Val(code, _) -> position + opLen code, depth + 1
                let inner = epc p d
                let s = match ex with
                        | ELabel i -> constant (int64 (lookup i - pcPos))
                        | ENeg (ELabel i) -> constant (int64 (pcPos - lookup i))
                        | EStack e -> inner e |> multN 8
                        | ENeg (EStack e) -> inner e |> multN -8
                        | e -> inner e
                value <- addValue value s
            // Finally return the result
            value

        | ENeg e -> e |> rec1 |> negValue
        | EPow2 e -> e |> rec1 |> pow2Value
        | ENot e -> e |> rec1 |> notValue
        | ESigx1 (ELoad1 _ as e) -> e |> rec1 |> sigx1Value true
        | ESigx2 (ELoad2 _ as e) -> e |> rec1 |> sigx2Value true
        | ESigx4 (ELoad4 _ as e) -> e |> rec1 |> sigx4Value true
        | ESigx1 e -> e |> rec1 |> sigx1Value false
        | ESigx2 e -> e |> rec1 |> sigx2Value false
        | ESigx4 e -> e |> rec1 |> sigx4Value false
        | ELoad1 e -> noOffset <| rec1coll e @ [LOAD1]
        | ELoad2 e -> noOffset <| rec1coll e @ [LOAD2]
        | ELoad4 e -> noOffset <| rec1coll e @ [LOAD4]
        | ELoad8 e -> noOffset <| rec1coll e @ [LOAD8]

        | EProd lst -> foldM multValue 1L lst
        | EConj lst -> foldM andValue -1L lst
        | EDisj lst -> foldM orValue 0L lst
        | EXor lst -> foldM xorValue 0L lst

        | EDivU (e1, e2) -> rec2 divUValue e1 e2
        | EDivS (e1, e2) -> rec2 divSValue e1 e2
        | EDivSU (e1, e2) -> rec2 divSUValue e1 e2
        | ERemU (e1, e2) -> rec2 remUValue e1 e2
        | ERemS (e1, e2) -> rec2 remSValue e1 e2

        | EEq (e1, e2) -> rec2 eqValue e1 e2
        | ELtU (e1, e2) -> rec2 ltUValue e1 e2
        | ELtS (e1, e2) -> rec2 ltSValue e1 e2
        | ELtEU (e1, e2) -> rec2 lteUValue e1 e2
        | ELtES (e1, e2) -> rec2 lteSValue e1 e2
        | EGtU (e1, e2) -> rec2 gtUValue e1 e2
        | EGtS (e1, e2) -> rec2 gtSValue e1 e2
        | EGtEU (e1, e2) -> rec2 gteUValue e1 e2
        | EGtES (e1, e2) -> rec2 gteSValue e1 e2

    epc // Return the recursive function

let expressionConst line e lookup =
    match exprPushCore lookup 0 0 e with
    | Const(x) -> x
    | _ -> failwithf "Non-constant data expression on line %d." line

let expressionData n line e lookup =
    expressionConst line e lookup |> uint64 |> bytes n

let expressionDataRelative line e lookup =
    match exprPushCore lookup 0 0 e with
    | Rel(x) -> bytes 8 (uint64 x + 1UL) // cf. export
    | _ -> failwithf "Something wrong with line %d." line

let expressionPush e lookup =
    exprPushCore lookup 0 0 e |> collapseValue

let export line i e lookup =
    match exprPushCore lookup 0 0 e with
    | Const(x) -> i, false, x
    | Rel(x) -> i, true, x + 1L // cf. expressionDataRelative
    | _ -> failwithf "Illegal export on line: %d" line

let expressionAdd e lookup =
    match exprPushCore lookup 0 0 e with
    | Zero -> []
    | v -> collapseValue v @ [ADD]

let expressionMult e lookup =
    match exprPushCore lookup 0 0 e with
    | One -> []
    | Zero -> pop 1 @ [PUSH0] // Pop value, push 0.
    | v -> collapseValue v @ [MULT]

let expressionAnd e lookup =
    match exprPushCore lookup 0 0 e with
    | True -> []
    | Zero -> pop 1 @ [PUSH0] // Pop value, push 0.
    | v -> collapseValue v @ [AND]

let expressionOr e lookup =
    match exprPushCore lookup 0 0 e with
    | Zero -> []
    | True -> pop 1 @ pushTrue // Pop value, push -1.
    | v -> collapseValue v @ [OR]

let expressionXor e lookup =
    match exprPushCore lookup 0 0 e with
    | Zero -> []
    | True -> [NOT]
    | v -> collapseValue v @ [XOR]


let expressionDivU e lookup =
    match exprPushCore lookup 0 0 e with
    | Zero -> pop 1 @ [PUSH0] // NB: x / 0 = 0
    | One -> []
    | v -> collapseValue v @ [DIV]

let expressionRemU e lookup =
    match exprPushCore lookup 0 0 e with
    | Zero -> pop 1 @ [PUSH0] // NB: x % 0 = 0
    | One -> pop 1 @ [PUSH0] // Pop value, push 0.
    | v -> collapseValue v @ [REM]

let expressionDivS e lookup =
    match exprPushCore lookup 0 0 e with
    | Zero -> pop 1 @ [PUSH0] // NB: x / 0 = 0
    | One -> []
    | True -> changeSign
    | Const(n) when n > 0L -> pushNum n @ oldDivSU
    | v -> collapseValue v @ divS

let expressionRemS e lookup =
    match exprPushCore lookup 0 0 e with
    | Zero -> pop 1 @ [PUSH0] // NB: x % 0 = 0
    | One -> pop 1 @ [PUSH0] // Pop value, push 0.
    | True -> pop 1 @ [PUSH0] // Pop value, push 0.
    | v -> collapseValue v @ remS

let expressionDivSU e lookup =
    match exprPushCore lookup 0 0 e with
    | Zero // arbitrary
    | One -> []
    | Power2Const(k) -> shiftrs k
    | v -> collapseValue v @ divSU

type Intermediate =
    | Label of int
    | Relative
    | Spacer of int * ((int -> int) -> int64)
    | Fragment of ((int -> int) -> int8 list)
    | Export of ((int -> int) -> int * bool * int64)

let (|LabelOffset|_|) (e: Expression) =
    match e with
    | ELabel i -> Some (i, 0L)
    | ESum [ENum o; ELabel i] -> Some (i, o)
    | ESum [ELabel i; ENum o] -> Some (i, o)
    | _ -> None

let intermediates (prog: Statement list) : seq<Intermediate> =
    let mutable rest = prog

    let fragment r f = rest <- r; [Fragment f]
    let frag r code = fragment r <| fun _ -> code
    let withDelta i o r f = rest <- r; [Fragment <| fun lookup -> lookup i |> int64 |> (+) o |> f]

    seq {
        while not rest.IsEmpty do
        yield!
            match rest with
            | SExport (line, i, e) :: r -> rest <- r; [Export (export line i e)]

            | SExit :: r -> frag r [EXIT]
            | SNot :: r -> frag r [NOT]
            | SNeg :: r -> frag r changeSign
            | SPow2 :: r -> frag r [POW2]

            | SSetSp :: r -> frag r [SET_SP]

            | SLoad1 :: SSigx1 :: r -> frag r <| [LOAD1] @ unsafeSigx 8
            | SLoad2 :: SSigx2 :: r -> frag r <| [LOAD2] @ unsafeSigx 16
            | SLoad4 :: SSigx4 :: r -> frag r <| [LOAD4] @ unsafeSigx 32
            | SLoad1 :: r -> frag r [LOAD1]
            | SLoad2 :: r -> frag r [LOAD2]
            | SLoad4 :: r -> frag r [LOAD4]
            | SLoad8 :: r -> frag r [LOAD8]
            | SSigx1 :: r -> frag r <| sigx 8
            | SSigx2 :: r -> frag r <| sigx 16
            | SSigx4 :: r -> frag r <| sigx 32
            | SStore1 :: r -> frag r [STORE1]
            | SStore2 :: r -> frag r [STORE2]
            | SStore4 :: r -> frag r [STORE4]
            | SStore8 :: r -> frag r [STORE8]

            | SLabel i :: SSpacer (ln, e) :: r ->
                rest <- r
                [Label i; Spacer (i, expressionConst ln e)]

            // Avoid optimizing away tight infinite loops.
            | SLabel i :: SPush (ELabel j) :: SJump :: r when i = j ->
                [Label i] @ frag r [PUSH0; JZ_BACK; 2y]

            | SLabel i :: r -> rest <- r; [Label i]

            | SPush (LabelOffset (i, o)) :: SJump :: r -> withDelta i o r deltaJump
            | SPush (LabelOffset (i, o)) :: SJumpZero :: r -> withDelta i o r deltaJumpZero
            | SPush (LabelOffset (i, o)) :: SJumpNotZero :: r -> withDelta i o r deltaJumpNotZero
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
            // In case of --noopt
            | SPush e :: SPow2 :: SDivSU :: r -> fragment r (expressionDivSU (EPow2 e))

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
            | SData8 (line, (LabelOffset (i, o) as e)) :: r ->
                Relative :: (fragment r (expressionDataRelative line e))
            | SData1 (line, e) :: r -> fragment r (expressionData 1 line e)
            | SData2 (line, e) :: r -> fragment r (expressionData 2 line e)
            | SData4 (line, e) :: r -> fragment r (expressionData 4 line e)
            | SData8 (line, e) :: r -> fragment r (expressionData 8 line e)

            | SCheck :: r -> frag r [CHECK]

            | SNewFrame  :: r -> frag r [NEW_FRAME]
            | SSetPixel  :: r -> frag r [SET_PIXEL]
            | SAddSample :: r -> frag r [ADD_SAMPLE]
            | SPutChar   :: r -> frag r [PUT_CHAR]
            | SPutByte   :: r -> frag r [PUT_BYTE]

            | SReadFrame :: r -> frag r [READ_FRAME]
            | SReadPixel :: r -> frag r [READ_PIXEL]
            | SReadChar :: r -> frag r [READ_CHAR]

            | _ -> failwithf "Impossible case: %O" rest
    }
