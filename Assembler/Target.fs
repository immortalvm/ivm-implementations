module Assembler.Target

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


let byteDist x = -128L <= x && x <= 127L

// NB. The delta is w.r.t. before the code.
let deltaJump delta =
    if delta = 0L then []
    elif byteDist <| delta - 3L then [PUSH0; JUMP_ZERO; int8 (delta - 3L)]
    elif delta < 0L then [GET_PC] @ pushNum (delta - 1L) @ [ADD; JUMP]
    else 
        let f pushLength = delta - int64 pushLength - 1L
        pushFix f @ [GET_PC; ADD; JUMP]

let deltaJumpZero delta =
    if byteDist <| delta - 2L then [JUMP_ZERO; int8 (delta - 2L)]
    else
        let jump = deltaJump (delta - 5L)
        [JUMP_ZERO; 3y; PUSH0; JUMP_ZERO; int8 (opLen jump)] @ jump

let deltaJumpNotZero delta = isZero @ deltaJumpZero (delta - int64 (opLen isZero))

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

let sigx1Value (v: Value) : Value =
    match v with
    | Const(n) -> constant (uint64 n |> signExtend1 |> int64)
    | _ -> noOffset <| collapseValue v @ [SIGX1]

let sigx2Value (v: Value) : Value =
    match v with
    | Const(n) -> constant (uint64 n |> signExtend2 |> int64)
    | _ -> noOffset <| collapseValue v @ [SIGX2]

let sigx4Value (v: Value) : Value =
    match v with
    | Const(n) -> constant (uint64 n |> signExtend4 |> int64)
    | _ -> noOffset <| collapseValue v @ [SIGX4]


let divUValue (v1: Value) (v2: Value) : Value =
    match v1, v2 with
    | _, Zero -> zeroValue // x / 0 = 0 !
    | Const(m), Const(n) -> constant (uint64 m / uint64 n |> int64)
    | _, One -> v1
    | Zero, _ -> zeroValue
    | _, _ -> noOffset <| collapseValue v1 @ collapseValue v2 @ [DIV]

let divSValue (v1: Value) (v2: Value) : Value =
    match v1, v2 with
    | _, Zero -> zeroValue // x / 0 = 0 !
    | Const(m), Const(n) -> constant (m / n)
    | _, One -> v1
    | Zero, _ -> zeroValue
    | Val(c, m), True -> Val(c @ changeSign, -m)
    | _, Const(n) when n > 0L -> noOffset <| collapseValue v1 @ pushNum n @ divSU
    | _, _ -> noOffset <| collapseValue v1 @ collapseValue v2 @ divS

let divSUValue (v1: Value) (v2: Value) : Value =
    match v1, v2 with
    | _, Zero -> zeroValue // x / 0 = 0 !
    | Const(m), Const(n) ->
        let sign = if m < 0L then -1L else 1L
        ((m * sign |> uint64) / (uint64 n)) |> int64 |> (*) sign |> constant
    | _, One -> v1
    | Zero, _ -> zeroValue
    | _, _ -> noOffset <| collapseValue v1 @ collapseValue v2 @ divSU

let remUValue (v1: Value) (v2: Value) : Value =
    match v1, v2 with
    | _, Zero -> zeroValue // x % 0 = 0 !
    | Const(m), Const(n) -> constant ((uint64 m % uint64 n |> int64))
    | _, One -> zeroValue
    | Zero, _ -> zeroValue
    | _, _ -> noOffset <| collapseValue v1 @ collapseValue v2 @ [REM]

let remSValue (v1: Value) (v2: Value) : Value =
    match v1, v2 with
    | _, Zero -> zeroValue // x % 0 = 0 !
    | Const(m), Const(n) -> constant (m % n)
    | _, One -> zeroValue
    | Zero, _ -> zeroValue
    | _, True -> zeroValue
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
            value <- addValue value <| multN nSp (Val([GET_SP], depth * 8 |> int64))

            for ex in lst do
                let p, d = match value with
                           | Const(_) -> position, depth
                           | Val(code, _) -> position + opLen code, depth + 1
                let inner = epc p d
                let s = match ex with
                        | ELabel i -> constant (int64 (lookup i - p))
                        | ENeg (ELabel i) -> constant (int64 (p - lookup i))
                        | EStack e -> inner e |> multN 8
                        | ENeg (EStack e) -> inner e |> multN -8
                        | e -> inner e
                value <- addValue value s
            // Finally return the result
            value

        | ENeg e -> e |> rec1 |> negValue
        | EPow2 e -> e |> rec1 |> pow2Value
        | ENot e -> e |> rec1 |> notValue
        | ESigx1 e -> e |> rec1 |> sigx1Value
        | ESigx2 e -> e |> rec1 |> sigx2Value
        | ESigx4 e -> e |> rec1 |> sigx4Value
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
    | Rel(x) -> bytes 8 (uint64 x + 1UL)
    | _ -> failwithf "Something wrong with line %d." line

let expressionPush e lookup =
    exprPushCore lookup 0 0 e |> collapseValue

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
    | Const(n) when n > 0L -> pushNum n @ divSU
    | v -> collapseValue v @ divS

let expressionRemS e lookup =
    match exprPushCore lookup 0 0 e with
    | Zero -> pop 1 @ [PUSH0] // NB: x % 0 = 0
    | One -> pop 1 @ [PUSH0] // Pop value, push 0.
    | True -> pop 1 @ [PUSH0] // Pop value, push 0.
    | v -> collapseValue v @ remS

let expressionDivSU e lookup =
    match exprPushCore lookup 0 0 e with
    | Zero -> pop 1 @ [PUSH0] // NB: x / 0 = 0
    | One -> []
    | v -> collapseValue v @ divSU

type FlexCode = (int -> int) -> int8 list

type Intermediate =
    | Label of int
    | Relative
    | Spacer of int * ((int -> int) -> int64)
    | Fragment of FlexCode

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
            | SExit :: r -> frag r [EXIT]
            | SNot :: r -> frag r [NOT]
            | SNeg :: r -> frag r changeSign
            | SPow2 :: r -> frag r [POW2]

            | SSetSp :: r -> frag r [SET_SP]
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

            | SLabel i :: SSpacer (ln, e) :: r ->
                rest <- r
                [Label i; Spacer (i, expressionConst ln e)]

            // Avoid optimizing away tight infinite loops.
            | SLabel i :: SPush (ELabel j) :: SJump :: r when i = j ->
                [Label i] @ frag r [PUSH0; JUMP_ZERO; -3y]

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

            | SNewFrame  :: r -> frag r [NEW_FRAME]
            | SSetPixel  :: r -> frag r [SET_PIXEL]
            | SAddSample :: r -> frag r [ADD_SAMPLE]
            | SPutChar   :: r -> frag r [PUT_CHAR]

            | SReadFrame :: r -> frag r [READ_FRAME]
            | SReadPixel :: r -> frag r [READ_PIXEL]

            | _ -> failwithf "Impossible case: %O" rest
    }
