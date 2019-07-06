module Assembler.Ast

type Expression =
    | ENum of int64
    | ELabel of int
    | EStack of Expression  // &n = SP + n * 8

    | ESum of Expression list
    | EProd of Expression list
    | EMinus of Expression

    | EConj of Expression list
    | EDisj of Expression list
    | EXor of Expression list
    | ENeg of Expression
    | EShift of Expression * Expression

    // Return -1 for true and 0 for false:
    | ELt of Expression * Expression
    | ELtE of Expression * Expression
    | EEq of Expression * Expression
    | EGtE of Expression * Expression
    | EGt of Expression * Expression

    | ELoad1 of Expression
    | ELoad2 of Expression
    | ELoad4 of Expression
    | ELoad8 of Expression

    | ESign1 of Expression
    | ESign2 of Expression
    | ESign4 of Expression

type MaxOneArgument = Expression option
type MaxTwoArguments = (Expression * Expression option) option

type Statement =
    | SLabel of int
    | SData of uint8 list

    | SPush of Expression
    | SExit | SSetSp
    // [SPush label; SJump None] -> [SJump (Some label)]
    | SJump of int option
    | SJumpZero of int option
    | SJumpNotZero of int option

    | SLoad1 | SLoad2 | SLoad4 | SLoad8
    | SSign1 | SSign2 | SSign4
    | SStore1 | SStore2 | SStore4 | SStore8

    | SAdd | SMult
    | SMinus
    | SAnd | SOr | SXor | SNeg
    | SShift // Second argument (top) signed.
    | SShiftS // Both arguments signed.

    | SDivU | SDivS
    | SRemU | SRemS

    // Return -1 for true and 0 for false:
    | SLtU | SLtS | SLtEU | SLtES
    | SEq
    | SGtEU | SGtES | SGtU | SGtS

    | SAlloc | SDealloc

    // TODO: Input and output
