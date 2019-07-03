module Assembler.Ast

type Expression =
    | ENum of int64
    | ELabel of string
    | EPc  // $pc
    | EStack of Expression  // &n = SP + n * 8
    | EPeek of Expression  // $n = Value at (SP + n * 8)

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
    | SLabel of string
    | SDef of string * Expression
    | SData of uint8 list

    | SExit
    | SPush of Expression list
    | SSetSp of MaxOneArgument
    | SJump of MaxOneArgument
    | SJumpZero of MaxTwoArguments
    | SJumpNotZero of MaxTwoArguments

    | SLoad1 of MaxOneArgument
    | SLoad2 of MaxOneArgument
    | SLoad4 of MaxOneArgument
    | SLoad8 of MaxOneArgument
    | SSign1 of MaxOneArgument
    | SSign2 of MaxOneArgument
    | SSign4 of MaxOneArgument
    | SStore1 of MaxTwoArguments
    | SStore2 of MaxTwoArguments
    | SStore4 of MaxTwoArguments
    | SStore8 of MaxTwoArguments

    | SAdd of MaxTwoArguments
    | SMult of MaxTwoArguments
    | SMinus of MaxOneArgument
    | SAnd of MaxTwoArguments
    | SOr of MaxTwoArguments
    | SXor of MaxTwoArguments
    | SNeg of MaxOneArgument
    | SShift of MaxTwoArguments // Second argument (top) signed.
    | SShiftS of MaxTwoArguments // Both arguments signed.

    | SDivU of MaxTwoArguments
    | SDivS of MaxTwoArguments
    | SRemU of MaxTwoArguments
    | SRemS of MaxTwoArguments

    // Return -1 for true and 0 for false:
    | SLt of MaxTwoArguments
    | SLtE of MaxTwoArguments
    | SEq of MaxTwoArguments
    | SGtE of MaxTwoArguments
    | SGt of MaxTwoArguments

    | SAlloc of MaxOneArgument
    | SDealloc of MaxOneArgument

    // TODO: Input and output
