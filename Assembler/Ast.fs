module Assembler.Ast

exception ParseException of string

type Expression =
    | ENum of int64
    | ELabel of int
    | EOffset of int * Expression
    | EStack of Expression // &n = SP + n * 8

    | ESum of Expression list
    | EProd of Expression list
    | ENeg of Expression
    | EPow2 of Expression

    | EConj of Expression list
    | EDisj of Expression list
    | EXor of Expression list
    | ENot of Expression

    | EDivU of Expression * Expression
    | EDivS of Expression * Expression
    | EDivSU of Expression * Expression // Divide signed with unsigned (e.g. 2^63)
    | ERemU of Expression * Expression
    | ERemS of Expression * Expression

    // Return -1 for true and 0 for false:
    | ELtU of Expression * Expression
    | ELtS of Expression * Expression
    | ELtEU of Expression * Expression
    | ELtES of Expression * Expression
    | EEq of Expression * Expression
    | EGtEU of Expression * Expression
    | EGtES of Expression * Expression
    | EGtU of Expression * Expression
    | EGtS of Expression * Expression

    | ELoad1 of Expression
    | ELoad2 of Expression
    | ELoad4 of Expression
    | ELoad8 of Expression
    | ESigx1 of Expression
    | ESigx2 of Expression
    | ESigx4 of Expression

type MaxOneArgument = Expression option
type MaxTwoArguments = (Expression * Expression option) option

type Statement =
    | SBoundary // Dummy statement for separating statements (block optimizations)
    | SLabel of int

    // Include line number to be used in error message if the expression is not an "assembly time" constant
    | SData1 of int64 * Expression
    | SData2 of int64 * Expression
    | SData4 of int64 * Expression
    | SData8 of int64 * Expression
    | SSpacer of int64 * Expression
    | SExport of int64 * int * Expression

    | SPush of Expression
    | SExit | SSetSp
    | SCall of int
    | SJump | SJumpZero | SJumpNotZero

    | SLoad1 | SLoad2 | SLoad4 | SLoad8
    | SSigx1 | SSigx2 | SSigx4
    | SStore1 | SStore2 | SStore4 | SStore8

    | SAdd | SMult
    | SNeg
    | SAnd | SOr | SXor | SNot
    | SPow2
    | SDivU | SDivS | SDivSU
    | SRemU | SRemS

    // Return -1 for true and 0 for false:
    | SLtU | SLtS | SLtEU | SLtES
    | SEq
    | SGtEU | SGtES | SGtU | SGtS

    | SNewFrame | SSetPixel | SAddSample
    | SPutChar | SPutByte

    | SReadFrame | SReadPixel
