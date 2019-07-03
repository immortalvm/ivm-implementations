module Assembler.Ast

type Expression =
    | ENum of int64
    | ELabel of string
    | EPc  // $pc
    | EStack of int64  // &n - SP + x * 8
    | EPeek of int64  // $n - Value at (SP + n * 8)

    | ESum of Expression list
    | EProd of Expression list
    | EMinus of Expression

    | EConj of Expression list
    | EDisj of Expression list
    | ENeg of Expression

    | ELoad1 of Expression
    | ELoad2 of Expression
    | ELoad4 of Expression
    | ELoad8 of Expression

    | ESign1 of Expression
    | ESign2 of Expression
    | ESign4 of Expression

type Statement =
    | SLabel of string
    | SMacroE of string * Expression
    | SMacroS of string * Statement list
    | SPush of Expression
    | SReturn // Set PC to value popped.
    | SJump of Expression
    | SJumpIfZero of Expression
    | SJumpIfNotZero of Expression
    | SSetSp
    | SLoad1 | SLoad2 | SLoad4 | SLoad8
    | SSign1 | SSign2 | SSign4
    | SStore1 | SStore2 | SStore4 | SStore8
