module Assembler.Target

open Assembler.Ast

val opLen : int8 list -> int

val nopsFor : int -> int8 list

// Code given label -> relative position (from statement _start_)
type FlexCode = (int -> int) -> int8 list

type Intermediate =
    | Label of int
    | Spacer of int * ((int -> int) -> int64)
    | Fragment of FlexCode

val intermediates : Statement list -> seq<Intermediate>
