module Assembler.Target

open Assembler.Ast

val opLen : int8 list -> int

val nopsFor : int -> int8 list

type Intermediate =
    | Label of int
    | Relative // Relativize the following 64-bit number
    | Spacer of int * ((int -> int) -> int64)
    // Code given label -> relative position (from statement _start_)
    | Fragment of ((int -> int) -> int8 list)
    | Export of ((int -> int) -> int * bool * int64) // (label, relative?, value)

val intermediates : Statement list -> seq<Intermediate>
