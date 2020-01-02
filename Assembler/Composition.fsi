module Assembler.Composition

open Assembler.Ast

val assemble : Statement list -> uint8 list * int[] * ((int * int64) list) * (int list) * (bool * int64)[]

val initialization : int -> (int * int64) list -> int list -> uint8 list
