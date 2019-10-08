module Assembler.Composition

open Assembler.Ast

val assemble : seq<Statement> -> uint8 list * int[] * ((int * uint64) list) * (int list)

val initialization : int -> (int * uint64) list -> int list -> uint8 list
