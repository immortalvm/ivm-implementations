module Assembler.Composition

open Assembler.Ast

val assemble : seq<Statement> -> uint8 list * int[] * ((int * uint64) list)

val initialization : int -> (int * uint64) list -> uint8 list
