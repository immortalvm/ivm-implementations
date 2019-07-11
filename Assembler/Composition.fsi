module Assembler.Composition

open Assembler.Ast

val assemble : seq<Statement> -> uint8 list * int[]
