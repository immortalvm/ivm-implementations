module Assembler.Composition

open Assembler.Ast

val assemble : seq<Statement> -> uint8 list * int[] * ((int * uint64) list)

val spacerAllocations : seq<int * uint64> -> uint8 list
