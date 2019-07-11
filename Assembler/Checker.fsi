module Assembler.Checker

val showValue : int64 -> string

val doAssemble : string -> uint8 list

val doRun : seq<uint8> -> bool -> seq<int64>

// Returns message if stack as expected, otherwise raises exception.
val doCheck : string -> string
