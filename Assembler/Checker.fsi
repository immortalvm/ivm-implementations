module Assembler.Checker

val showValue : int64 -> string

val getDependencies : string -> Set<string>

val doAssemble : string -> uint8 list * (string * int) list * (string * int) list

val doRun : seq<uint8> -> Map<int, string> option -> seq<int64>

// Returns message if stack as expected, otherwise raises exception.
val doCheck : string -> string
