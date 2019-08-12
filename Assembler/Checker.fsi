module Assembler.Checker

type assemblerOutput = string * uint8 list * (string * int) list * (string * int) list

val showValue : int64 -> string

val SOURCE_EXTENSION : string
val BINARY_EXTENSION : string
val SYMBOLS_EXTENSION : string

val nodePath : string -> string -> string -> string

val getBuildOrder : string -> string -> seq<string>

val doAssemble : string -> assemblerOutput

val doRun : seq<uint8> -> seq<uint8> -> Map<int, string> option -> seq<int64>

// Returns message if stack as expected, otherwise raises exception.
val doCheck : string -> string

val doBuild : string -> seq<string> -> seq<assemblerOutput>

val doCollect : seq<assemblerOutput> -> assemblerOutput
