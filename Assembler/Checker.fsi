module Assembler.Checker

type AssemblerOutput = {
    Node: string;
    Binary: seq<uint8>;
    Exported: seq<string * int>;
    Labels: seq<string * int>;
    Spacers: seq<int * uint64>;
}

val showValue : int64 -> string

val SOURCE_EXTENSION : string
val BINARY_EXTENSION : string
val SYMBOLS_EXTENSION : string

val nodePath : string -> string -> string -> string

val getBuildOrder : string -> string -> seq<string>

val doAssemble : string -> AssemblerOutput

val doRun : seq<uint8> -> seq<uint8> -> string option -> Map<int, string> option -> seq<int64>

// Returns message if stack as expected, otherwise raises exception.
val doCheck : string -> string

val doBuild : string -> seq<AssemblerOutput> -> seq<string> -> seq<AssemblerOutput>

val doCollect : int -> seq<AssemblerOutput> -> AssemblerOutput
