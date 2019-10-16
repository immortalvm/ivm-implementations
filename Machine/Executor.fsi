module Machine.Executor

exception AccessException of string
exception UndefinedException of string

// Execute at random location (multiple of 1000) and return terminal stack.
// Trace execution if symbol mapping is provided.
// This method is likely to change...
val execute : uint64 -> seq<uint8> -> seq<uint8> -> string option -> Map<int, string> option -> seq<uint64>
