module Machine.Executor

exception AccessException of string
exception UndefinedException of string

// Execute at random location (multiple of 1000) and return terminal stack.
// This method is likely to change...
val execute : seq<uint8> -> seq<uint64>

val trace : seq<uint8> -> seq<uint64>
