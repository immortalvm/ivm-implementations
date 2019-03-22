module Basics

(* Types *)

type ValueType = I32Type | I64Type | F32Type | F64Type
type ElemType = FuncRefType
type StackType = ValueType list
type FuncType = FuncType of StackType * StackType

type 'a Limits = {min : 'a; max : 'a option}
type Mutability = Immutable | Mutable
type TableType = TableType of uint32 Limits * ElemType
type MemoryType = MemoryType of uint32 Limits
type GlobalType = GlobalType of ValueType * Mutability

let size = function
  | I32Type | F32Type -> 4
  | I64Type | F64Type -> 8


(* Source *)

type 'a Phrase = {at : int; it : 'a}
let (@@) x position = {it = x; at = position}
let noRegion = 0


(* Values *)

type ('i32, 'i64, 'f32, 'f64) Op =
  I32 of 'i32 | I64 of 'i64 | F32 of 'f32 | F64 of 'f64
type Value = Op<int, int64, float32, float>


(* Memory *)

type Offset = uint32
type PackSize = Pack8 | Pack16 | Pack32
type Extension = SX | ZX
