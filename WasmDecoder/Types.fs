module Types

type ValueType = I32Type | I64Type | F32Type | F64Type
type ElemType = FuncRefType
type StackType = ValueType list
type FuncType = FuncType of StackType * StackType

type 'a Limits = {min : 'a; max : 'a option}
type Mutability = Immutable | Mutable
type TableType = TableType of uint32 Limits * ElemType
type MemoryType = MemoryType of uint32 Limits
type GlobalType = GlobalType of ValueType * Mutability


(* Attributes *)

let size = function
  | I32Type | F32Type -> 4
  | I64Type | F64Type -> 8
