module Values

open Types


(* Values and operators *)

type ('i32, 'i64, 'f32, 'f64) Op =
  I32 of 'i32 | I64 of 'i64 | F32 of 'f32 | F64 of 'f64

type Value = Op<int, int64, float32, float>
