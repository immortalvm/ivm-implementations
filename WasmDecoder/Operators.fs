module Operators

open Basics
open Ast


module I32Op = IntOp
module I64Op = IntOp
module F32Op = FloatOp
module F64Op = FloatOp


let i32Const n = Const (I32 n.it @@ n.at)
let i64Const n = Const (I64 n.it @@ n.at)
let f32Const n = Const (F32 n.it @@ n.at)
let f64Const n = Const (F64 n.it @@ n.at)

let unreachable = Unreachable
let nop = Nop
let drop = Drop
let select = Select
let block ts es = Block (ts, es)
let loop ts es = Loop (ts, es)
let br x = Br x
let brIf x = BrIf x
let brTable xs x = BrTable (xs, x)
let if1 ts es1 es2 = If (ts, es1, es2)

let return1 = Return
let call x = Call x
let callIndirect x = CallIndirect x

let localGet x = LocalGet x
let localSet x = LocalSet x
let localTee x = LocalTee x
let globalGet x = GlobalGet x
let globalSet x = GlobalSet x

let i32load align offset = Load {ty = I32Type; align=align; offset=offset; sz = None}
let i64load align offset = Load {ty = I64Type; align=align; offset=offset; sz = None}
let f32load align offset = Load {ty = F32Type; align=align; offset=offset; sz = None}
let f64load align offset = Load {ty = F64Type; align=align; offset=offset; sz = None}
let i32load8s align offset =
  Load {ty = I32Type; align=align; offset=offset; sz = Some (Pack8, SX)}
let i32load8u align offset =
  Load {ty = I32Type; align=align; offset=offset; sz = Some (Pack8, ZX)}
let i32load16s align offset =
  Load {ty = I32Type; align=align; offset=offset; sz = Some (Pack16, SX)}
let i32load16u align offset =
  Load {ty = I32Type; align=align; offset=offset; sz = Some (Pack16, ZX)}
let i64load8s align offset =
  Load {ty = I64Type; align=align; offset=offset; sz = Some (Pack8, SX)}
let i64load8u align offset =
  Load {ty = I64Type; align=align; offset=offset; sz = Some (Pack8, ZX)}
let i64load16s align offset =
  Load {ty = I64Type; align=align; offset=offset; sz = Some (Pack16, SX)}
let i64load16u align offset =
  Load {ty = I64Type; align=align; offset=offset; sz = Some (Pack16, ZX)}
let i64load32s align offset =
  Load {ty = I64Type; align=align; offset=offset; sz = Some (Pack32, SX)}
let i64load32u align offset =
  Load {ty = I64Type; align=align; offset=offset; sz = Some (Pack32, ZX)}

let i32Store align offset = Store {ty = I32Type; align=align; offset=offset; sz = None}
let i64Store align offset = Store {ty = I64Type; align=align; offset=offset; sz = None}
let f32Store align offset = Store {ty = F32Type; align=align; offset=offset; sz = None}
let f64Store align offset = Store {ty = F64Type; align=align; offset=offset; sz = None}
let i32Store8 align offset =
  Store {ty = I32Type; align=align; offset=offset; sz = Some Pack8}
let i32Store16 align offset =
  Store {ty = I32Type; align=align; offset=offset; sz = Some Pack16}
let i64Store8 align offset =
  Store {ty = I64Type; align=align; offset=offset; sz = Some Pack8}
let i64Store16 align offset =
  Store {ty = I64Type; align=align; offset=offset; sz = Some Pack16}
let i64Store32 align offset =
  Store {ty = I64Type; align=align; offset=offset; sz = Some Pack32}

let i32Clz = Unary (I32 I32Op.Clz)
let i32Ctz = Unary (I32 I32Op.Ctz)
let i32Popcnt = Unary (I32 I32Op.Popcnt)
let i64Clz = Unary (I64 I64Op.Clz)
let i64Ctz = Unary (I64 I64Op.Ctz)
let i64Popcnt = Unary (I64 I64Op.Popcnt)
let f32Neg = Unary (F32 F32Op.Neg)
let f32Abs = Unary (F32 F32Op.Abs)
let f32Sqrt = Unary (F32 F32Op.Sqrt)
let f32Ceil = Unary (F32 F32Op.Ceil)
let f32Floor = Unary (F32 F32Op.Floor)
let f32Trunc = Unary (F32 F32Op.Trunc)
let f32Nearest = Unary (F32 F32Op.Nearest)
let f64Neg = Unary (F64 F64Op.Neg)
let f64Abs = Unary (F64 F64Op.Abs)
let f64Sqrt = Unary (F64 F64Op.Sqrt)
let f64Ceil = Unary (F64 F64Op.Ceil)
let f64Floor = Unary (F64 F64Op.Floor)
let f64Trunc = Unary (F64 F64Op.Trunc)
let f64Nearest = Unary (F64 F64Op.Nearest)

let i32Add = Binary (I32 I32Op.Add)
let i32Sub = Binary (I32 I32Op.Sub)
let i32Mul = Binary (I32 I32Op.Mul)
let i32Divs = Binary (I32 I32Op.DivS)
let i32Divu = Binary (I32 I32Op.DivU)
let i32Rems = Binary (I32 I32Op.RemS)
let i32Remu = Binary (I32 I32Op.RemU)
let i32And = Binary (I32 I32Op.And)
let i32Or = Binary (I32 I32Op.Or)
let i32Xor = Binary (I32 I32Op.Xor)
let i32Shl = Binary (I32 I32Op.Shl)
let i32Shrs = Binary (I32 I32Op.ShrS)
let i32Shru = Binary (I32 I32Op.ShrU)
let i32Rotl = Binary (I32 I32Op.Rotl)
let i32Rotr = Binary (I32 I32Op.Rotr)
let i64Add = Binary (I64 I64Op.Add)
let i64Sub = Binary (I64 I64Op.Sub)
let i64Mul = Binary (I64 I64Op.Mul)
let i64Divs = Binary (I64 I64Op.DivS)
let i64Divu = Binary (I64 I64Op.DivU)
let i64Rems = Binary (I64 I64Op.RemS)
let i64Remu = Binary (I64 I64Op.RemU)
let i64And = Binary (I64 I64Op.And)
let i64Or = Binary (I64 I64Op.Or)
let i64Xor = Binary (I64 I64Op.Xor)
let i64Shl = Binary (I64 I64Op.Shl)
let i64Shrs = Binary (I64 I64Op.ShrS)
let i64Shru = Binary (I64 I64Op.ShrU)
let i64Rotl = Binary (I64 I64Op.Rotl)
let i64Rotr = Binary (I64 I64Op.Rotr)
let f32Add = Binary (F32 F32Op.Add)
let f32Sub = Binary (F32 F32Op.Sub)
let f32Mul = Binary (F32 F32Op.Mul)
let f32Div = Binary (F32 F32Op.Div)
let f32Min = Binary (F32 F32Op.Min)
let f32Max = Binary (F32 F32Op.Max)
let f32Copysign = Binary (F32 F32Op.CopySign)
let f64Add = Binary (F64 F64Op.Add)
let f64Sub = Binary (F64 F64Op.Sub)
let f64Mul = Binary (F64 F64Op.Mul)
let f64Div = Binary (F64 F64Op.Div)
let f64Min = Binary (F64 F64Op.Min)
let f64Max = Binary (F64 F64Op.Max)
let f64Copysign = Binary (F64 F64Op.CopySign)

let i32Eqz = Test (I32 I32Op.Eqz)
let i64Eqz = Test (I64 I64Op.Eqz)

let i32Eq = Compare (I32 I32Op.Eq)
let i32Ne = Compare (I32 I32Op.Ne)
let i32Lts = Compare (I32 I32Op.LtS)
let i32Ltu = Compare (I32 I32Op.LtU)
let i32Les = Compare (I32 I32Op.LeS)
let i32Leu = Compare (I32 I32Op.LeU)
let i32Gts = Compare (I32 I32Op.GtS)
let i32Gtu = Compare (I32 I32Op.GtU)
let i32Ges = Compare (I32 I32Op.GeS)
let i32Geu = Compare (I32 I32Op.GeU)
let i64Eq = Compare (I64 I64Op.Eq)
let i64Ne = Compare (I64 I64Op.Ne)
let i64Lts = Compare (I64 I64Op.LtS)
let i64Ltu = Compare (I64 I64Op.LtU)
let i64Les = Compare (I64 I64Op.LeS)
let i64Leu = Compare (I64 I64Op.LeU)
let i64Gts = Compare (I64 I64Op.GtS)
let i64Gtu = Compare (I64 I64Op.GtU)
let i64Ges = Compare (I64 I64Op.GeS)
let i64Geu = Compare (I64 I64Op.GeU)
let f32Eq = Compare (F32 F32Op.Eq)
let f32Ne = Compare (F32 F32Op.Ne)
let f32Lt = Compare (F32 F32Op.Lt)
let f32Le = Compare (F32 F32Op.Le)
let f32Gt = Compare (F32 F32Op.Gt)
let f32Ge = Compare (F32 F32Op.Ge)
let f64Eq = Compare (F64 F64Op.Eq)
let f64Ne = Compare (F64 F64Op.Ne)
let f64Lt = Compare (F64 F64Op.Lt)
let f64Le = Compare (F64 F64Op.Le)
let f64Gt = Compare (F64 F64Op.Gt)
let f64Ge = Compare (F64 F64Op.Ge)

let i32WrapI64 = Convert (I32 I32Op.WrapI64)
let i32TruncF32s = Convert (I32 I32Op.TruncSF32)
let i32TruncF32u = Convert (I32 I32Op.TruncUF32)
let i32TruncF64s = Convert (I32 I32Op.TruncSF64)
let i32TruncF64u = Convert (I32 I32Op.TruncUF64)
let i64ExtendI32s = Convert (I64 I64Op.ExtendSI32)
let i64ExtendI32u = Convert (I64 I64Op.ExtendUI32)
let i64TruncF32s = Convert (I64 I64Op.TruncSF32)
let i64TruncF32u = Convert (I64 I64Op.TruncUF32)
let i64TruncF64s = Convert (I64 I64Op.TruncSF64)
let i64TruncF64u = Convert (I64 I64Op.TruncUF64)
let f32ConvertI32s = Convert (F32 F32Op.ConvertSI32)
let f32ConvertI32u = Convert (F32 F32Op.ConvertUI32)
let f32ConvertI64s = Convert (F32 F32Op.ConvertSI64)
let f32ConvertI64u = Convert (F32 F32Op.ConvertUI64)
let f32DemoteF64 = Convert (F32 F32Op.DemoteF64)
let f64ConvertI32s = Convert (F64 F64Op.ConvertSI32)
let f64ConvertI32u = Convert (F64 F64Op.ConvertUI32)
let f64ConvertI64s = Convert (F64 F64Op.ConvertSI64)
let f64ConvertI64u = Convert (F64 F64Op.ConvertUI64)
let f64PromoteF32 = Convert (F64 F64Op.PromoteF32)
let i32ReinterpretF32 = Convert (I32 I32Op.ReinterpretFloat)
let i64ReinterpretF64 = Convert (I64 I64Op.ReinterpretFloat)
let f32ReinterpretI32 = Convert (F32 F32Op.ReinterpretInt)
let f64ReinterpretI64 = Convert (F64 F64Op.ReinterpretInt)

let memorySize = MemorySize
let memoryGrow = MemoryGrow
