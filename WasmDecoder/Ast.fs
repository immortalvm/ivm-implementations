module Ast

(*
 * Throughout the implementation we use consistent naming conventions for
 * syntactic elements, associated with the types defined here and in a few
 * other places:
 *
 *   x : var
 *   v : Value
 *   e : instrr
 *   f : func
 *   m : module_
 *
 *   t : ValueType
 *   s : FuncType
 *   c : context / config
 *
 * These conventions mostly follow standard practice in language semantics.
 *)

open Types


(* Operators *)

module IntOp =
  type Unop = Clz | Ctz | Popcnt
  type Binop = Add | Sub | Mul | DivS | DivU | RemS | RemU
             | And | Or | Xor | Shl | ShrS | ShrU | Rotl | Rotr
  type Testop = Eqz
  type Relop = Eq | Ne | LtS | LtU | GtS | GtU | LeS | LeU | GeS | GeU
  type Cvtop = ExtendSI32 | ExtendUI32 | WrapI64
             | TruncSF32 | TruncUF32 | TruncSF64 | TruncUF64
             | ReinterpretFloat

module FloatOp =
  type Unop = Neg | Abs | Ceil | Floor | Trunc | Nearest | Sqrt
  type Binop = Add | Sub | Mul | Div | Min | Max | CopySign
  type Testop = unit (* Does this make sense? *)
  type Relop = Eq | Ne | Lt | Gt | Le | Ge
  type Cvtop = ConvertSI32 | ConvertUI32 | ConvertSI64 | ConvertUI64
             | PromoteF32 | DemoteF64
             | ReinterpretInt

module I32Op = IntOp
module I64Op = IntOp
module F32Op = FloatOp
module F64Op = FloatOp

type Unop = Values.Op<I32Op.Unop, I64Op.Unop, F32Op.Unop, F64Op.Unop>
type Binop = Values.Op<I32Op.Binop, I64Op.Binop, F32Op.Binop, F64Op.Binop>
type Testop = Values.Op<I32Op.Testop, I64Op.Testop, F32Op.Testop, F64Op.Testop>
type Relop = Values.Op<I32Op.Relop, I64Op.Relop, F32Op.Relop, F64Op.Relop>
type Cvtop = Values.Op<I32Op.Cvtop, I64Op.Cvtop, F32Op.Cvtop, F64Op.Cvtop>

type 'a Memop =
  {ty : ValueType; align : int; offset : Memory.Offset; sz : 'a option}
type Loadop = (Memory.PackSize * Memory.Extension) Memop
type Storeop = Memory.PackSize Memop


(* Expressions *)

type Var = uint32 Source.Phrase
type Literal = Values.Value Source.Phrase

type Instr = Instr' Source.Phrase
and Instr' =
  | Unreachable                       (* trap unconditionally *)
  | Nop                               (* do nothing *)
  | Drop                              (* forget a value *)
  | Select                            (* branchless conditional *)
  | Block of StackType * Instr list  (* execute in sequence *)
  | Loop of StackType * Instr list   (* loop header *)
  | If of StackType * Instr list * Instr list  (* conditional *)
  | Br of Var                         (* break to n-th surrounding label *)
  | BrIf of Var                       (* conditional break *)
  | BrTable of Var list * Var         (* indexed break *)
  | Return                            (* break from function body *)
  | Call of Var                       (* call function *)
  | CallIndirect of Var               (* call function through table *)
  | LocalGet of Var                   (* read local variable *)
  | LocalSet of Var                   (* write local variable *)
  | LocalTee of Var                   (* write local variable and keep value *)
  | GlobalGet of Var                  (* read global variable *)
  | GlobalSet of Var                  (* write global variable *)
  | Load of Loadop                    (* read memory at address *)
  | Store of Storeop                  (* write memory at address *)
  | MemorySize                        (* size of linear memory *)
  | MemoryGrow                        (* grow linear memory *)
  | Const of Literal                  (* constant *)
  | Test of Testop                    (* numeric test *)
  | Compare of Relop                  (* numeric comparison *)
  | Unary of Unop                     (* unary numeric operator *)
  | Binary of Binop                   (* binary numeric operator *)
  | Convert of Cvtop                  (* conversion *)


(* Globals & Functions *)

type Constant = Instr list Source.Phrase

type Global1' = {
  gtype : GlobalType;
  value : Constant;
}
type Global1 = Global1' Source.Phrase

type Func' = {
  ftype : Var;
  locals : ValueType list;
  body : Instr list;
}
type Func = Func' Source.Phrase


(* Tables & Memories *)

type Table' = {
  ttype : TableType;
}
type Table = Table' Source.Phrase

type Mem' = {
  mtype : MemoryType;
}
type Mem = Mem' Source.Phrase

type 'data Segment' = {
  index : Var;
  offset : Constant;
  init : 'data;
}
type 'data Segment = 'data Segment' Source.Phrase


(* Modules *)

type Type = FuncType Source.Phrase

type ExportDesc = ExportDesc' Source.Phrase
and ExportDesc' =
  | FuncExport of Var
  | TableExport of Var
  | MemoryExport of Var
  | GlobalExport of Var

type Export' = {
  name : string;
  edesc : ExportDesc;
}
type Export = Export' Source.Phrase

type ImportDesc' =
  | FuncImport of Var
  | TableImport of TableType
  | MemoryImport of MemoryType
  | GlobalImport of GlobalType
type ImportDesc = ImportDesc' Source.Phrase

type Import' = {
  moduleName : string;
  itemName : string;
  idesc : ImportDesc;
}
type Import = Import' Source.Phrase

type ModuleType = {
  types : Type list;
  globals : Global1 list;
  tables : Table list;
  memories : Mem list;
  funcs : Func list;
  start : Var option;
  elems : Var list Segment list;
  data : (byte array) Segment list;
  imports : Import list;
  exports : Export list;
}
