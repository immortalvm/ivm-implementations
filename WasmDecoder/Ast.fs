module Ast

(*
 * Throughout the implementation we use consistent naming conventions for
 * syntactic elements, associated with the types defined here and in a few
 * other places:
 *
 *   x : var
 *   v : value
 *   e : instrr
 *   f : func
 *   m : module_
 *
 *   t : value_type
 *   s : func_type
 *   c : context / config
 *
 * These conventions mostly follow standard practice in language semantics.
 *)

open Types


(* Operators *)

module IntOp =
  type unop = Clz | Ctz | Popcnt
  type binop = Add | Sub | Mul | DivS | DivU | RemS | RemU
             | And | Or | Xor | Shl | ShrS | ShrU | Rotl | Rotr
  type testop = Eqz
  type relop = Eq | Ne | LtS | LtU | GtS | GtU | LeS | LeU | GeS | GeU
  type cvtop = ExtendSI32 | ExtendUI32 | WrapI64
             | TruncSF32 | TruncUF32 | TruncSF64 | TruncUF64
             | ReinterpretFloat

module FloatOp =
  type unop = Neg | Abs | Ceil | Floor | Trunc | Nearest | Sqrt
  type binop = Add | Sub | Mul | Div | Min | Max | CopySign
  type testop = unit (* Does this make sense? *)
  type relop = Eq | Ne | Lt | Gt | Le | Ge
  type cvtop = ConvertSI32 | ConvertUI32 | ConvertSI64 | ConvertUI64
             | PromoteF32 | DemoteF64
             | ReinterpretInt

module I32Op = IntOp
module I64Op = IntOp
module F32Op = FloatOp
module F64Op = FloatOp

type unop = Values.op<I32Op.unop, I64Op.unop, F32Op.unop, F64Op.unop>
type binop = Values.op<I32Op.binop, I64Op.binop, F32Op.binop, F64Op.binop>
type testop = Values.op<I32Op.testop, I64Op.testop, F32Op.testop, F64Op.testop>
type relop = Values.op<I32Op.relop, I64Op.relop, F32Op.relop, F64Op.relop>
type cvtop = Values.op<I32Op.cvtop, I64Op.cvtop, F32Op.cvtop, F64Op.cvtop>

type 'a memop =
  {ty : value_type; align : int; offset : Memory.offset; sz : 'a option}
type loadop = (Memory.pack_size * Memory.extension) memop
type storeop = Memory.pack_size memop


(* Expressions *)

type var = uint32 Source.phrase
type literal = Values.value Source.phrase

type instr = instr' Source.phrase
and instr' =
  | Unreachable                       (* trap unconditionally *)
  | Nop                               (* do nothing *)
  | Drop                              (* forget a value *)
  | Select                            (* branchless conditional *)
  | Block of stack_type * instr list  (* execute in sequence *)
  | Loop of stack_type * instr list   (* loop header *)
  | If of stack_type * instr list * instr list  (* conditional *)
  | Br of var                         (* break to n-th surrounding label *)
  | BrIf of var                       (* conditional break *)
  | BrTable of var list * var         (* indexed break *)
  | Return                            (* break from function body *)
  | Call of var                       (* call function *)
  | CallIndirect of var               (* call function through table *)
  | LocalGet of var                   (* read local variable *)
  | LocalSet of var                   (* write local variable *)
  | LocalTee of var                   (* write local variable and keep value *)
  | GlobalGet of var                  (* read global variable *)
  | GlobalSet of var                  (* write global variable *)
  | Load of loadop                    (* read memory at address *)
  | Store of storeop                  (* write memory at address *)
  | MemorySize                        (* size of linear memory *)
  | MemoryGrow                        (* grow linear memory *)
  | Const of literal                  (* constant *)
  | Test of testop                    (* numeric test *)
  | Compare of relop                  (* numeric comparison *)
  | Unary of unop                     (* unary numeric operator *)
  | Binary of binop                   (* binary numeric operator *)
  | Convert of cvtop                  (* conversion *)


(* Globals & Functions *)

type constant = instr list Source.phrase

type global' = {
  gtype : global_type;
  value : constant;
}
type global1 = global' Source.phrase

type func' = {
  ftype : var;
  locals : value_type list;
  body : instr list;
}
type func = func' Source.phrase


(* Tables & Memories *)

type table' = {
  ttype : table_type;
}
type table = table' Source.phrase

type memory' = {
  mtype : memory_type;
}
type memory = memory' Source.phrase

type 'data segment' = {
  index : var;
  offset : constant;
  init : 'data;
}
type 'data segment = 'data segment' Source.phrase

type table_segment = var list segment
type memory_segment = string segment


(* Modules *)

type type_ = func_type Source.phrase

type export_desc = export_desc' Source.phrase
and export_desc' =
  | FuncExport of var
  | TableExport of var
  | MemoryExport of var
  | GlobalExport of var

type export' = {
  name : string;
  edesc : export_desc;
}
type export = export' Source.phrase

type import_desc' =
  | FuncImport of var
  | TableImport of table_type
  | MemoryImport of memory_type
  | GlobalImport of global_type
type import_desc = import_desc' Source.phrase

type import' = {
  module_name : string;
  item_name : string;
  idesc : import_desc;
}
type import = import' Source.phrase

type module_' = {
  types : type_ list;
  globals : global1 list;
  tables : table list;
  memories : memory list;
  funcs : func list;
  start : var option;
  elems : var list segment list;
  data : (byte array) segment list;
  imports : import list;
  exports : export list;
}
type module_ = module_' Source.phrase


(* Auxiliary functions *)

let empty_module = {
  types = [];
  globals = [];
  tables = [];
  memories = [];
  funcs = [];
  start = None;
  elems  = [];
  data = [];
  imports = [];
  exports = [];
}
