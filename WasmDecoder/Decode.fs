module Decode

open Basics
open Ast
open Operators


(* From Lib *)

module Fun =
  let uncurry f (x, y) = f x y


(* Are these functions inteded for lists of signed or unsigned integers? *)
module List32 =
  let rec make n x = make' n x []
  and make' (n: uint32) x xs =
    if n = 0ul then xs else make' (n - 1ul) x (x::xs)


module Option =
  let map f = function
    | Some x -> Some (f x)
    | None -> None



(* Decoding stream *)

type Stream = {
  bytes : byte array;
  pos : int ref;
}

exception EosException

let stream bs = {bytes = bs; pos = ref 0}

let len s : int = Array.length s.bytes
let pos s = !(s.pos)
let eos s = (pos s = len s)

let check n s = if pos s + n > len s then raise EosException
let skip0 n s = if n < 0 then raise EosException else check n s; s.pos := !(s.pos) + n

let read s : byte = s.bytes.[!(s.pos)]
let peek s : byte option = if eos s then None else Some (read s)
let get0 s : byte = check 1 s; let b = read s in skip0 1 s; b
let getString0 n s : byte array = let i = pos s in skip0 n s; s.bytes.[i..i+n-1]


(* Errors *)

exception CodeException of int * string

let stringOfByte b = Printf.sprintf "%02x" b

let error s pos msg = raise (CodeException (pos, msg))
let require b s pos msg = if not b then error s pos msg

let guard f s =
  try f s with EosException -> error s (len s) "unexpected end of section or function"

let get = guard get0
let getString n = guard (getString0 n)
let skip n = guard (skip0 n)

let expect b s msg = require (guard get s = b) s (pos s - 1) msg
let illegal s pos b = error s pos ("illegal opcode " + stringOfByte b)


let at (f: Stream -> 'a) (s: Stream) : 'a Phrase =
  let left = pos s in
  let x = f s in
  x @@ left



(* Generic values *)

let u8 s : int =
  get s |> int

let u16 s : int =
  let lo = u8 s in
  let hi = u8 s  in
  (hi <<< 8) ||| lo

let u32 s : uint32 =
  let lo = u16 s |> uint32 in
  let hi = u16 s |> uint32 in
  (hi <<< 16) ||| lo

let u64 s : uint64 =
  let lo = u32 s |> uint64 in
  let hi = u32 s |> uint64 in
  (hi <<< 32) ||| lo

(* LEB128. Unsigned *)
let rec vuN n s : uint64 =
  require (n > 0) s (pos s) "integer representation too long";
  let b = u8 s in
  require (n >= 7 || b &&& 0x7f < (1 <<< n)) s (pos s - 1) "integer too large";
  let x = uint64 (b &&& 0x7f) in
  if b &&& 0x80 = 0 then x else x ||| ((vuN (n - 7) s) <<< 7)

(* LEB128. Signed *)
let rec vsN n s : int64 =
  require (n > 0) s (pos s) "integer representation too long";
  let b = u8 s in
  let mask = (-1 <<< (n - 1)) &&& 0x7f in
  require (n >= 7 || b &&& mask = 0 || b &&& mask = mask) s (pos s - 1)
    "integer too large";
  let x = int64 (b &&& 0x7f) in
  if b &&& 0x80 = 0
  then (if b &&& 0x40 = 0 then x else x ||| (-1L ^^^ 0x7fL))
  else x ||| ((vsN (n - 7) s) <<< 7)

let vu1 s = int (vuN 1 s)
let vu32 s = uint32 (vuN 32 s)
let vs7 s = int (vsN 7 s)
let vs32 s = int (vsN 32 s)
let vs64 s = vsN 64 s
let f32 s = System.BitConverter.Int32BitsToSingle (int (u32 s))
let f64 s = System.BitConverter.Int64BitsToDouble (int64 (u64 s))

let len32 s : int =
  let pos = pos s in
  let n = vu32 s in
  if n <= (len s |> uint32) then int n else
    error s pos "length out of bounds"

let bool s = (vu1 s = 1)
let string1 s = let n = len32 s in getString n s
let rec list f n s = if n = 0 then [] else let x = f s in x :: list f (n - 1) s
let opt f b s = if b then Some (f s) else None
let vec f s = let n = len32 s in list f n s

open System

let name (s: Stream) : string =
  let pos = pos s in
  try System.Text.Encoding.UTF8.GetString (string1 s) with
      | :? System.ArgumentException -> error s pos "invalid UTF-8 encoding"

let sized f s =
  let size = len32 s in
  let start = pos s in
  let x = f size s in
  require (pos s = start + size) s start "section size mismatch";
  x


(* Types *)

let ValueType s =
  match vs7 s with
  | -0x01 -> I32Type
  | -0x02 -> I64Type
  | -0x03 -> F32Type
  | -0x04 -> F64Type
  | _ -> error s (pos s - 1) "invalid value type"

let elemType s =
  match vs7 s with
  | -0x10 -> FuncRefType
  | _ -> error s (pos s - 1) "invalid element type"

let stackType s =
  match peek s with
  | Some 0x40uy -> skip 1 s; []
  | _ -> [ValueType s]

let funcType s =
  match vs7 s with
  | -0x20 ->
    let ins = vec ValueType s in
    let out = vec ValueType s in
    FuncType (ins, out)
  | _ -> error s (pos s - 1) "invalid function type"

let limits vu s =
  let hasMax = bool s in
  let min = vu s in
  let max = opt vu hasMax s in
  {min=min; max=max}

let tableType s =
  let t = elemType s in
  let lim = limits vu32 s in
  TableType (lim, t)

let memoryType s =
  let lim = limits vu32 s in
  MemoryType lim

let mutability s =
  match u8 s with
  | 0 -> Immutable
  | 1 -> Mutable
  | _ -> error s (pos s - 1) "invalid mutability"

let globalType s =
  let t = ValueType s in
  let mut = mutability s in
  GlobalType (t, mut)


(* Decode instructions *)

let var (s: Stream) : uint32 = vu32 s

let op s = u8 s
let end1 s = expect 0x0buy s "END opcode expected"

let memop s : int * uint32 =
  let align = vu32 s in
  require (align <= 32u) s (pos s - 1) "invalid memop flags";
  let offset = vu32 s in
  int align, offset

let rec instr s =
  let pos = pos s in
  match int (op s) with
  | 0x00 -> unreachable
  | 0x01 -> nop

  | 0x02 ->
    let ts = stackType s in
    let es' = instrBlock s in
    end1 s;
    block ts es'
  | 0x03 ->
    let ts = stackType s in
    let es' = instrBlock s in
    end1 s;
    loop ts es'
  | 0x04 ->
    let ts = stackType s in
    let es1 = instrBlock s in
    if peek s = Some 0x05uy then begin
      expect 0x05uy s "ELSE or END opcode expected";
      let es2 = instrBlock s in
      end1 s;
      if1 ts es1 es2
    end else begin
      end1 s;
      if1 ts es1 []
    end

  | 0x05 -> error s pos "misplaced ELSE opcode"
  | 0x06| 0x07 | 0x08 | 0x09 | 0x0a as b -> illegal s pos b
  | 0x0b -> error s pos "misplaced END opcode"

  | 0x0c -> br (at var s)
  | 0x0d -> brIf (at var s)
  | 0x0e ->
    let xs = vec (at var) s in
    let x = at var s in
    brTable xs x
  | 0x0f -> return1

  | 0x10 -> call (at var s)
  | 0x11 ->
    let x = at var s in
    expect 0x00uy s "zero flag expected";
    callIndirect x

  | 0x12 | 0x13 | 0x14 | 0x15 | 0x16 | 0x17 | 0x18 | 0x19 as b -> illegal s pos b

  | 0x1a -> drop
  | 0x1b -> select

  | 0x1c | 0x1d | 0x1e | 0x1f as b -> illegal s pos b

  | 0x20 -> localGet (at var s)
  | 0x21 -> localSet (at var s)
  | 0x22 -> localTee (at var s)
  | 0x23 -> globalGet (at var s)
  | 0x24 -> globalSet (at var s)

  | 0x25 | 0x26 | 0x27 as b -> illegal s pos b

  | 0x28 -> let a, o = memop s in i32load a o
  | 0x29 -> let a, o = memop s in i64load a o
  | 0x2a -> let a, o = memop s in f32load a o
  | 0x2b -> let a, o = memop s in f64load a o
  | 0x2c -> let a, o = memop s in i32load8s a o
  | 0x2d -> let a, o = memop s in i32load8u a o
  | 0x2e -> let a, o = memop s in i32load16s a o
  | 0x2f -> let a, o = memop s in i32load16u a o
  | 0x30 -> let a, o = memop s in i64load8s a o
  | 0x31 -> let a, o = memop s in i64load8u a o
  | 0x32 -> let a, o = memop s in i64load16s a o
  | 0x33 -> let a, o = memop s in i64load16u a o
  | 0x34 -> let a, o = memop s in i64load32s a o
  | 0x35 -> let a, o = memop s in i64load32u a o

  | 0x36 -> let a, o = memop s in i32Store a o
  | 0x37 -> let a, o = memop s in i64Store a o
  | 0x38 -> let a, o = memop s in f32Store a o
  | 0x39 -> let a, o = memop s in f64Store a o
  | 0x3a -> let a, o = memop s in i32Store8 a o
  | 0x3b -> let a, o = memop s in i32Store16 a o
  | 0x3c -> let a, o = memop s in i64Store8 a o
  | 0x3d -> let a, o = memop s in i64Store16 a o
  | 0x3e -> let a, o = memop s in i64Store32 a o

  | 0x3f ->
    expect 0x00uy s "zero flag expected";
    memorySize
  | 0x40 ->
    expect 0x00uy s "zero flag expected";
    memoryGrow

  | 0x41 -> i32Const (at vs32 s)
  | 0x42 -> i64Const (at vs64 s)
  | 0x43 -> f32Const (at f32 s)
  | 0x44 -> f64Const (at f64 s)

  | 0x45 -> i32Eqz
  | 0x46 -> i32Eq
  | 0x47 -> i32Ne
  | 0x48 -> i32Lts
  | 0x49 -> i32Ltu
  | 0x4a -> i32Gts
  | 0x4b -> i32Gtu
  | 0x4c -> i32Les
  | 0x4d -> i32Leu
  | 0x4e -> i32Ges
  | 0x4f -> i32Geu

  | 0x50 -> i64Eqz
  | 0x51 -> i64Eq
  | 0x52 -> i64Ne
  | 0x53 -> i64Lts
  | 0x54 -> i64Ltu
  | 0x55 -> i64Gts
  | 0x56 -> i64Gtu
  | 0x57 -> i64Les
  | 0x58 -> i64Leu
  | 0x59 -> i64Ges
  | 0x5a -> i64Geu

  | 0x5b -> f32Eq
  | 0x5c -> f32Ne
  | 0x5d -> f32Lt
  | 0x5e -> f32Gt
  | 0x5f -> f32Le
  | 0x60 -> f32Ge

  | 0x61 -> f64Eq
  | 0x62 -> f64Ne
  | 0x63 -> f64Lt
  | 0x64 -> f64Gt
  | 0x65 -> f64Le
  | 0x66 -> f64Ge

  | 0x67 -> i32Clz
  | 0x68 -> i32Ctz
  | 0x69 -> i32Popcnt
  | 0x6a -> i32Add
  | 0x6b -> i32Sub
  | 0x6c -> i32Mul
  | 0x6d -> i32Divs
  | 0x6e -> i32Divu
  | 0x6f -> i32Rems
  | 0x70 -> i32Remu
  | 0x71 -> i32And
  | 0x72 -> i32Or
  | 0x73 -> i32Xor
  | 0x74 -> i32Shl
  | 0x75 -> i32Shrs
  | 0x76 -> i32Shru
  | 0x77 -> i32Rotl
  | 0x78 -> i32Rotr

  | 0x79 -> i64Clz
  | 0x7a -> i64Ctz
  | 0x7b -> i64Popcnt
  | 0x7c -> i64Add
  | 0x7d -> i64Sub
  | 0x7e -> i64Mul
  | 0x7f -> i64Divs
  | 0x80 -> i64Divu
  | 0x81 -> i64Rems
  | 0x82 -> i64Remu
  | 0x83 -> i64And
  | 0x84 -> i64Or
  | 0x85 -> i64Xor
  | 0x86 -> i64Shl
  | 0x87 -> i64Shrs
  | 0x88 -> i64Shru
  | 0x89 -> i64Rotl
  | 0x8a -> i64Rotr

  | 0x8b -> f32Abs
  | 0x8c -> f32Neg
  | 0x8d -> f32Ceil
  | 0x8e -> f32Floor
  | 0x8f -> f32Trunc
  | 0x90 -> f32Nearest
  | 0x91 -> f32Sqrt
  | 0x92 -> f32Add
  | 0x93 -> f32Sub
  | 0x94 -> f32Mul
  | 0x95 -> f32Div
  | 0x96 -> f32Min
  | 0x97 -> f32Max
  | 0x98 -> f32Copysign

  | 0x99 -> f64Abs
  | 0x9a -> f64Neg
  | 0x9b -> f64Ceil
  | 0x9c -> f64Floor
  | 0x9d -> f64Trunc
  | 0x9e -> f64Nearest
  | 0x9f -> f64Sqrt
  | 0xa0 -> f64Add
  | 0xa1 -> f64Sub
  | 0xa2 -> f64Mul
  | 0xa3 -> f64Div
  | 0xa4 -> f64Min
  | 0xa5 -> f64Max
  | 0xa6 -> f64Copysign

  | 0xa7 -> i32WrapI64
  | 0xa8 -> i32TruncF32s
  | 0xa9 -> i32TruncF32u
  | 0xaa -> i32TruncF64s
  | 0xab -> i32TruncF64u
  | 0xac -> i64ExtendI32s
  | 0xad -> i64ExtendI32u
  | 0xae -> i64TruncF32s
  | 0xaf -> i64TruncF32u
  | 0xb0 -> i64TruncF64s
  | 0xb1 -> i64TruncF64u
  | 0xb2 -> f32ConvertI32s
  | 0xb3 -> f32ConvertI32u
  | 0xb4 -> f32ConvertI64s
  | 0xb5 -> f32ConvertI64u
  | 0xb6 -> f32DemoteF64
  | 0xb7 -> f64ConvertI32s
  | 0xb8 -> f64ConvertI32u
  | 0xb9 -> f64ConvertI64s
  | 0xba -> f64ConvertI64u
  | 0xbb -> f64PromoteF32

  | 0xbc -> i32ReinterpretF32
  | 0xbd -> i64ReinterpretF64
  | 0xbe -> f32ReinterpretI32
  | 0xbf -> f64ReinterpretI64

  | b -> illegal s pos b

and instrBlock s = List.rev (instrBlock' s [])
and instrBlock' s es =
  match peek s with
  | None | Some (0x05uy | 0x0buy) -> es
  | _ ->
    let pos = pos s in
    let e' = instr s in
    instrBlock' s ((e' @@ pos) :: es)

let const1 s =
  let c = at instrBlock s in
  end1 s;
  c


(* Sections *)

type Section =
    | CustomSection
    | TypeSection
    | ImportSection
    | FuncSection
    | TableSection
    | MemorySection
    | GlobalSection
    | ExportSection
    | StartSection
    | ElemSection
    | CodeSection
    | DataSection

let id s =
  let bo = peek s in
  Option.map
    (function
    | 0uy -> CustomSection
    | 1uy -> TypeSection
    | 2uy -> ImportSection
    | 3uy -> FuncSection
    | 4uy -> TableSection
    | 5uy -> MemorySection
    | 6uy -> GlobalSection
    | 7uy -> ExportSection
    | 8uy -> StartSection
    | 9uy -> ElemSection
    | 10uy -> CodeSection
    | 11uy -> DataSection
    | _ -> error s (pos s) "invalid section id"
    ) bo

let sectionWithSize tag f default1 s =
  match id s with
  | Some tag' when tag' = tag -> ignore (u8 s); sized f s
  | _ -> default1

let section tag f default1 s =
  sectionWithSize tag (fun _ -> f) default1 s


(* Type section *)

let type1 s = at funcType s

let typeSection s =
  section TypeSection (vec type1) [] s


(* Import section *)

let importDesc s =
  match u8 s with
  | 0x00 -> FuncImport (at var s)
  | 0x01 -> TableImport (tableType s)
  | 0x02 -> MemoryImport (memoryType s)
  | 0x03 -> GlobalImport (globalType s)
  | _ -> error s (pos s - 1) "invalid import kind"

let itest (s: Stream) : string =
  name s

let import (s: Stream) =
  let moduleName = name s in
  let itemName = name s in
  let idesc = at importDesc s in
  {moduleName=moduleName; itemName=itemName; idesc=idesc}

let importSection s =
  section ImportSection (vec (at import)) [] s


(* Function section *)

let funcSection s =
  section FuncSection (vec (at var)) [] s


(* Table section *)

let table s =
  let ttype = tableType s in
  {ttype=ttype}

let tableSection s =
  section TableSection (vec (at table)) [] s


(* Memory section *)

let memory s =
  let mtype = memoryType s in
  {mtype=mtype}

let memorySection s =
  section MemorySection (vec (at memory)) [] s


(* Global section *)

let global1 s =
  let gtype = globalType s in
  let value = const1 s in
  {gtype=gtype; value=value}

let globalSection s =
  section GlobalSection (vec (at global1)) [] s


(* Export section *)

let exportDesc s =
  match u8 s with
  | 0x00 -> FuncExport (at var s)
  | 0x01 -> TableExport (at var s)
  | 0x02 -> MemoryExport (at var s)
  | 0x03 -> GlobalExport (at var s)
  | _ -> error s (pos s - 1) "invalid export kind"

let export s =
  let name = name s in
  let edesc = at exportDesc s in
  {name=name; edesc=edesc}

let exportSection s =
  section ExportSection (vec (at export)) [] s


(* Start section *)

let startSection s =
  section StartSection (opt (at var) true) None s


(* Code section *)

let local s =
  let n = vu32 s in
  let t = ValueType s in
  n, t

let code _ s =
  let pos = pos s in
  let nts = vec local s in
  let ns = List.map (fun (n, _) -> int64 n) nts in
  require (List.sum ns < 0x1_0000_0000L)
    s pos "too many locals";
  let locals = List.ofSeq  (Seq.collect (Fun.uncurry List32.make) nts) in
  let body = instrBlock s in
  end1 s;
  {locals=locals; body=body; ftype = ((uint32 -1) @@ noRegion)}

let codeSection s =
  section CodeSection (vec (at (sized code))) [] s


(* Element section *)

let segment dat s =
  let index = at var s in
  let offset = const1 s in
  let init = dat s in
  {index=index; offset=offset; init=init}

let tableSegment s =
  segment (vec (at var)) s

let elemSection s =
  section ElemSection (vec (at tableSegment)) [] s


(* Data section *)

let memorySegment s =
  segment string1 s

let dataSection s =
  section DataSection (vec (at memorySegment)) [] s


(* Custom section *)

let custom size s =
  let start = pos s in
  let _id = name s in
  skip (size - (pos s - start)) s;
  true

let customSection s =
  sectionWithSize CustomSection custom false s


(* Modules *)

let rec iterate f s = if f s then iterate f s

let makeModule s =
  let magic = u32 s in
  require (magic = 0x6d736100ul) s 0 "magic header not detected";
  let version = u32 s in
  require (version = 1ul) s 4 "unknown binary version";
  iterate customSection s;
  let types = typeSection s in
  iterate customSection s;
  let imports = importSection s in
  iterate customSection s;
  let funcTypes = funcSection s in
  iterate customSection s;
  let tables = tableSection s in
  iterate customSection s;
  let memories = memorySection s in
  iterate customSection s;
  let globals = globalSection s in
  iterate customSection s;
  let exports = exportSection s in
  iterate customSection s;
  let start = startSection s in
  iterate customSection s;
  let elems = elemSection s in
  iterate customSection s;
  let funcBodies = codeSection s in
  iterate customSection s;
  let data = dataSection s in
  iterate customSection s;
  require (pos s = len s) s (len s) "junk after last section";
  require (List.length funcTypes = List.length funcBodies)
    s (len s) "function and code section have inconsistent lengths";
  let funcs =
    List.map2 (fun t (f: Func) -> {f.it with ftype = t} @@ f.at)
      funcTypes funcBodies
  in {types=types; tables=tables; memories=memories; globals=globals; funcs=funcs;
      imports=imports; exports=exports; elems=elems; data=data; start=start}


let decode (bs: byte array) = at makeModule (stream bs)
