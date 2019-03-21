module Decode

(* Decoding stream *)

type stream = {
  name : string;
  bytes : byte array;
  pos : int ref;
}

exception EOS

let stream name bs = {name = name; bytes = bs; pos = ref 0}

let len s : int = Array.length s.bytes
let pos s = !(s.pos)
let eos s = (pos s = len s)

let check n s = if pos s + n > len s then raise EOS
let skip0 n s = if n < 0 then raise EOS else check n s; s.pos := !(s.pos) + n

let read s : byte = s.bytes.[!(s.pos)]
let peek s : byte option = if eos s then None else Some (read s)
let get0 s : byte = check 1 s; let b = read s in skip0 1 s; b
let get_string0 n s : byte array = let i = pos s in skip0 n s; s.bytes.[i..i+n-1]


(* Errors *)

exception Code of Source.region * string

let string_of_byte b = Printf.sprintf "%02x" b

let position (s: stream) pos = {Source.pos.file = s.name; Source.pos.line = -1; Source.pos.column = pos}
let region (s: stream) left right : Source.region =
  {Source.region.left = position s left; Source.region.right = position s right}

let error s pos msg = raise (Code (region s pos pos, msg))
let require b s pos msg = if not b then error s pos msg

let guard f s =
  try f s with EOS -> error s (len s) "unexpected end of section or function"

let get = guard get0
let get_string n = guard (get_string0 n)
let skip n = guard (skip0 n)

let expect b s msg = require (guard get s = b) s (pos s - 1) msg
let illegal s pos b = error s pos ("illegal opcode " ^ string_of_byte b)


(* Since F# does not seem to support local scopes, Source.(....). *)
let (@@) = Source.(@@)


let at (f: stream -> 'a) (s: stream) : 'a Source.phrase =
  let left = pos s in
  let x = f s in
  let right = pos s in
  x @@ region s left right



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
let string1 s = let n = len32 s in get_string n s
let rec list f n s = if n = 0 then [] else let x = f s in x :: list f (n - 1) s
let opt f b s = if b then Some (f s) else None
let vec f s = let n = len32 s in list f n s

open System

let name (s: stream) : string =
  System.Text.Encoding.UTF8.GetString (string1 s)
  (* Trouble catching .NET exceptions in .ml files
  let pos = pos s in
   try System.Text.Encoding.UTF8.GetString (string1 s) with | ?: System.ArgumentException ->
                                                               error s pos "invalid UTF-8 encoding"
   *)

let sized f s =
  let size = len32 s in
  let start = pos s in
  let x = f size s in
  require (pos s = start + size) s start "section size mismatch";
  x


(* Types *)

open Types

let value_type s =
  match vs7 s with
  | -0x01 -> I32Type
  | -0x02 -> I64Type
  | -0x03 -> F32Type
  | -0x04 -> F64Type
  | _ -> error s (pos s - 1) "invalid value type"

let elem_type s =
  match vs7 s with
  | -0x10 -> FuncRefType
  | _ -> error s (pos s - 1) "invalid element type"

let stack_type s =
  match peek s with
  | Some 0x40uy -> skip 1 s; []
  | _ -> [value_type s]

let func_type s =
  match vs7 s with
  | -0x20 ->
    let ins = vec value_type s in
    let out = vec value_type s in
    FuncType (ins, out)
  | _ -> error s (pos s - 1) "invalid function type"

let limits vu s =
  let has_max = bool s in
  let min = vu s in
  let max = opt vu has_max s in
  {min=min; max=max}

let table_type s =
  let t = elem_type s in
  let lim = limits vu32 s in
  TableType (lim, t)

let memory_type s =
  let lim = limits vu32 s in
  MemoryType lim

let mutability s =
  match u8 s with
  | 0 -> Immutable
  | 1 -> Mutable
  | _ -> error s (pos s - 1) "invalid mutability"

let global_type s =
  let t = value_type s in
  let mut = mutability s in
  GlobalType (t, mut)


(* Decode instructions *)

open Ast
open Operators

let var (s: stream) : uint32 = vu32 s

let op s = u8 s
let end_ s = expect 0x0buy s "END opcode expected"

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
    let ts = stack_type s in
    let es' = instr_block s in
    end_ s;
    block ts es'
  | 0x03 ->
    let ts = stack_type s in
    let es' = instr_block s in
    end_ s;
    loop ts es'
  | 0x04 ->
    let ts = stack_type s in
    let es1 = instr_block s in
    if peek s = Some 0x05uy then begin
      expect 0x05uy s "ELSE or END opcode expected";
      let es2 = instr_block s in
      end_ s;
      if_ ts es1 es2
    end else begin
      end_ s;
      if_ ts es1 []
    end

  | 0x05 -> error s pos "misplaced ELSE opcode"
  | 0x06| 0x07 | 0x08 | 0x09 | 0x0a as b -> illegal s pos b
  | 0x0b -> error s pos "misplaced END opcode"

  | 0x0c -> br (at var s)
  | 0x0d -> br_if (at var s)
  | 0x0e ->
    let xs = vec (at var) s in
    let x = at var s in
    br_table xs x
  | 0x0f -> return1

  | 0x10 -> call (at var s)
  | 0x11 ->
    let x = at var s in
    expect 0x00uy s "zero flag expected";
    call_indirect x

  | 0x12 | 0x13 | 0x14 | 0x15 | 0x16 | 0x17 | 0x18 | 0x19 as b -> illegal s pos b

  | 0x1a -> drop
  | 0x1b -> select

  | 0x1c | 0x1d | 0x1e | 0x1f as b -> illegal s pos b

  | 0x20 -> local_get (at var s)
  | 0x21 -> local_set (at var s)
  | 0x22 -> local_tee (at var s)
  | 0x23 -> global_get (at var s)
  | 0x24 -> global_set (at var s)

  | 0x25 | 0x26 | 0x27 as b -> illegal s pos b

  | 0x28 -> let a, o = memop s in i32_load a o
  | 0x29 -> let a, o = memop s in i64_load a o
  | 0x2a -> let a, o = memop s in f32_load a o
  | 0x2b -> let a, o = memop s in f64_load a o
  | 0x2c -> let a, o = memop s in i32_load8_s a o
  | 0x2d -> let a, o = memop s in i32_load8_u a o
  | 0x2e -> let a, o = memop s in i32_load16_s a o
  | 0x2f -> let a, o = memop s in i32_load16_u a o
  | 0x30 -> let a, o = memop s in i64_load8_s a o
  | 0x31 -> let a, o = memop s in i64_load8_u a o
  | 0x32 -> let a, o = memop s in i64_load16_s a o
  | 0x33 -> let a, o = memop s in i64_load16_u a o
  | 0x34 -> let a, o = memop s in i64_load32_s a o
  | 0x35 -> let a, o = memop s in i64_load32_u a o

  | 0x36 -> let a, o = memop s in i32_store a o
  | 0x37 -> let a, o = memop s in i64_store a o
  | 0x38 -> let a, o = memop s in f32_store a o
  | 0x39 -> let a, o = memop s in f64_store a o
  | 0x3a -> let a, o = memop s in i32_store8 a o
  | 0x3b -> let a, o = memop s in i32_store16 a o
  | 0x3c -> let a, o = memop s in i64_store8 a o
  | 0x3d -> let a, o = memop s in i64_store16 a o
  | 0x3e -> let a, o = memop s in i64_store32 a o

  | 0x3f ->
    expect 0x00uy s "zero flag expected";
    memory_size
  | 0x40 ->
    expect 0x00uy s "zero flag expected";
    memory_grow

  | 0x41 -> i32_const (at vs32 s)
  | 0x42 -> i64_const (at vs64 s)
  | 0x43 -> f32_const (at f32 s)
  | 0x44 -> f64_const (at f64 s)

  | 0x45 -> i32_eqz
  | 0x46 -> i32_eq
  | 0x47 -> i32_ne
  | 0x48 -> i32_lt_s
  | 0x49 -> i32_lt_u
  | 0x4a -> i32_gt_s
  | 0x4b -> i32_gt_u
  | 0x4c -> i32_le_s
  | 0x4d -> i32_le_u
  | 0x4e -> i32_ge_s
  | 0x4f -> i32_ge_u

  | 0x50 -> i64_eqz
  | 0x51 -> i64_eq
  | 0x52 -> i64_ne
  | 0x53 -> i64_lt_s
  | 0x54 -> i64_lt_u
  | 0x55 -> i64_gt_s
  | 0x56 -> i64_gt_u
  | 0x57 -> i64_le_s
  | 0x58 -> i64_le_u
  | 0x59 -> i64_ge_s
  | 0x5a -> i64_ge_u

  | 0x5b -> f32_eq
  | 0x5c -> f32_ne
  | 0x5d -> f32_lt
  | 0x5e -> f32_gt
  | 0x5f -> f32_le
  | 0x60 -> f32_ge

  | 0x61 -> f64_eq
  | 0x62 -> f64_ne
  | 0x63 -> f64_lt
  | 0x64 -> f64_gt
  | 0x65 -> f64_le
  | 0x66 -> f64_ge

  | 0x67 -> i32_clz
  | 0x68 -> i32_ctz
  | 0x69 -> i32_popcnt
  | 0x6a -> i32_add
  | 0x6b -> i32_sub
  | 0x6c -> i32_mul
  | 0x6d -> i32_div_s
  | 0x6e -> i32_div_u
  | 0x6f -> i32_rem_s
  | 0x70 -> i32_rem_u
  | 0x71 -> i32_and
  | 0x72 -> i32_or
  | 0x73 -> i32_xor
  | 0x74 -> i32_shl
  | 0x75 -> i32_shr_s
  | 0x76 -> i32_shr_u
  | 0x77 -> i32_rotl
  | 0x78 -> i32_rotr

  | 0x79 -> i64_clz
  | 0x7a -> i64_ctz
  | 0x7b -> i64_popcnt
  | 0x7c -> i64_add
  | 0x7d -> i64_sub
  | 0x7e -> i64_mul
  | 0x7f -> i64_div_s
  | 0x80 -> i64_div_u
  | 0x81 -> i64_rem_s
  | 0x82 -> i64_rem_u
  | 0x83 -> i64_and
  | 0x84 -> i64_or
  | 0x85 -> i64_xor
  | 0x86 -> i64_shl
  | 0x87 -> i64_shr_s
  | 0x88 -> i64_shr_u
  | 0x89 -> i64_rotl
  | 0x8a -> i64_rotr

  | 0x8b -> f32_abs
  | 0x8c -> f32_neg
  | 0x8d -> f32_ceil
  | 0x8e -> f32_floor
  | 0x8f -> f32_trunc
  | 0x90 -> f32_nearest
  | 0x91 -> f32_sqrt
  | 0x92 -> f32_add
  | 0x93 -> f32_sub
  | 0x94 -> f32_mul
  | 0x95 -> f32_div
  | 0x96 -> f32_min
  | 0x97 -> f32_max
  | 0x98 -> f32_copysign

  | 0x99 -> f64_abs
  | 0x9a -> f64_neg
  | 0x9b -> f64_ceil
  | 0x9c -> f64_floor
  | 0x9d -> f64_trunc
  | 0x9e -> f64_nearest
  | 0x9f -> f64_sqrt
  | 0xa0 -> f64_add
  | 0xa1 -> f64_sub
  | 0xa2 -> f64_mul
  | 0xa3 -> f64_div
  | 0xa4 -> f64_min
  | 0xa5 -> f64_max
  | 0xa6 -> f64_copysign

  | 0xa7 -> i32_wrap_i64
  | 0xa8 -> i32_trunc_f32_s
  | 0xa9 -> i32_trunc_f32_u
  | 0xaa -> i32_trunc_f64_s
  | 0xab -> i32_trunc_f64_u
  | 0xac -> i64_extend_i32_s
  | 0xad -> i64_extend_i32_u
  | 0xae -> i64_trunc_f32_s
  | 0xaf -> i64_trunc_f32_u
  | 0xb0 -> i64_trunc_f64_s
  | 0xb1 -> i64_trunc_f64_u
  | 0xb2 -> f32_convert_i32_s
  | 0xb3 -> f32_convert_i32_u
  | 0xb4 -> f32_convert_i64_s
  | 0xb5 -> f32_convert_i64_u
  | 0xb6 -> f32_demote_f64
  | 0xb7 -> f64_convert_i32_s
  | 0xb8 -> f64_convert_i32_u
  | 0xb9 -> f64_convert_i64_s
  | 0xba -> f64_convert_i64_u
  | 0xbb -> f64_promote_f32

  | 0xbc -> i32_reinterpret_f32
  | 0xbd -> i64_reinterpret_f64
  | 0xbe -> f32_reinterpret_i32
  | 0xbf -> f64_reinterpret_i64

  | b -> illegal s pos b

and instr_block s = List.rev (instr_block' s [])
and instr_block' s es =
  match peek s with
  | None | Some (0x05uy | 0x0buy) -> es
  | _ ->
    let pos = pos s in
    let e' = instr s in
    instr_block' s ((e' @@ region s pos pos) :: es)

let const1 s =
  let c = at instr_block s in
  end_ s;
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
  Lib.Option.map
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

let section_with_size tag f default1 s =
  match id s with
  | Some tag' when tag' = tag -> ignore (u8 s); sized f s
  | _ -> default1

let section tag f default1 s =
  section_with_size tag (fun _ -> f) default1 s


(* Type section *)

let type_ s = at func_type s

let type_section s =
  section TypeSection (vec type_) [] s


(* Import section *)

let import_desc s =
  match u8 s with
  | 0x00 -> FuncImport (at var s)
  | 0x01 -> TableImport (table_type s)
  | 0x02 -> MemoryImport (memory_type s)
  | 0x03 -> GlobalImport (global_type s)
  | _ -> error s (pos s - 1) "invalid import kind"

let itest (s: stream) : string =
  name s

let import (s: stream) =
  let module_name = name s in
  let item_name = name s in
  let idesc = at import_desc s in
  {module_name=module_name; item_name=item_name; idesc=idesc}

let import_section s =
  section ImportSection (vec (at import)) [] s


(* Function section *)

let func_section s =
  section FuncSection (vec (at var)) [] s


(* Table section *)

let table s =
  let ttype = table_type s in
  {ttype=ttype}

let table_section s =
  section TableSection (vec (at table)) [] s


(* Memory section *)

let memory s =
  let mtype = memory_type s in
  {mtype=mtype}

let memory_section s =
  section MemorySection (vec (at memory)) [] s


(* Global section *)

let global1 s =
  let gtype = global_type s in
  let value = const1 s in
  {gtype=gtype; value=value}

let global_section s =
  section GlobalSection (vec (at global1)) [] s


(* Export section *)

let export_desc s =
  match u8 s with
  | 0x00 -> FuncExport (at var s)
  | 0x01 -> TableExport (at var s)
  | 0x02 -> MemoryExport (at var s)
  | 0x03 -> GlobalExport (at var s)
  | _ -> error s (pos s - 1) "invalid export kind"

let export s =
  let name = name s in
  let edesc = at export_desc s in
  {name=name; edesc=edesc}

let export_section s =
  section ExportSection (vec (at export)) [] s


(* Start section *)

let start_section s =
  section StartSection (opt (at var) true) None s


(* Code section *)

let local s =
  let n = vu32 s in
  let t = value_type s in
  n, t

let code _ s =
  let pos = pos s in
  let nts = vec local s in
  let ns = List.map (fun (n, _) -> int64 n) nts in
  require ((List.fold (+) 0L ns) < 0x1_0000_0000L)
    s pos "too many locals";
  let locals = List.ofSeq  (Seq.collect (Lib.Fun.uncurry Lib.List32.make) nts) in
  let body = instr_block s in
  end_ s;
  {locals=locals; body=body; ftype = ((uint32 -1) @@ Source.no_region)}

let code_section s =
  section CodeSection (vec (at (sized code))) [] s


(* Element section *)

let segment dat s =
  let index = at var s in
  let offset = const1 s in
  let init = dat s in
  {index=index; offset=offset; init=init}

let table_segment s =
  segment (vec (at var)) s

let elem_section s =
  section ElemSection (vec (at table_segment)) [] s


(* Data section *)

let memory_segment s =
  segment string1 s

let data_section s =
  section DataSection (vec (at memory_segment)) [] s


(* Custom section *)

let custom size s =
  let start = pos s in
  let _id = name s in
  skip (size - (pos s - start)) s;
  true

let custom_section s =
  section_with_size CustomSection custom false s


(* Modules *)

let rec iterate f s = if f s then iterate f s

let module_ s =
  let magic = u32 s in
  require (magic = 0x6d736100ul) s 0 "magic header not detected";
  let version = u32 s in
  require (version = 1ul) s 4 "unknown binary version";
  iterate custom_section s;
  let types = type_section s in
  iterate custom_section s;
  let imports = import_section s in
  iterate custom_section s;
  let func_types = func_section s in
  iterate custom_section s;
  let tables = table_section s in
  iterate custom_section s;
  let memories = memory_section s in
  iterate custom_section s;
  let globals = global_section s in
  iterate custom_section s;
  let exports = export_section s in
  iterate custom_section s;
  let start = start_section s in
  iterate custom_section s;
  let elems = elem_section s in
  iterate custom_section s;
  let func_bodies = code_section s in
  iterate custom_section s;
  let data = data_section s in
  iterate custom_section s;
  require (pos s = len s) s (len s) "junk after last section";
  require (List.length func_types = List.length func_bodies)
    s (len s) "function and code section have inconsistent lengths";
  let funcs =
    List.map2 (fun t (f: func) -> {f.it with ftype = t} @@ f.at)
      func_types func_bodies
  in {types=types; tables=tables; memories=memories; globals=globals; funcs=funcs;
      imports=imports; exports=exports; elems=elems; data=data; start=start}


let decode (name: string) (bs: byte array) = at module_ (stream name bs)
