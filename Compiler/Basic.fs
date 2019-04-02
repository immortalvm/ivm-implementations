module Basic

open Ast
open VM0

exception UndefinedException

let rec compiler (is: Instr' list) : int list =
  match is with
  | [] -> [EXIT]
  | _  -> failwith "Not implemented";

let rec compileInst (i : Instr') : int =
  match i with
  | Load _op -> raise UndefinedException
  | _ -> raise UndefinedException