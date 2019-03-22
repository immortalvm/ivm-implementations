module DecoderTests

open System
open Expecto
open Expecto.Impl
open FsCheck
open Swensen.Unquote

open Basics
open Ast


[<Tests>]
let tests =
    testList "Decoder tests" [
        testProperty "Decode WebAssembly Studio example" <| fun (NonNull input) ->
            // (module
            //   (type $t0 (func (param i32 i32) (result i32)))
            //   (func $add (export "add") (type $t0) (param $lhs i32) (param $rhs i32) (result i32)
            //     get_local $lhs
            //     get_local $rhs
            //     i32.add))
            let program = [| 0x00; 0x61; 0x73; 0x6d; 0x01; 0x00; 0x00; 0x00; 0x01; 0x07; 0x01; 0x60; 0x02; 0x7f; 0x7f; 0x01;
                             0x7f; 0x03; 0x02; 0x01; 0x00; 0x07; 0x07; 0x01; 0x03; 0x61; 0x64; 0x64; 0x00; 0x00; 0x0a; 0x09;
                             0x01; 0x07; 0x00; 0x20; 0x00; 0x20; 0x01; 0x6a; 0x0b; 0x00; 0x1c; 0x04; 0x6e; 0x61; 0x6d; 0x65;
                             0x01; 0x06; 0x01; 0x00; 0x03; 0x61; 0x64; 0x64; 0x02; 0x0d; 0x01; 0x00; 0x02; 0x00; 0x03; 0x6c;
                             0x68; 0x73; 0x01; 0x03; 0x72; 0x68; 0x73; |]
            let expected = {at = 0;
                            it =
                             {types = [{at = 11; it = FuncType ([I32Type; I32Type],[I32Type])}];
                              globals = [];
                              tables = [];
                              memories = [];
                              funcs =
                               [{at = 33;
                                 it =
                                  {ftype = {at = 20; it = 0u};
                                   locals = [];
                                   body =
                                    [{at = 35; it = LocalGet {at = 36; it = 0u}};
                                     {at = 37; it = LocalGet {at = 38; it = 1u}};
                                     {at = 39; it = Binary (I32 IntOp.Add)}]}}];
                              start = None;
                              elems = [];
                              data = [];
                              imports = [];
                              exports = [{at = 24;
                                          it = {name = "add";
                                                edesc = {at = 28; it = FuncExport {at = 29; it = 0u}}}}]}}
            test <@ Decode.decode (Array.map byte program) = expected @>
    ]
