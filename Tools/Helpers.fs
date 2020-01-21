module Tools.Helpers

open System.IO
open Assembler.Namespace

let nodePath rootDir extension (node: string) =
    (Array.append [|rootDir|] (splitNodeString node) |> Path.Combine) + extension

let private uniqueNodesAndFiles (filenames: seq<string>): seq<string * string> =
    let mutable rev: (string * string) list = []
    for path in filenames do
        let full = Path.GetFullPath path
        if not <| List.exists (snd >> (=) full) rev then
            let name = Path.GetFileNameWithoutExtension path
            // This is very primitive, but presumably this will not happen very often.
            let node = if List.exists (fst >> (=) name) rev then full else name
            rev <- (node, full) :: rev
    Seq.rev rev

let src (filenames: string list): (string * (unit -> Stream)) list =
    [
        for node, full in uniqueNodesAndFiles filenames ->
            node, fun () ->
                printfn "Opening %s..." full
                upcast File.OpenRead full
    ]
    