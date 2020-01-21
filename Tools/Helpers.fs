module Tools.Helpers

open System.IO
open Assembler.Ast
open Assembler.Namespace
open Assembler.Parser
open Assembler.Integration

let private uniqueFullPaths (filenames: seq<string>) =
    let mutable seen = set []
    seq {
        for full in Seq.map Path.GetFullPath filenames do
            if not <| seen.Contains full then
                yield full
    }

let private relativeNodes (fullPaths: seq<string>) (sourceRoot: string): seq<string * string> =
    seq {
        for path in fullPaths ->
            // Does this work if path is not below sourceRoot?
            Path.GetRelativePath (path, sourceRoot), path
    }

let private nonRelativeNodes (fullPaths: seq<string>): seq<string * string> =
    let names = [for path in fullPaths -> Path.GetFileNameWithoutExtension path]
    let mutable seen = set []
    seq {
        for name, path in Seq.zip names fullPaths do
            let node =
                if not <| seen.Contains name then name
                else Seq.initInfinite id
                     |> Seq.map (sprintf "%s$%d" name)
                     |> Seq.filter (seen.Contains >> not)
                     |> Seq.filter (fun s -> not <| List.contains s names)
                     |> Seq.head
            yield node, path
            seen <- seen.Add node
    }

let src (filenames: string list) (sourceRoot: string option): (string * (unit -> Stream)) list =
    let full = uniqueFullPaths filenames
    let pairs = match sourceRoot with
                | Some sr -> relativeNodes full sr
                | None -> nonRelativeNodes full
    [
        for node, full in pairs ->
            node, fun () ->
                printfn "Opening %s..." full
                upcast File.OpenRead full
    ]

let nodePath rootDir extension (node: string) =
    (Array.append [|rootDir|] (splitNodeString node) |> Path.Combine) + extension

let emptyLibrary = {
    Contains = fun _ -> false
    ExportedBy = fun _ -> None
    Dependencies = fun node -> failwithf "Missing from library: %s" node
    Get = fun node -> failwithf "Missing from library: %s" node
}

// We only support explicit imports with this library
let private rootLib (sourceRoot: string): Library =
    let path node = nodePath sourceRoot SOURCE_EXTENSION node
    let contains (node: string) = File.Exists <| path node
    let exportedBy (_: string): string option = None
    let dependencies (node: string) : Set<string> =
        let filename = path node
        printfn "Analyzing dependencies in %s..." filename
        use stream = File.OpenRead filename
        try parseDependencies stream
        with ParseException(msg) -> failwith msg
    let get (node: string) : string list * Stream =
        [], upcast (path node |> File.OpenRead)
    {
        Contains = contains
        ExportedBy = exportedBy
        Dependencies = dependencies
        Get = get
    }

let libraries (sourceRoot: string option): Library list =
    match sourceRoot with
    | None -> [emptyLibrary]
    | Some sr -> [rootLib sr]
