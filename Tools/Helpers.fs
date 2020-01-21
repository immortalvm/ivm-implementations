module Tools.Helpers

open System.IO
open System.IO.Compression

open Assembler.Ast
open Assembler.Namespace
open Assembler.Parser
open Assembler.Integration

// These constants should be annotated [<Literal>], but then they can no longer
// be made available through the signature file. This is (yet another) reason
// to stop using signature files.
let SOURCE_EXTENSION = ".s"
let BINARY_EXTENSION = ".b"
let SYMBOLS_EXTENSION = ".sym"

let private uniqueFullPaths (filenames: seq<string>) =
    let mutable seen = set []
    seq {
        for full in Seq.map Path.GetFullPath filenames do
            if not <| seen.Contains full then
                yield full
    }

// Remove the extension but keep the directory
let pathToNode (path: string) : string =
    Path.Combine(
        Path.GetDirectoryName path,
        Path.GetFileNameWithoutExtension path)

let private relativeNodes (sourceRoot: string) (fullPaths: seq<string>): seq<string * string> =
    seq {
        for path in fullPaths ->
            // Does this work if path is not below sourceRoot?
            (Path.GetRelativePath (sourceRoot, path) |> pathToNode), path
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
    let fullPaths = uniqueFullPaths filenames
    let pairs = match sourceRoot with
                | Some sr -> relativeNodes sr fullPaths
                | None -> nonRelativeNodes fullPaths
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
    let contains (node: string) =
        File.Exists <| path node
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

[<Literal>]
let EXPORT_FILE = ".exports"

[<Literal>]
let DEPENDENCY_SUFFIX = ".deps"

[<Literal>]
let IMPLICIT_IMPORT_SUFFIX = ".implimps"

let dirToZipLib (root: string) (filename: string) =
    use fs = new FileStream (filename, FileMode.Create)
    use ar = new ZipArchive (fs, ZipArchiveMode.Create)

    let analyses = 
        let files = Directory.EnumerateFiles (root, "*" + SOURCE_EXTENSION, SearchOption.AllDirectories)
        [ 
            for node, filename in files |> relativeNodes root do
                printfn "Analyzing %s..." node
                yield node, filename, analyze <| fun () -> upcast File.OpenRead filename
        ]

    let exports =
        let res = Map <| seq {
            for other, _, a in analyses do
                for sym in a.Exported do
                    yield sym, other
        }
        let toSort = Seq.toArray <| seq { for pair in res -> pair.Key, pair.Value }
        Array.sortInPlaceBy fst toSort
        printfn "Creating %s..." EXPORT_FILE
        let e = ar.CreateEntry EXPORT_FILE
        use s = e.Open ()
        use w = new StreamWriter(s)
        for pair in res do
            sprintf "%s\t%s" pair.Key pair.Value |> w.WriteLine
        res

    for node, filename, a in analyses do
        printfn "Creating files for %s..." node
        let others = [
            for sym in a.Undefined do
                match Map.tryFind sym exports with
                | Some other -> yield other
                | None -> failwithf "Not found: %s (used in %s)" sym node
        ]
        (
            let e = ar.CreateEntry <| node + DEPENDENCY_SUFFIX
            use s = e.Open ()
            use w = new StreamWriter(s)
            for dep in set <| Seq.append a.ImportsFrom others do
                w.WriteLine dep
        )
        (
            let e = ar.CreateEntry <| node + IMPLICIT_IMPORT_SUFFIX
            use s = e.Open ()
            use w = new StreamWriter(s)
            for sym, other in Seq.zip a.Undefined others do
                nodeJoin [other; sym] |> w.WriteLine
        )
        (
            let e = ar.CreateEntry <| node + SOURCE_EXTENSION
            use s = e.Open ()
            use from = File.OpenRead filename
            from.CopyTo s
        )

let lines (r: StreamReader): seq<string> =
    seq {
        while not r.EndOfStream do
        yield r.ReadLine()
    }

let tabLines (r: StreamReader): seq<string[]> =
    seq { for line in lines r -> line.Split('\t') }

let libFromZip (filename: string) : Library =
    let withAr fn =
        using (new FileStream (filename, FileMode.Open))
        <| fun fs -> using (new ZipArchive (fs, ZipArchiveMode.Read)) fn

    let cache fn =
        let mutable v = None
        fun () ->
            match v with
            | Some w -> w
            | None ->
                let w = withAr fn
                v <- Some w
                w

    let contains = cache <| fun ar ->
        ar.Entries |> Seq.map (fun e -> e.FullName |> pathToNode) |> set

    let exports = cache <| fun ar ->
        let e = ar.GetEntry EXPORT_FILE
        use s = e.Open()
        use r = new StreamReader(s)
        Map <| seq { for line in tabLines r -> line.[0], line.[1] }

    let dependencies node = withAr <| fun ar ->
        let e = ar.GetEntry <| node + DEPENDENCY_SUFFIX
        use s = e.Open()
        use r = new StreamReader(s)
        lines r |> set // Read the lines before closing the stream.

    let get node = withAr <| fun ar ->
        let imp =
            let e = ar.GetEntry <| node + IMPLICIT_IMPORT_SUFFIX
            use s = e.Open()
            use r = new StreamReader(s)
            lines r |> Seq.toList
        use s = ar.GetEntry(node + SOURCE_EXTENSION).Open()
        // TODO: Using a MemoryStream is a hack to get around that ar will be closed.
        let ms = new MemoryStream ()
        s.CopyTo ms
        ms.Position <- 0L
        imp, (upcast ms : Stream)

    // TODO: It would be faster to use binary search with sorted arrays above.
    {
        Contains = fun node -> contains().Contains node
        ExportedBy = fun sym -> Map.tryFind sym <| exports ()
        Dependencies = dependencies
        Get = get
    }

let libraries (sourceRoot: string option) (libs: string list): Library list =
    (match sourceRoot with
    | None -> emptyLibrary
    | Some sr -> rootLib sr) :: List.map libFromZip libs
