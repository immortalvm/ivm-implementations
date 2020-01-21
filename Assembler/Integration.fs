module Assembler.Integration

open System.IO
open System.Text.RegularExpressions
open Assembler.Ast
open Assembler.Parser
open Assembler.ConnectedComponents
open Assembler.Composition
open Assembler.Namespace
open Machine.Executor

// These constants should be annotated [<Literal>], but then they can no longer
// be made available through the signature file. This is (yet another) reason
// to stop using signature files.
let SOURCE_EXTENSION = ".s"
let BINARY_EXTENSION = ".b"
let SYMBOLS_EXTENSION = ".sym"

type AssemblerOutput = {
    Node: string
    Binary: seq<uint8>
    Exported: seq<string * int64>
    Constants: seq<string * int64>
    Labels: seq<string * int64>
    Spacers: seq<int * int64>
    Relatives: seq<int>
}

type Library = {
    Contains: string -> bool            // node -> exists in library?
    ExportedBy: string -> string option // symbol -> node
    Dependencies: string -> Set<string> // node -> nodes
    Get: string -> string list * Stream // node -> implicit imports, source
}

// This helper method can be avoided if ConnectedComponents is generalized.
let private findComp (start: seq<int option * string>) (next: int option * string -> seq<int option * string>) =
    let toStr (num: int option, node: string) =
        let prefix = match num with
                     | None -> ""
                     | Some n -> n.ToString ()
        prefix + "#" + node
    let fromStr (str: string) =
        let i = str.IndexOf("#")
        let num = if i = 0 then None else str.[0..i-1] |> int |> Some
        num, str.[i+1..]
    let goal = ""
    let edges str = Seq.map toStr <| if str = goal then start else next (fromStr str)
    findComponents goal edges
    |> Seq.skip 1 // Skip dummy goal component
    |> Seq.map (Seq.map fromStr)

type Chunk = string * (unit -> string list * Stream)

// The node names in files must be unique.
// The first library is the (possibly empty) root library.
let private prepareBuild
    (files: (string * (unit -> Stream)) list) // node -> source
    (libraries: Library list) : seq<seq<Chunk>> =

    let fileNodes = List.map fst files
    let analyses = [for node, streamFun in files -> node, analyze streamFun]
    let fileNum: int option = None

    let explicitImports = [
        for node, a in analyses -> [
            for other in a.ImportsFrom ->
                if List.contains other fileNodes then
                    fileNum, other
                else
                    match Seq.tryFindIndex (fun lib -> lib.Contains other) libraries with
                    | Some libNum -> Some libNum, other
                    | _ -> sprintf "Not resolved node: %s in %s" other node |> ParseException |> raise
        ]
    ]

    let mutable exported = Map.empty
    for node, a in Seq.rev analyses do
        for e in a.Exported do
            exported <- exported.Add (e, node)
    let implicitImports = [
        for node, a in analyses -> [
            for u in a.Undefined ->
                match exported.TryFind u with
                | Some other -> (fileNum, other), u
                | None ->
                    // Try libraries in order
                    let look (libNum: int, lib: Library) =
                        match lib.ExportedBy u with
                        | Some other -> Some (libNum, other)
                        | None -> None
                    match Seq.indexed libraries
                          |> Seq.skip 1 // Skip root library, which does not support implicit imports.
                          |> Seq.map look |> Seq.tryFind Option.isSome with
                    | Some (Some (libNum, other)) -> (Some libNum, other), u
                    | _ -> sprintf "Not resolved: %s in %s" u node |> ParseException |> raise
        ]
    ]

    let allFileImports =
        seq {
            for ei, ii in Seq.zip explicitImports implicitImports ->
                set (Seq.append ei <| Seq.map fst ii)
        } |> Seq.zip fileNodes |> Map

    let withNum num nodes = Seq.map (fun node -> num, node) nodes
    let next (num, node) = 
        match num with
        | None -> allFileImports.[node] |> seq
        | Some 0 ->
            seq { // The root library
                for other in libraries.[0].Dependencies node ->
                    (if List.contains other fileNodes then fileNum else num), other
            }
        | Some n -> withNum num <| libraries.[n].Dependencies node

    let get (num: int option, node: string) =
        match num with
        | None ->
            let imp ((_, other), sym) = other + "." + sym
            let ((_, str), ii) =
                Seq.zip files implicitImports
                |> Seq.find (fst >> fst >> (=) node)
            node, fun () -> (List.map imp ii, str ())
        | Some n ->
            let lib = libraries.[n]
            node, fun () -> lib.Get node

    findComp (withNum None fileNodes) next
    |> Seq.rev
    |> Seq.map (Seq.rev >> Seq.map get)

let showValue (x: int64) =
    let mutable hex = sprintf "%016X" x
    hex <- Regex.Replace(hex, "^000+", "..00")
    hex <- Regex.Replace(hex, "^FFF+", "..FF")
    sprintf "%d (0x%s)" x hex

// Build one (mutually dependent) component.
let private buildOne (comp: Chunk list) (extSymbols: string -> (bool * int64) option) =
    let program, exported, labels = parseProgram comp extSymbols
    //for line in program do printfn "%O" line

    let bytes, symbols, spacers, relatives, exports = assemble program
    let exp = [ for i in exported -> labels.[i], exports.[i] ]
    {
        Node = fst comp.[0]
        Binary = bytes
        Exported = [ for (l, (r, v)) in exp do if r then yield l, v ]
        Constants = [ for (l, (r, v)) in exp do if not r then yield l, v ]
        Labels = [
            for pair in labels do
                let i = pair.Key
                if i < symbols.Length then
                    let pos = symbols.[i]
                    if pos >= 0 then
                        yield pair.Value, int64 pos
        ]
        Spacers = spacers
        Relatives = relatives
    }

let doBuild (reused: seq<AssemblerOutput>) (buildOrder: seq<seq<Chunk>>) : seq<AssemblerOutput> =
    let mutable currentSize : int64 = 0L
    // Maps each qualified names to its distance from the end [0, currentSize].
    let mutable allSymbols : Map<string, bool * int64> = Map.empty

    let incorporate (ao: AssemblerOutput) =
        currentSize <- currentSize + int64 (Seq.length ao.Binary)
        for qn, pos in ao.Exported do
            allSymbols <- allSymbols.Add (qn, (true, currentSize - pos))
        for qn, value in ao.Constants do
            allSymbols <- allSymbols.Add (qn, (false, value))

    for ao in reused do
        incorporate ao

    let lookup qn =
                    match allSymbols.TryFind qn with
                    | None -> None
                    | Some (true, dist) -> Some (true, currentSize - dist)
                    | Some (false, value) -> Some (false, value)
    seq {
        for comp in buildOrder do
            let ao = buildOne (Seq.toList comp) lookup
            yield ao
            incorporate ao
    }

let doCollect (outputs: seq<AssemblerOutput>): AssemblerOutput =
    let rev = outputs |> Seq.cache |> Seq.rev
    let spacers = [
        let mutable offset = 0
        for ao in rev do
            for pos, size in ao.Spacers do
                yield pos + offset, size
            offset <- offset + Seq.length ao.Binary
    ]
    let mutable offset = 0
    let relatives = [
        for ao in rev do
            for pos in ao.Relatives do
                yield pos + offset
            offset <- offset + Seq.length ao.Binary
    ]
    let initBin = initialization offset spacers relatives
    let binary =
        Seq.append initBin
                   (rev |> Seq.map (fun ao -> ao.Binary) |> Seq.concat)
    let labels = seq {
        let mutable offset = int64 initBin.Length
        for ao in rev do
            for qn, pos in ao.Labels do
                yield qn, pos + offset
            offset <- offset + int64(Seq.length ao.Binary)
    }
    {
        Node=(Seq.head rev).Node
        Binary=Seq.cache binary
        Exported=[]
        Constants=[]
        Labels=Seq.cache labels
        Spacers=[]
        Relatives=[]
    }

// TODO: Build incrementally if "build directory" specified
let doAssemble files libraries =
    try
        let buildOrder = prepareBuild files libraries
        buildOrder |> doBuild [] |> doCollect // 64 KiB stack
    with
        ParseException(msg) -> failwith msg

let doRun binary arg outputDir traceSyms =
    try
        // Memory size: 16 MiB (for now)
        execute (1UL <<< 24) binary arg outputDir traceSyms |> Seq.map int64
    with
        | AccessException msg -> failwith "Access exception!"
        | UndefinedException msg -> failwith "Undefined instruction!"

