module Assembler.Namespace

[<Literal>]
let NODE_SEPARATOR = '/'

let splitNodeString (node: string) =
    node.Split NODE_SEPARATOR

let nodeJoin (ids: string seq) = System.String.Join(NODE_SEPARATOR, ids)

let nodeString ids = nodeJoin <| Seq.take (Seq.length ids - 1) ids

let nodeId ids = nodeString ids, Seq.last ids

let unqualify (qualifiedName: string) =
    let ids = splitNodeString qualifiedName
    ids.[ids.Length - 1]

let qnNode (qualifiedName: string) =
    qualifiedName |> splitNodeString |> nodeString
