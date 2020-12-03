module Tools.Tests.ZipLibraryTests

open System.IO
open Expecto
open Expecto.Impl
open Swensen.Unquote

open Tools.Helpers

[<Literal>]
let DIRECTORY = "test_code"

let createAndReadLibrary () =
    let filename = Path.GetTempFileName ()
    dirToZipLib DIRECTORY filename
    let lib = libFromZip filename
    Expect.isTrue (lib.Contains "test_linking1") "Source file missing"
    Expect.equal (lib.ExportedBy "label3c") (Some "test_linking3") "Missing export"
    Expect.sequenceEqual (lib.Dependencies "test_circular2" |> Set.toList) ["test_circular1"] "Wrong dependencies"
    let implImps, str = lib.Get "test_circular1"
    Expect.sequenceEqual implImps [] "Wrong implicit imports"
    let mutable actualSize = 0L
    while str.ReadByte () >= 0 do
        actualSize <- actualSize + 1L
    str.Close ()
    let expectedSize = (FileInfo <| Path.Combine (DIRECTORY, "test_circular1.s")).Length
    Expect.equal actualSize expectedSize "Wrong file size"

[<Tests>]
let zipLibraryTests =
    testList "zipLibraryTests" [
        testCase "Create and read library" createAndReadLibrary
    ]
