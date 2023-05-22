open Fake.IO
open Fake.IO.Globbing.Operators //enables !! and globbing
open Fake.IO.FileSystemOperators
open Fake.Core
open Fake.DotNet
open Fake.Tools
open Fake.Core.TargetOperators

open System.Runtime.InteropServices


[<EntryPoint>]
let main argv =
    argv
    |> Array.toList
    |> Context.FakeExecutionContext.Create false "build.fs"
    |> Context.RuntimeContext.Fake
    |> Context.setExecutionContext

    Target.initEnvironment()

    // From https://fake.build/guide/dotnet-cli.html#Minimal-working-example
    let install = lazy DotNet.install DotNet.Versions.FromGlobalJson
    let inline dotnetSimple arg = DotNet.Options.lift install.Value arg

    Target.create "Clean" (fun _ ->
        let res = DotNet.exec dotnetSimple "clean" "" // "ivm-implementations.sln"
        if not res.OK then failwithf "'dotnet clean' failed with code %i" res.ExitCode
    )

    Target.create "Test" (fun _ ->
        // TODO: Reduce output noise when running tests / running in general
        DotNet.test dotnetSimple "."
    )

    // Since Git.Information.getLastTag only sees annotated tags
    let getLastTag () =
        Git.CommandHelper.getGitResult "." "describe --tags --abbrev=0" |> Seq.head

    let publish (alsoZip: bool) (runtimes: string list) =
        let framework = "net7.0"
        let tag = getLastTag ()
        let version = tag.Substring 1
        Trace.log $"Publishing {tag}..."

        for runtime in runtimes do
        let releaseDir = "Command" </> "bin" </> "release" </> framework </> runtime
        let publishDir = releaseDir </> "publish"

        Shell.cleanDir publishDir

        // Inspired by https://github.com/cannorin/flxble/blob/master/build.fsx
        let opt (options: DotNet.PublishOptions) =
            {
                options with
                    Configuration = DotNet.BuildConfiguration.Release
                    SelfContained = Some true
                    Runtime = Some runtime

                    MSBuildParams =
                    {
                        options.MSBuildParams with
                            // DisableInternalBinLog = true // https://gitter.im/fsharp/FAKE?at=5e6a729be203784a55a350db
                            Properties =
                                ("PackAsTool", "false")
                                :: ("TargetFramework", framework)
                                :: ("PublishSingleFile", "true")
                                :: ("PublishTrimmed", "true")
                                :: ("AssemblyVersion", version)
                                :: options.MSBuildParams.Properties
                    }
            }
        DotNet.publish (dotnetSimple >> opt) "Command"

        if alsoZip then
            // Put the executable and .pdb file in one zip file.
            // This overwrites any existing zip file.
            !! (publishDir </> "*") |> Zip.zip publishDir (releaseDir </> sprintf "%s_%s.zip" tag runtime)

    // Publish and zip for all platforms (after clean and test)
    Target.create "Publish" <| fun _ ->
        publish true ["osx-x64"; "linux-x64"; "win-x64"]

    // Publish for current platform
    Target.create "Pub" <| fun _ ->
        let runtime =
            if RuntimeInformation.IsOSPlatform(OSPlatform.OSX) then "osx-x64"
            else if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then "win-x64"
            else "linux-x64"
        publish false [runtime]

    ignore <| [
        "Clean" ==> "Publish"
        "Clean" ?=> "Test"
        "Test" ==> "Publish"
    ]

    Target.runOrDefaultWithArguments "Publish"
    0
