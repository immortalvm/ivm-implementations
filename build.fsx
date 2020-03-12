#r "paket:
nuget Fake.IO.FileSystem
nuget Fake.IO.Zip
nuget Fake.DotNet.Cli
nuget Fake.Tools.Git
nuget Fake.Core.Target //"
#load "./.fake/build.fsx/intellisense.fsx"

open Fake.IO
open Fake.IO.Globbing.Operators //enables !! and globbing
open Fake.IO.FileSystemOperators
open Fake.Core
open Fake.DotNet
open Fake.Tools

Target.create "Clean" (fun _ ->
  Trace.log " --- Cleaning stuff --- "
  // Why isn't there a DotNet.clean (yet)?
  let res = DotNet.exec id  "clean" ""
  if not res.OK then failwithf "dotnet clean failed with code %i" res.ExitCode
)

Target.create "Test" (fun _ ->
  Trace.log " --- Testing --- "
  // TODO: Reduce output noise when running tests / running in general
  DotNet.test id "."
)

// Since Git.Information.getLastTag only sees annotated tags
let getLastTag () =
  Git.CommandHelper.getGitResult "." "describe --tags --abbrev=0" |> Seq.head

let publish (alsoZip: bool) (runtimes: string list) =
  let framework = "netcoreapp3.0"
  let tag = getLastTag ()
  let version = tag.Substring 1
  Trace.logfn "Publishing %s for %s..." tag <| System.String.Join(", ", runtimes)

  for runtime in runtimes do
    let releaseDir = "Command" </> "bin" </> "release" </> framework </> runtime
    let publishDir = releaseDir </> "publish"

    Shell.cleanDir <| publishDir

    // Inspired by https://github.com/cannorin/flxble/blob/master/build.fsx
    DotNet.publish (fun options ->
    {
      options with
        Configuration = DotNet.BuildConfiguration.Release
        SelfContained = Some true
        Runtime = Some runtime

        MSBuildParams =
        {
          MSBuild.CliArguments.Create() with
            Properties =
              ("PackAsTool", "false")
              :: ("TargetFramework", framework)
              :: ("PublishSingleFile", "true")
              :: ("PublishTrimmed", "true")
              :: ("AssemblyVersion", version)
              :: options.MSBuildParams.Properties
        }
    }) "Command"

    if alsoZip then
      // Put the executable and .pdb file in one zip file.
      // This overwrites any existing zip file.
      !! (publishDir </> "*") |> Zip.zip publishDir (releaseDir </> sprintf "%s_%s.zip" tag runtime)

Target.create "Publish" <| fun _ ->
  Trace.log " --- Publish and zip for all platforms (after clean and test) --- "
  publish true ["osx-x64"; "linux-x64"; "win-x64"]

open System.Runtime.InteropServices

Target.create "Pub" <| fun _ ->
  Trace.log " --- Publish for current platform --- "
  let runtime =
    if RuntimeInformation.IsOSPlatform(OSPlatform.OSX) then "osx-x64"
    else if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then "win-x64"
    else "linux-x64"
  publish false [runtime]

open Fake.Core.TargetOperators

"Clean" ==> "Publish"
"Clean" ?=> "Test"
"Test" ==> "Publish"

Target.runOrDefault "Publish" // "Publish"
