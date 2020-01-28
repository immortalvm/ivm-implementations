open System.IO

open System.CommandLine
open System.CommandLine.Invocation
open System.CommandLine.Builder

open Tools.Interface
open Tools.Checks
open System.CommandLine.Parsing

open System.Runtime.CompilerServices

// Based on ExistingOnly extension method in System.CommandLine.ArgumentExtensions.
// Presumably needed because F# covariance rules are more strict than those of C#.
[<Extension>]
type Ext() =
    [<Extension>]
    static member inline ExistingOnly(argument: Argument<seq<FileInfo>>) =
        argument.AddValidator(fun a ->
            a.Tokens
            |> Seq.map (fun t -> t.Value)
            |> Seq.filter (fun filePath -> not <| File.Exists filePath)
            |> Seq.map (sprintf "File does not exist: %s")
            |> Seq.tryHead
            |> Option.toObj)
        argument


[<EntryPoint>]
let main argv =
    let extend (c: Command) (sl: Symbol List) : Command =
        for x in sl do c.Add(x)
        c

    let com (name: string) (descr: string) (sl: Symbol List) (handler: ICommandHandler) : Command =
        extend (Command(name, Description=descr, Handler=handler)) sl

    let strArg (name: string) (descr: string) = Argument<string>(name, Description=descr)
    let fileArg (name: string) (descr: string) = Argument<FileInfo>(name, Description=descr)
    let dirArg (name: string) (descr: string) = Argument<DirectoryInfo>(name, Description=descr, Arity=ArgumentArity.ZeroOrOne)

    let opt (name: string) (descr: string) : Option =
        Option(name, Description=descr)

    let argOpt (name: string) (descr: string) (a: Argument<'t>): Option<'t> =
        assert name.StartsWith "--"
        Option<'t>(name, Argument=a, Description=descr)

    let alias(name: string) (opt: Option) =
        assert (name.StartsWith "-" && not(name.StartsWith "--"))
        opt.AddAlias(name)
        opt

    let sources = Argument<seq<FileInfo>>("source files", Description="Names of source files (<name>.s)", Arity=ArgumentArity.OneOrMore).ExistingOnly()
    let root = argOpt "--root" "Name of source root directory (default: none)" (dirArg "root" null) |> alias "-r"
    let entry = argOpt "--entry" "Name of entry point (default: none, suggestion: main)" (strArg "entry" null) |> alias "-e"
    let trace = opt "--trace" "Turn on trace output" |> alias "-t"
    let arg = argOpt "--arg" "Specify argument file (default: none)" <| (fileArg "argument file" null).ExistingOnly()
    let out = argOpt "--out" "Specify output directory (default: none)" <| dirArg "output directory" null

    // TODO: Allow multiple libraries.
    let library = argOpt "--library" "Specify a library" <| (fileArg "library" null).ExistingOnly() |> alias "-l"

    // Observe that ~ etc. should be expanded by the shell and not here.
    let fName (fsi: FileSystemInfo) : string = fsi.FullName
    let fNames (fsis: seq<FileSystemInfo>) : string list =
        if fsis = null then []
        else Seq.map (fun (fsi: FileSystemInfo) -> fsi.FullName) fsis |> Seq.toList
    let oName (fsi: FileSystemInfo) : string option =
        if fsi = null then None else Some fsi.FullName

    let rootCommand =
        extend (RootCommand("ivm", Description="iVM Assembler and VM")) [

            com "as" "Assemble source files" [
                (argOpt "--bin" "Specify output binary file (default: <name>.b)" <| fileArg "binary file" null)
                (argOpt "--sym" "Specify output symbol file (default: <name>.sym)" <| fileArg "symbol file" null)
                entry; root; sources; library
            ] <| CommandHandler.Create(fun bin sym entry root ``source files`` library ->
                    assem (fNames ``source files``) (oName root) (Option.toList <| oName library) entry (oName bin) (oName sym))

            com "run" "Execute binary" [
                trace; arg; out
                (fileArg "binary file" "Name of binary file").ExistingOnly()
            ] <| CommandHandler.Create(fun trace arg out ``binary file`` ->
                    run (fName ``binary file``) (oName arg) (oName out) trace)

            com "as-run" "Assemble and run" [
                trace; arg; out
                entry; root; sources; library
            ] <| CommandHandler.Create(fun trace arg out entry root ``source files`` library ->
                    asRun (fNames ``source files``) (oName root) (Option.toList <| oName library) entry (oName arg) (oName out) trace)

            com "check" "Assemble, run and check final stack" [
                trace;
                entry; root; sources; library
            ] <| CommandHandler.Create(fun trace entry root ``source files`` library ->
                    doCheck (fNames ``source files``) (oName root) (Option.toList <| oName library) entry trace |> printfn "%s")

            com "lib" "Create library" [
                Argument<DirectoryInfo>("directory", Description="Root directory of the library files").ExistingOnly()
                fileArg "library" "Library filename"
            ] <| CommandHandler.Create(fun directory library ->
                    createLibrary (fName directory) (fName library))
        ]
    try
        let version =
            let assembly = System.Reflection.Assembly.GetExecutingAssembly()
            let v = assembly.GetName().Version
            sprintf "v%d.%d" v.Major v.Minor
        printfn "iVM Assembler and VM, %s" version
        CommandLineBuilder(rootCommand).UseHelp().Build().Invoke argv
    with
        :? System.Reflection.TargetInvocationException as e ->
            printfn "%O" <| e.InnerException.Message; 1
