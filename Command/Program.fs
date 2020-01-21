open System.IO

open System.CommandLine
open System.CommandLine.Invocation
open System.CommandLine.Builder

open Tools.Interface
open Tools.Checks

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

    let sources = Argument<FileInfo[]>("source files", Description="Names of source files (<name>.s)", Arity=ArgumentArity.OneOrMore).ExistingOnly()
    let root = argOpt "--root" "Name of source root directory (default: none)" (dirArg "root" null) |> alias "-r"
    let trace = opt "--trace" "Turn on trace output" |> alias "-t"
    let arg = argOpt "--arg" "Specify argument file (default: none)" <| (fileArg "argument file" null).ExistingOnly()
    let out = argOpt "--out" "Specify output directory (default: none)" <| dirArg "output directory" null

    // TODO: Allow multiple libraries.
    let library = argOpt "--library" "Specify a library" <| (fileArg "library" null).ExistingOnly() |> alias "-l"

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
                root; sources; library
            ] <| CommandHandler.Create(fun bin sym root ``source files`` library ->
                    assem (fNames ``source files``) (oName root) (Option.toList <| oName library) (oName bin) (oName sym))

            com "run" "Execute binary" [
                trace; arg; out
                (fileArg "binary file" "Name of binary file").ExistingOnly()
            ] <| CommandHandler.Create(fun trace arg out ``binary file`` ->
                    run (fName ``binary file``) (oName arg) (oName out) trace)

            com "as-run" "Assemble and run" [
                trace; arg; out
                root; sources; library
            ] <| CommandHandler.Create(fun trace arg out root ``source files`` library ->
                    asRun (fNames ``source files``) (oName root) (Option.toList <| oName library) (oName arg) (oName out) trace)

            com "check" "Assemble, run and check final stack" [
                trace;
                root; sources; library
            ] <| CommandHandler.Create(fun trace root ``source files`` library ->
                    doCheck (fNames ``source files``) (oName root) (Option.toList <| oName library) trace |> printfn "%s")

            com "lib" "Create library" [
                Argument<DirectoryInfo>("directory", Description="Root directory of the library files")
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
        CommandLineBuilder(rootCommand).Build().Invoke argv
    with
        :? System.Reflection.TargetInvocationException as e ->
            printfn "%O" <| e.InnerException.Message; 1
