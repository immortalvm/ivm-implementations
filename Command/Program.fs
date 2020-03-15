open System.IO

open System.CommandLine
open System.CommandLine.Invocation
open System.CommandLine.Builder

open Tools.Interface
open Tools.Checks
open System.CommandLine.Parsing
open System.CommandLine.Binding

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
            |> Seq.filter (File.Exists >> not)
            |> Seq.map (sprintf "File does not exist: %s")
            |> Seq.tryHead
            |> Option.toObj)
        argument

// Needed because Func<...> stops at 7 arguments.
type AsRunDelegate = delegate of string * bool * FileInfo
                                 * DirectoryInfo * DirectoryInfo * string option * bool
                                 * DirectoryInfo * seq<FileSystemInfo>
                                 * seq<FileSystemInfo> -> int

[<EntryPoint>]
let main argv =
    let extend (c: Command) (sl: Symbol List) : Command =
        for x in sl do c.Add(x)
        c

    let com (name: string) (descr: string) (sl: Symbol List) (handler: ICommandHandler) : Command =
        extend (Command(name, Description=descr, Handler=handler)) sl

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
    let entry = argOpt "--entry" "Name of entry point (default: none, suggestion: main)" <| Argument<string>("entry") |> alias "-e"
    let noopt = opt "--noopt" "Do not optimize" |> alias "-n"
    let mem = argOpt "--memory" "Specify the total VM memory in bytes (default: 16Mi)" <| Argument<string>("memory") |> alias "-m"
    let trace = opt "--trace" "Turn on trace output" |> alias "-t"
    let arg = argOpt "--arg" "Specify argument file (default: none)" <| (fileArg "argument file" null).ExistingOnly() |> alias "-a"
    let inp = argOpt "--inp" "Specify input directory (default: none)" <| dirArg "input directory" null |> alias "-i"
    let out = argOpt "--out" "Specify output directory (default: none)" <| dirArg "output directory" null |> alias "-o"

    // NB. Library options must come after source file arguments.
    let libraries = argOpt "--library" "Specify one or more libraries" <| (Argument<seq<FileInfo>>("libraries")).ExistingOnly() |> alias "-l"

    // Observe that ~ etc. should be expanded by the shell and not here.
    let fName (fsi: FileSystemInfo) : string = fsi.FullName
    let fNames (fsis: seq<FileSystemInfo>) : string list =
        if isNull fsis then []
        else Seq.map (fun (fsi: FileSystemInfo) -> fsi.FullName) fsis |> Seq.toList
    let oName (fsi: FileSystemInfo) : string option =
        if isNull fsi then None else Some fsi.FullName

    let oMem (m: string): uint64 option =
        if isNull m then None else System.Decimal.Parse(m) |> uint64 |> Some

    let rootCommand =
        extend (RootCommand("ivm", Description="iVM Assembler and VM")) [

            com "as" "Assemble source files" [
                (argOpt "--bin" "Specify output binary file (default: <name>.b)" <| fileArg "binary file" null)
                (argOpt "--sym" "Specify output symbol file (default: <name>.sym)" <| fileArg "symbol file" null)
                entry; noopt; root; sources; libraries
            ] <| CommandHandler.Create(fun bin sym entry noopt root ``source files`` library ->
                    assem (fNames ``source files``) (oName root) (Option.toList <| oName library) entry (oName bin) (oName sym) noopt)

            com "run" "Execute binary" [
                mem; trace; arg; inp; out
                (fileArg "binary file" "Name of binary file").ExistingOnly()
            ] <| CommandHandler.Create(fun memory trace arg inp out ``binary file`` ->
                    run (oMem memory) (fName ``binary file``) (oName arg) (oName inp) (oName out) trace)

            com "as-run" "Assemble and run" [
                mem; trace; arg; out
                entry; noopt; root; sources; libraries
            ] <| CommandHandler.Create(new AsRunDelegate(fun memory trace arg inp out entry noopt root ``source files`` library ->
                         asRun (fNames ``source files``) (oName root) (fNames library) entry (oMem memory) (oName arg) (oName inp) (oName out) trace noopt))

            com "check" "Assemble, run and check final stack" [
                mem; trace;
                entry; noopt; root; sources; libraries
            ] <| CommandHandler.Create(fun memory trace entry noopt root ``source files`` library ->
                    doCheck (fNames ``source files``) (oName root) (Option.toList <| oName library) entry (oMem memory) trace noopt |> printfn "%s")

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
