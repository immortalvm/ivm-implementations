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
    let dirArg (name: string) (descr: string) = Argument<DirectoryInfo>(name, Description=descr)

    let opt (name: string) (descr: string) : Option =
        Option(name, Description=descr)

    let argOpt (name: string) (descr: string) (a: Argument<'t>): Option<'t> =
        assert name.StartsWith "--"
        Option<'t>(name, Argument=a, Description=descr)

    let alias(name: string) (opt: Option) =
        assert (name.StartsWith "-" && not(name.StartsWith "--"))
        opt.AddAlias(name)
        opt

    let source = (fileArg "source file" "Name of source file (<name>.s)").ExistingOnly()
    let trace = opt "--trace" "Turn on trace output" |> alias "-t"
    let arg = argOpt "--arg" "Specify argument file (default: none)" <| (fileArg "argument file" null).ExistingOnly()
    let out = argOpt "--out" "Specify output directory (default: none)" <| dirArg "output directory" null

    let fName (fsi: FileSystemInfo) : string = fsi.FullName
    let oName (fsi: FileSystemInfo) : string option =
        if fsi = null then None else Some fsi.FullName

    let root =
        extend (RootCommand("ivm", Description="iVM Assembler and VM")) [

            com "as" "Assemble source file" [
                (argOpt "--bin" "Specify output binary file (default: <name>.b)" <| fileArg "binary file" null)
                (argOpt "--sym" "Specify output symbol file (default: <name>.sym)" <| fileArg "symbol file" null)
                source
            ] <| CommandHandler.Create(fun bin sym ``source file`` ->
                    assem (fName ``source file``) (oName bin) (oName sym))

            com "run" "Execute binary" [
                trace; arg; out
                (fileArg "binary file" "Name of binary file").ExistingOnly()
            ] <| CommandHandler.Create(fun trace arg out ``binary file`` ->
                    run (fName ``binary file``) (oName arg) (oName out) trace)

            com "as-run" "Assemble and run" [
                trace; arg; out
                source
            ] <| CommandHandler.Create(fun trace arg out ``source file`` ->
                    asRun (fName ``source file``) (oName arg) (oName out) trace)

            com "check" "Assemble, run and check final stack" [
                trace;
                source
            ] <| CommandHandler.Create(fun trace ``source file`` ->
                    doCheck (fName ``source file``) trace |> printfn "%s")

        ]
    try
        let version =
            let assembly = System.Reflection.Assembly.GetExecutingAssembly()
            let v = assembly.GetName().Version
            sprintf "v%d.%d" v.Major v.Minor
        printfn "iVM Assembler and VM, %s" version
        root.Invoke(argv)
    with
        Failure msg -> printfn "%s" msg; 1
