module Machine.Executor

open Machine.Instructions
open Machine.Utils

open System.Drawing
open System.IO

exception AccessException of string
exception UndefinedException of string

// Little-endian encoding
let private fromBytes : seq<uint8> -> uint64 =
    Seq.mapi (fun i x -> uint64 x <<< i*8) >> Seq.sum

// n = 1,2,4,8
let private toBytes n (x: uint64) : seq<uint8> =
    [| 0 .. n-1 |] |> Seq.map (fun i -> x >>> i*8 |> uint8)

type private Machine
    (
        memorySize: uint64,
        initialProgram: seq<uint8>,
        startLocation: uint64,
        inputDir: string option,
        outputDir: string option,
        traceSyms: Map<int, string> option) =

    let memory =
        try
            let size =
                if memorySize > uint64 System.Int32.MaxValue
                then failwithf "Max memory size: %d" System.Int32.MaxValue
                else int memorySize
            let mem = Array.create size 0uy
            Seq.iteri (fun i x -> mem.[i] <- x) initialProgram
            mem
        with
            :? System.IndexOutOfRangeException -> raise (AccessException "Program too big")

    let memoryIndex location : int =
        let i = location - startLocation
        if i < uint64 memory.Length
        then int i
        else raise (AccessException (sprintf "%d (%d + %d)" location startLocation i))

    let load location = memory.[memoryIndex location]
    let store location value = memory.[memoryIndex location] <- value

    let mutable pendingInputFiles: string[] option = None
    let mutable inputBitmap: Bitmap option = None
    let rec readFrame (i: uint64) : int * int =
        match pendingInputFiles with
        | None ->
            pendingInputFiles <-
                match inputDir with
                | None -> Some [||]
                | Some dir ->
                    Directory.GetFiles dir
                    |> Array.toSeq
                    |> Seq.map Path.GetFileName
                    |> Seq.filter (Path.GetExtension >> (=) ".png")
                    |> Seq.sort
                    |> Seq.map (fun name -> Path.Join (dir, name))
                    |> Seq.toArray
                    |> Some
            readFrame i
        | Some arr when i < uint64 arr.Length ->
            let bitmap = new Bitmap(arr.[int i])
            inputBitmap <- Some bitmap
            (bitmap.Width, bitmap.Height)
        | _ -> (0, 0)
    let rec readPixel x y : uint8 =
        let c = inputBitmap.Value.GetPixel (x, y)
         // Transparent = white, 765 = 255 * 3, 382 ~ 765 / 2
        let g = (int c.R + int c.G + int c.B) * int c.A + 765 * (255 - int c.A)
        (g + 382) / 765 |> uint8

    let mutable outputEncountered = false
    let mutable frameCounter = 0 // Frame 0 will have no bitmap or sound.
    let mutable text : char [] list = []
    let mutable bytes : uint8 list = []
    let mutable bitmap : Bitmap option = None
    let mutable sampleRate = 0u
    let mutable samples : (int16 * int16) list = [] // Reversed
    let flushFrame () =
        if outputDir.IsNone
        then
            if not outputEncountered
            then printfn "Output ignored"
                 outputEncountered <- true
        else
            if not outputEncountered
            then Directory.CreateDirectory outputDir.Value |> ignore
                 printfn "Output to: %s" outputDir.Value
                 outputEncountered <- true
            let path = Path.Combine(outputDir.Value, sprintf "%08d." frameCounter)
            match text with
            | [] -> ()
            | _ -> File.WriteAllText (path + "text", text |> Seq.rev |> Seq.concat |> System.String.Concat )
            match bytes with
            | [] -> ()
            | _ -> File.WriteAllBytes (path + "bytes", bytes |> Seq.rev |> Seq.toArray)
            match bitmap with
            | None -> ()
            | Some b ->
                b.Save(path + "png", Imaging.ImageFormat.Png)
            match samples with
            | [] -> ()
            | _ ->
                // See http://www-mmsp.ece.mcgill.ca/Documents/AudioFormats/WAVE/WAVE.html
                let data = samples |> Seq.rev |> Seq.collect (fun (l, r) -> [l; r]) |> Seq.toArray
                let sampleLength = 2 // 16 bits
                let channels = 2 // left, right
                let blockAlign = sampleLength * channels
                let blocks = Array.length data / channels
                use s = File.OpenWrite <| path + "wav"
                let w = new BinaryWriter (s)
                w.Write("RIFF".ToCharArray ())
                w.Write(uint32 <| 36 + blocks * channels * sampleLength)
                w.Write("WAVEfmt ".ToCharArray ());
                w.Write(uint32 16) // Chunk size
                w.Write(uint16 1) // WAVE_FORMAT_PCM
                w.Write(uint16 channels)
                w.Write(uint32 sampleRate)
                w.Write(uint32 blockAlign * sampleRate)
                w.Write(uint16 <| blockAlign);
                w.Write(uint16 <| 8 * sampleLength);
                w.Write("data".ToCharArray ());
                w.Write(uint32 blockAlign * uint32 blocks) // Chunk size
                for x in data do w.Write(x)
            frameCounter <- frameCounter + 1

    member val Terminated = false with get, set

    // Program counter (next location).
    // Initialized to the start of the initial memory contents.
    member val ProgramCounter = startLocation with get, set

    // Stack pointer (the current top). The stack grows downwards.
    // Initialized to the end of the initial memory contents.
    member val StackPointer = startLocation + uint64 memory.Length with get, set

    // Little-endian, n = 1,2,4,8, not sign-extended
    member m.LoadN n location =
        [| location .. location + uint64(n-1) |]
        |> Seq.map load
        |> fromBytes

    // Little-endian, n = 1,2,4,8
    member m.StoreN n location (value: uint64) =
        toBytes n value
        |> Seq.iteri (fun i x -> store (location + uint64 i) x)

    member m.NextOp n =
        let op = m.LoadN n m.ProgramCounter
        m.ProgramCounter <- m.ProgramCounter + uint64 n
        op

    // Only 64-bit values on stack!
    member m.Pop () =
        let result = m.LoadN 8 m.StackPointer
        m.StackPointer <- m.StackPointer + 8UL
        result

    member m.Push value =
        m.StackPointer <- m.StackPointer - 8UL
        m.StoreN 8 m.StackPointer value

    member m.Run () =
        if traceSyms.IsSome
        then
            printfn "\nInitial program"
            initialProgram
            |> Seq.map (sprintf "%3d")
            |> Seq.chunkBySize 5
            |> Seq.map (String.concat " ")
            |> Seq.chunkBySize 4
            |> Seq.map (String.concat "   ")
            |> Seq.iteri (fun i x -> printfn "%4d-..  %s" (i * 20) x)
            printfn ""

        let mutable counter = 0
        try
            while not m.Terminated do

                if traceSyms.IsSome
                then
                    let pc = m.ProgramCounter
                    match traceSyms.Value.TryFind <| int (pc - startLocation) with
                    | Some name -> printfn "-- %s --" name
                    | None -> ()
                    let op = m.LoadN 1 pc
                    let name = Machine.Disassembler.instructionNames.[int op]
                    printf "%4d: %-12s" pc name

                m.Step ()
                counter <- counter + 1

                if traceSyms.IsSome
                then
                    printfn " %4d: %s" m.StackPointer
                    <| System.String.Join(" ", m.Stack 50 |> Seq.rev |> Seq.map int64)

            flushFrame ()
        finally
            printfn "\nExecution steps: %d\n" counter

    // For testing
    member m.Stack ?max : seq<uint64> =
        let safeGet i =
            try
                m.StackPointer + uint64 (i*8) |> m.LoadN 8 |> Some
            with
                | AccessException msg -> None
        let limit = valueOr 1000 max
        seq {
            let mutable i = 0
            while i < limit do
                match safeGet i with
                | Some xx -> yield xx; i <- i + 1
                | None -> i <- limit
        }

    // This is the only method which references the instruction set!
    member m.Step () =
        match m.NextOp 1 |> int8 with
        | EXIT -> m.Terminated <- true
        | NOP -> ()

        | JUMP -> m.ProgramCounter <- m.Pop ()
        | JUMP_ZERO ->
            let offset = m.NextOp 1 |> signExtend1
            if m.Pop () = 0UL
            then m.ProgramCounter <- m.ProgramCounter + offset
        | SET_SP -> m.StackPointer <- m.Pop ()
        | GET_PC -> m.ProgramCounter |> m.Push
        | GET_SP -> m.StackPointer |> m.Push

        | PUSH0 -> 0UL |> m.Push
        | PUSH1 -> m.NextOp 1 |> m.Push
        | PUSH2 -> m.NextOp 2 |> m.Push
        | PUSH4 -> m.NextOp 4 |> m.Push
        | PUSH8 -> m.NextOp 8 |> m.Push

        | SIGX1 -> m.Pop () |> signExtend1 |> m.Push
        | SIGX2 -> m.Pop () |> signExtend2 |> m.Push
        | SIGX4 -> m.Pop () |> signExtend4 |> m.Push

        | LOAD1 -> m.Pop () |> m.LoadN 1 |> m.Push
        | LOAD2 -> m.Pop () |> m.LoadN 2 |> m.Push
        | LOAD4 -> m.Pop () |> m.LoadN 4 |> m.Push
        | LOAD8 -> m.Pop () |> m.LoadN 8 |> m.Push

        // Address on top!
        | STORE1 -> m.StoreN 1 (m.Pop ()) (m.Pop ())
        | STORE2 -> m.StoreN 2 (m.Pop ()) (m.Pop ())
        | STORE4 -> m.StoreN 4 (m.Pop ()) (m.Pop ())
        | STORE8 -> m.StoreN 8 (m.Pop ()) (m.Pop ())

        | ADD -> m.Pop () + m.Pop () |> m.Push
        | MULT -> m.Pop () * m.Pop () |> m.Push
        | DIV ->
            let x, y = m.Pop (), m.Pop ()
            if x = 0UL then 0UL else y / x
            |> m.Push
        | REM ->
            let x, y = m.Pop (), m.Pop ()
            if x = 0UL then 0UL else y % x
            |> m.Push
        | LE -> (if m.Pop () >= m.Pop () then -1 else 0) |> uint64 |> m.Push

        | AND -> (m.Pop ()) &&& (m.Pop ()) |> m.Push
        | OR -> (m.Pop ()) ||| (m.Pop ()) |> m.Push
        | NOT -> ~~~ (m.Pop ()) |> m.Push
        | XOR -> (m.Pop ()) ^^^ (m.Pop ()) |> m.Push

        | POW2 ->
            let n = m.Pop()
            if n <= 63UL then 1UL <<< int n else 0UL
            |> m.Push

        | READ_FRAME ->
            let width, height = m.Pop () |> readFrame
            width |> uint64 |> m.Push
            height |> uint64 |> m.Push
        | READ_PIXEL ->
            let y = m.Pop () |> int
            let x = m.Pop () |> int
            readPixel x y |> uint64 |> m.Push
        | NEW_FRAME ->
            flushFrame ()
            sampleRate <- m.Pop () |> uint32
            samples <- []
            let height = m.Pop () |> int
            let width = m.Pop () |> int
            bitmap <- Some <| new Bitmap (width, height)
            System.Console.Error.Write "\r\f" // carriage return + form feed
        | SET_PIXEL ->
            let b = m.Pop () |> int
            let g = m.Pop () |> int
            let r = m.Pop () |> int
            let y = m.Pop () |> int
            let x = m.Pop () |> int
            let c = Color.FromArgb(r &&& 255, g &&& 255, b &&& 255)
            bitmap.Value.SetPixel(x, y, c)
        | ADD_SAMPLE ->
            let right = m.Pop () |> int16
            let left = m.Pop () |> int16
            samples <- (left, right) :: samples
        | PUT_CHAR ->
            let c =
                m.Pop ()
                |> toBytes 4
                |> Seq.toArray
                |> System.Text.Encoding.UTF32.GetChars
            text <- c :: text
            System.Console.Error.Write c
        | PUT_BYTE ->
            bytes <- uint8 (m.Pop ()) :: bytes

        | undefined -> raise (UndefinedException(sprintf "%d" undefined))


let private random = System.Random ()

// Execute at random location (multiple of 1000) and return terminal stack.
// Trace execution if symbol mapping is provided.
// This method is likely to change...
let execute (memorySize: uint64) (prog: seq<uint8>) (arg: seq<uint8>)
            (inputDir: string option) (outputDir: string option)
            (traceSyms: Map<int, string> option) (loc: uint64 option) =
    let start = if loc.IsSome then loc.Value
                // Otherwise Start at 0, 1000, ... or 7000.
                else random.Next () % 8 |> ( * ) 1000 |> uint64
    let cachedArg = Seq.cache arg
    let machine =
        Machine(
            memorySize,
            Seq.concat [prog; Seq.length cachedArg |> uint64 |> toBytes 8; cachedArg],
            start,
            inputDir,
            outputDir,
            traceSyms)
    machine.Run ()
    machine.Stack ()
