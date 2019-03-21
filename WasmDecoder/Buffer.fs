module Buffer

open System.IO
open System.Text

type Buffer = MemoryStream

let create (n: int) = new MemoryStream(n)

let write (b: Buffer) (a: byte array) = b.Write(a, 0, a.Length)

let add_string (b: Buffer) (s: string) = Encoding.Default.GetBytes s |> write b

let add_char (b: Buffer) (c: char) = Encoding.Default.GetBytes [|c|] |> write b

let contents (b: Buffer) : byte array =
    let a = b.ToArray ()
    b.Dispose ()
    a
