module Buffer

open System.IO
open System.Text

type Buffer = MemoryStream

let create (n: int) = new MemoryStream(n)

let write (b: Buffer) (a: byte array) = b.Write(a, 0, a.Length)

let contents (b: Buffer) : byte array =
    let a = b.ToArray ()
    b.Dispose ()
    a
