module Machine.Disassembler

open System.Reflection

let instructionNames : string[] =
    let t = typeof<Machine.Instructions.IReflectionMarker>.DeclaringType
    let toPair (field: FieldInfo) : string * sbyte =
        field.Name, downcast field.GetValue None
    let pairs = t.GetFields(BindingFlags.Public ||| BindingFlags.Static)
                |> Seq.map toPair
    let result = Array.create 256 "-"
    for (key, value) in pairs do
        result.[value |> uint8 |> int] <- key
    result
