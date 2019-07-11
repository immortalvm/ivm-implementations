module Machine.Disassembler

open System.Reflection

let instructionNames : string[] =
    let t = typeof<Machine.Instructions.ReflectionMarker>.DeclaringType
    let toPair (field: FieldInfo) : string * sbyte =
        field.Name, downcast field.GetValue None
    let pairs = t.GetFields(BindingFlags.Public ||| BindingFlags.Static)
                |> Seq.map toPair
    let n = pairs |> Seq.map snd |> Seq.max |> int |> (+) 1
    let result = Array.create n ""
    for (key, value) in pairs do
        result.[int value] <- key
    result
