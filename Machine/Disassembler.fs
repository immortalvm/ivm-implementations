module Machine.Disassembler

open System.Reflection

let instructionNames : string[] =
    let t = typeof<Machine.Instructions.IReflectionMarker>.DeclaringType
    let result = Array.create 256 "-"
    for (key, value) in
            t.GetFields(BindingFlags.Public ||| BindingFlags.Static)
            |> Seq.map (fun field -> field.Name, field.GetValue None)
            |> Seq.where (fun (_, value) -> value :? sbyte)
            |> Seq.map (fun (name, value) -> name, value :?> sbyte |> uint8 |> int) do
        result.[value] <- key
    result
