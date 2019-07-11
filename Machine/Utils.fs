module Machine.Utils

let signExtend1 : uint64 -> uint64 = uint8 >> int8 >> uint64
let signExtend2 : uint64 -> uint64 = uint16 >> int16 >> uint64
let signExtend4 : uint64 -> uint64 = uint32 >> int32 >> uint64

// TODO: There should be a built-in function like this.
let valueOr<'a> (def: 'a) (x: 'a option) = match x with Some z -> z | _ -> def

let mapGet<'a, 'b when 'a: comparison> (m: Map<'a,'b>) (x: 'a) (def: 'b) =
    m.TryFind x |> valueOr def
