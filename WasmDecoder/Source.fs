module Source

type Pos = {file : string; line : int; column : int}
type Region = {left : Pos; right : Pos}
type 'a Phrase = {at : Region; it : 'a}

let (@@) x region = {it = x; at = region}


(* Positions and regions *)

let noPos = {file = ""; line = 0; column = 0}
let noRegion = {left = noPos; right = noPos}
