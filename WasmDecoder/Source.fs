module Source

type 'a Phrase = {at : int; it : 'a}

let (@@) x region = {it = x; at = region}


(* Positions and regions *)

let noRegion = 0
