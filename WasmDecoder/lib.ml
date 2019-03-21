module Fun =
struct
  let uncurry f (x, y) = f x y
end


module String =
struct
  let implode cs =
    let buf = Buffer.create 80 in
    List.iter (Buffer.add_char buf) cs;
    Buffer.contents buf

  let explode (s: string) =
    let cs = ref [] in
    for i = s.Length - 1 downto 0 do cs := s.[i] :: !cs done;
    !cs
end


module List =
struct
  let rec map_filter f = function
    | [] -> []
    | x::xs ->
      match f x with
      | None -> map_filter f xs
      | Some y -> y :: map_filter f xs
end


(* Are these functions inteded for lists of signed or unsigned integers? *)
module List32 =
struct
  let rec make n x = make' n x []
  and make' (n: uint32) x xs =
    if n = 0ul then xs else make' (n - 1ul) x (x::xs)

  let rec length xs = length' xs 0l
  and length' xs n =
    match xs with
    | [] -> n
    | _::xs' when n < (1 <<< 30) -> length' xs' (n + 1)
    | _ -> failwith "length"

  let rec nth xs n =
    match n, xs with
    | 0l, x::_ -> x
    | n, _::xs' when n > 0l -> nth xs' (n - 1)
    | _ -> failwith "nth"

  let rec take n xs =
    match n, xs with
    | 0l, _ -> []
    | n, x::xs' when n > 0l -> x :: take (n - 1) xs'
    | _ -> failwith "take"

  let rec drop n xs =
    match n, xs with
    | 0l, _ -> xs
    | n, _::xs' when n > 0l -> drop (n - 1) xs'
    | _ -> failwith "drop"
end


module Array32 =
struct
  let make n x =
    if n < 0l || n > (1 <<< 30) then
      raise (System.ArgumentException("Array32.make"));
    Array.create n x

  let length a = Array.length a

  let index_of_int32 i =
    if i < 0l || i > (1 <<< 30) then -1 else
    i

  let get a i = Array.get a (index_of_int32 i)
  let set a i x = Array.set a (index_of_int32 i) x
  let blit a1 i1 a2 i2 n =
    Array.blit a1 (index_of_int32 i1) a2 (index_of_int32 i2) (index_of_int32 n)
end


module Option =
struct
  let get o x =
    match o with
    | Some y -> y
    | None -> x

  let map f = function
    | Some x -> Some (f x)
    | None -> None

  let app f = function
    | Some x -> f x
    | None -> ()
end
