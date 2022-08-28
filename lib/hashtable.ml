type ('k, 'v) ent =
  Nil
| Node of
    {
      mutable link : ('k, 'v) ent;
      key : 'k;
      mutable value : 'v;
    }

type ('k, 'v) t = {
    equal : 'k -> 'k -> bool;
    hash : 'k -> int;
    mutable v : ('k, 'v) ent array;
    mutable count : int;
}

let create equal hash size =
  {
    equal; hash;
    v = Array.make size Nil;
    count = 0
  }

let length ht = ht.count

let normal_hash ht k =
  let hv = ht.hash k in
  if hv >= 0 then
    hv
  else if hv = Int.min_int then
    1
  else
    -hv

let find_opt ht k =
  let i = (normal_hash ht k) mod (Array.length ht.v) in
  let rec loop p =
    match p with
      Nil -> None
    | Node { link; key; value } ->
       if ht.equal key k then
         Some value
       else
         loop link
  in
  loop ht.v.(i)

(* return false iff added as a new entry *)
let replace_r ht k v =
  let i = (normal_hash ht k) mod (Array.length ht.v) in
  let rec loop p =
    match p with
      Nil ->
       ht.v.(i) <- Node { link = ht.v.(i); key = k; value = v };
       ht.count <- ht.count + 1;
       false
    | Node r ->
       if ht.equal r.key k then
         (r.value <- v; true)
       else
         loop r.link
  in
  loop ht.v.(i)

let replace ht k v = let _ = replace_r ht k v in ()

let%test _ =
  let ht = create (=) Hashtbl.hash 10 in
  match find_opt ht 0 with
    None -> true
  | Some _ -> false

let%test _ =
  let ht = create (=) Hashtbl.hash 10 in
  replace ht 0 3;
  match find_opt ht 0 with
    None -> false
  | Some x -> x=3

let%test _ =
  let ht = create (=) Hashtbl.hash 10 in
  replace ht 0 3;
  replace ht 0 4;
  match find_opt ht 0 with
    None -> false
  | Some x -> x=4

let add_if_not_exists ht k v =
  let i = (normal_hash ht k) mod (Array.length ht.v) in
  let rec loop p =
    match p with
      Nil ->
       ht.v.(i) <- Node { link = ht.v.(i); key = k; value = v };
       ht.count <- ht.count + 1;
       true
    | Node r ->
       if ht.equal r.key k then
         false
       else
         loop r.link
  in
  loop ht.v.(i)

let%test _ =
  let ht = create (=) Hashtbl.hash 10 in
  let b = add_if_not_exists ht 0 3 in
  match find_opt ht 0 with
    None -> false
  | Some x -> b && x=3

let%test _ =
  let ht = create (=) Hashtbl.hash 10 in
  replace ht 0 3;
  let b = add_if_not_exists ht 0 4 in
  match find_opt ht 0 with
    None -> false
  | Some x -> not b && x=3

let iter f ht =
  let rec loop p =
    match p with
      Nil -> ()
    | Node { key; value; link } ->
       f key value; loop link
  in
  let n = Array.length ht.v in
  for i=0 to n-1 do
    loop ht.v.(i)
  done

let fold f ht acc =
  let rec loop acc p =
    match p with
      Nil -> acc
    | Node { key; value; link } ->
       loop (f key value acc) link
  in
  let rec loopv acc i =
    if i=0 then
      acc
    else
      loopv (loop acc ht.v.(i-1)) (i-1)
  in
  loopv acc (Array.length ht.v)
