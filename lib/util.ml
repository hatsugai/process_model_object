let rec cartesian_product xss =
  match xss with
    [] -> [[]]
  | xs::xss' ->
     List.fold_left
       (fun acc ys ->
         List.fold_left
           (fun acc x -> (x::ys)::acc)
           acc xs)
       [] (cartesian_product xss')

let generate_id () = ref ()

let equal_id x y = (x == y)
