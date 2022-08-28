open Type

let f channel_to_event_list p n =

  let ht = Hashtable.create equal_state hash_state n in
  let que = Queue.create () in

  let add s path =
    let id = Hashtable.length ht in
    let b = Hashtable.add_if_not_exists ht s (id, []) in
    if b then Queue.add (s, id, path) que
  in

  let rec loop () =
    if Queue.is_empty que then () else
      let (p, id, path) = Queue.take que in
      let trans_list = transitions p in
      let ts =
        List.fold_left
          (fun acc trans ->
            match trans with
              T.Tau q ->
               add q ((p, E.Tau)::path);
               (E.Tau, q)::acc
            | T.Event (e, q) ->
               add q ((p, E.Event e)::path);
               (E.Event e, q)::acc
            | T.Receive (ch, guard, targetf) ->
               List.fold_left
                 (fun acc e ->
                   if guard e then
                     let q = targetf e in
                     add q ((p, E.Event e)::path);
                     (E.Event e, q)::acc
                   else
                     acc)
                 acc (channel_to_event_list ch))
          [] trans_list
      in
      Hashtable.replace ht p (id, ts);
      loop ()
  in
  add p [];
  loop ();
  ht
