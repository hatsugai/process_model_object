open Printf
open Type

type ('e, 'ch) send_receive =
  Send of 'e * ('e, 'ch) process
| Receive of 'e guard * ('e, 'ch) targetf

exception Error of string

let error s = raise (Error s)

let composition equal_event e_to_ch sync
      (pk : ('e, 'ch) process list -> ('e, 'ch) process)
      ps =

  let n = List.length ps in

  let ht = Hashtbl.create 0 in

  let reg ch i term =
    let v =
      match Hashtbl.find_opt ht ch with
        None ->
         let v = Array.make n [] in
         Hashtbl.add ht ch v;
         v
      | Some v -> v
    in
    v.(i) <- term::v.(i);
  in

  let consensus ch ys =

    let rec loop_ev e rs ys =
      match ys with
        [] ->
         let qs = List.rev rs in
         Some (T.Event (e, pk qs))
      | y::ys' ->
         (match y with
            Send (e', q) ->
             if equal_event e e' then
               loop_ev e (q::rs) ys'
             else
               None
          | Receive (g, tf) ->
             if g e then
               loop_ev e ((tf e)::rs) ys'
             else
               None)

    and loop_recv gs rs ys =
      match ys with
        [] ->
         let ts = List.rev rs in
         Some (T.Receive (ch,
                          (fun e -> List.for_all (fun g -> g e) gs),
                          (fun e ->
                            let qs = List.map (fun tf -> tf e) ts in
                            pk qs)))
      | y::ys' ->
         (match y with
            Send (e, q) ->
             if List.for_all (fun g -> g e) gs then
               loop_ev e (q::(List.map (fun tf -> tf e) rs)) ys'
             else
               None
          | Receive (g, tf) ->
             loop_recv (g::gs) (tf::rs) ys')
    in

    match ys with
      [] -> error "composition: no processes"
    | y::ys' ->
       (match y with
        | Send (e, q) -> loop_ev e [q] ys'
        | Receive (g, tf) -> loop_recv [g] [tf] ys')
  in

  let sync_trans acc =
    Hashtbl.fold
      (fun ch v acc ->
        let xss = Array.to_list v in
        let yss = Util.cartesian_product xss in
        List.fold_left
          (fun acc ys ->
            match consensus ch ys with
              None -> acc
            | Some t -> t::acc)
          acc yss)
      ht acc
  in
  
  let rec loop acc i rs ps =
    match ps with
      [] -> sync_trans acc
    | p::ps' ->
       let rec scan acc xs =
         match xs with
           [] -> loop acc (i+1) (p::rs) ps'
         | x::xs' ->
            (match x with 
               T.Tau p' ->
                scan (T.Tau (pk (List.rev_append rs (p'::ps')))::acc) xs'
             | T.Event (e, p') ->
                let ch = e_to_ch e in
                if sync ch then
                  (reg ch i (Send (e, p')); scan acc xs')
                else
                  scan (T.Event (e, (pk (List.rev_append rs (p'::ps'))))::acc) xs'
             | T.Receive (ch, guard, targetf) ->
                if sync ch then
                  (reg ch i (Receive (guard, targetf)); scan acc xs')
                else
                  let x_targetf e = pk (List.rev_append rs (targetf e::ps')) in
                  scan (T.Receive (ch, guard, x_targetf)::acc) xs')
       in
       scan acc (transitions p)
  in
  loop [] 0 [] ps


class ['e, 'ch] process_composition equal_event e_to_ch sync
        (ps : ('e, 'ch) process list) =
  let ref_ps = ref ps in
  object
    inherit ['e, 'ch] process
    val ps = ps
    method transitions =
      let pk ps = (({<ps>}) :> ('e, 'ch) process) in
      composition equal_event e_to_ch sync pk ps
    method equal =
      List.for_all2 equal_state !ref_ps ps
    method compare =
      let rec loop ps qs =
        match ps, qs with
          [], [] -> 0
        | p::ps', q::qs' ->
           let s = compare_state p q in
           if s <> 0 then
             s
           else
             loop ps' qs'
        | _, _ -> error "composition: compare"
      in
      loop !ref_ps ps
    method set_state = ref_ps := ps
    method hash =
      List.fold_left (fun hv p -> hv + hash_state p) 0 ps
    method anatomy = ps
    method show =
      let s =
        List.fold_left
          (fun s p ->
            if s="" then (p#show) else sprintf "%s; %s" s (p#show))
          "" ps
      in
      "[" ^ s ^ "]"
  end

let f equal_event e_to_ch sync ps =
  ((new process_composition equal_event e_to_ch sync ps) :> ('e, 'ch) process)
