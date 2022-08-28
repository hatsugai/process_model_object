open Printf
open Process_model_object
open Type
open T
open Base.Hash.Builtin

(**********************************************************************)

type event = A | B | C of int
[@@deriving show { with_path=false }, eq, ord, hash]

type channel = ChA | ChB | ChC
[@@deriving show { with_path=false }, eq, ord, hash]

let e_to_ch e =
  match e with
    A -> ChA
  | B -> ChB
  | C _ -> ChC

let ch_to_es ch =
  match ch with
    ChA -> [A]
  | ChB -> [B]
  | ChC -> List.map (fun x -> C x) [0; 1; 2; 3]

(**********************************************************************)

type p_state = P0 | P1 | P2 | P3
[@@deriving show { with_path=false }, eq, ord, hash]

let p_transf pk state =
  match state with
    P0 -> [Event (A, pk P1); Event (B, pk P2);
           Receive (ChC, (fun _ -> true), (fun _ -> pk P3))]
  | P1 -> [Event (C 2, pk P3)]
  | P2 -> [Receive (ChC, (fun _ -> true), (fun _ -> pk P3))]
  | _ -> []

let p_process_ref_state = ref P0

let p_class =
  make_process_class p_transf equal_p_state compare_p_state hash_p_state
    show_p_state P0

let p = make_process p_class P0

(**********************************************************************)

type q_state = Q0 | Q1 | Q2 | Q3
[@@deriving show { with_path=false }, eq, ord, hash]

let q_transf pk state =
  match state with
    Q0 -> [Event (A, pk Q1); Event (B, pk Q2);
           Event (C 3, pk Q2)]
  | Q1 -> [Event (C 1, pk Q3)]
  | _ -> []

let q_process_ref_state = ref Q0
let q_class =
  make_process_class q_transf equal_q_state compare_q_state hash_q_state
    show_q_state Q0

let q = make_process q_class Q0


(**********************************************************************)

let sync ch =
  match ch with
    ChA -> true
  | ChC -> true
  | _ -> false

let pq = Composition.f equal_event e_to_ch sync [p; q]

let () =
  List.iteri
    (fun i x -> printf "%d. %s\n" i (show_trans pp_event pp_channel pp_process x))
    (transitions pq)

(**********************************************************************)

let print_ht ht =
  printf "----------------------------------------\n";
  Hashtable.iter
    (fun p (id, trans) ->
      printf "%d. %s\n" id (p#show);
      List.iteri
        (fun i (u, q) ->
          printf "    %d. %s %s\n" i (E.show pp_event u) (q#show))
        trans)
    ht

let print_process p =
  let ht = Unfold.f ch_to_es p 100 in
  print_ht ht


let () = print_process p
let () = print_process q
let () = print_process pq

