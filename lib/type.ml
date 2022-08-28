module E =
  struct
    type 'e t = Tau | Event of 'e
    [@@deriving show { with_path=false }, eq, ord, hash]
  end

type 'e guard = 'e -> bool
[@printer fun fmt _ -> fprintf fmt "<guard>"]
[@@deriving show { with_path=false }]

type ('e, 'ch, 'p) targetf' = 'e -> 'p
[@printer fun fmt _ -> fprintf fmt "<targetf>"]
[@@deriving show { with_path=false }]

module T =
  struct
    type ('e, 'ch, 'p) trans =
      Tau of 'p
    | Event of 'e * 'p
    | Receive of 'ch * 'e guard * ('e, 'ch, 'p) targetf'
    [@@deriving show { with_path=false }]
  end

class virtual ['e, 'ch] process =
  object
    method virtual transitions : ('e, 'ch, ('e, 'ch) process) T.trans list
    method virtual equal : bool
    method virtual compare : int
    method virtual set_state : unit
    method virtual hash : int
    method virtual anatomy : ('e, 'ch) process list
    method virtual show : string
  end

let pp_process fmt process =
  Format.fprintf fmt "%s" (process#show)

type ('e, 'ch) targetf = ('e, 'ch, ('e, 'ch) process) targetf'
[@printer fun fmt _ -> fprintf fmt "<targetf>"]
[@@deriving show { with_path=false }]

type ('e, 'ch, 'state) process_class = {
    transf : ('state -> ('e, 'ch) process) -> 'state ->
             ('e, 'ch, ('e, 'ch) process) T.trans list;
    equal : 'state -> 'state -> bool;
    compare : 'state -> 'state -> int;
    hash : 'state -> int;
    show : 'state -> string;
    mutable state : 'state;
  }

class ['e, 'ch] process_concrete process_class initial_state =
  object
    inherit ['e, 'ch] process
    val state = initial_state
    method transitions =
      let pk state = (({<state>}) :> ('e, 'ch) process) in
      process_class.transf pk state
    method equal = process_class.equal process_class.state state
    method compare = process_class.compare process_class.state state
    method set_state = process_class.state <- state
    method hash = process_class.hash state
    method anatomy = []
    method show = process_class.show state
  end

let make_process_class transf equal compare hash show state =
  { transf; equal; compare; hash; show; state }

let make_process process_class initial_state =
  ((new process_concrete process_class initial_state) :> ('e, 'ch) process)

let transitions (p : ('e, 'ch) process) : ('e, 'ch, ('e, 'ch) process) T.trans list =
  p#transitions

let equal_state p q =
  p#set_state;
  q#equal

let compare_state p q =
  p#set_state;
  q#compare

let hash_state p = p#hash
