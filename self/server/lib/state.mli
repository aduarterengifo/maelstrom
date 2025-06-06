module StringSet : Set.S with type elt = string

type 'a env = 'a constraint 'a = <
  clock : _ Eio.Time.clock;  
  stdin : _ Eio.Flow.source;  
  stdout : _ Eio.Flow.sink;
  stderr : _ Eio.Flow.sink;
> as 'a

type locks = {
  state: Eio.Mutex.t;
  stdout : Eio.Mutex.t;
  stderr : Eio.Mutex.t;
}

type 'a state = {
  mutable node_id : string;
  msg_id : int Atomic.t;
  mutable messages : StringSet.t;
  mutable neighbors : StringSet.t;
  mutable nodes: StringSet.t;
  callbacks : (int, Msg.inbound_body -> unit) Hashtbl.t;
  env : 'a env;
  locks: locks
}

val next_msg_id: 'a state -> int 