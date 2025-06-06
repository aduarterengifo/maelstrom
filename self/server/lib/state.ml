module StringSet = Set.Make(String)

type 'a env = 'a constraint 'a = <
  clock : _ Eio.Time.clock;   (** For time-related operations *)
  stdin : _ Eio.Flow.source;  (** For reading from standard input *)
  stdout : _ Eio.Flow.sink;
  stderr : _ Eio.Flow.sink;
> as 'a

type locks = {
  state : Eio.Mutex.t;
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

let next_msg_id (state: _ state) =
  Atomic.fetch_and_add state.msg_id 1