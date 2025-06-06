type inbound
type outbound

type init_body = {
  msg_id: int;
  node_id: string;
  node_ids: string list;
}

type echo_body = {
  echo: string;
  msg_id: int;
}

type generate_body = {
  msg_id: int;
}

type topology_body = {
  topology: (string, string list) Hashtbl.t;
  msg_id: int option;
}

type broadcast_body = {
  message: string;
  msg_id: int option;
} 

type read_body = {
  msg_id: int option;
}

type broadcast_ok_body = {
  in_reply_to: int;
  msg_id: int option;
} 

type echo_ok_body = {
  echo: string;
  msg_id: int option; 
  in_reply_to: int;
}

type init_ok_body = {
  in_reply_to: int;
  msg_id: int;
} 

type generate_ok_body = {
  in_reply_to: int;
  msg_id: int option;
  id: string;
} 

type topology_ok_body = {
  in_reply_to: int;
  msg_id: int option;
} 

type read_ok_body = {
  messages: string list;
  in_reply_to: int;
  msg_id: int option;
} 

type error_body = {
  in_reply_to: int;
  code: int;
  text: string;
} 

type inbound_body =
  | Init of init_body
  | Echo of echo_body
  | Generate of generate_body
  | Topology of topology_body
  | Broadcast of broadcast_body
  | BroadcastOk of broadcast_ok_body
  | Read of read_body
  | Unknown of Yojson.Safe.t

val inbound_body_id: inbound_body -> string 
val inbound_body_of_yojson : Yojson.Safe.t -> inbound_body
val yojson_of_inbound_body : inbound_body -> Yojson.Safe.t

type outbound_body =
  | InitOk of init_ok_body
  | EchoOk of echo_ok_body
  | GenerateOk of generate_ok_body
  | TopologyOk of topology_ok_body
  | BroadcastOk of broadcast_ok_body
  | ReadOk of read_ok_body
  | Error of error_body
  | Broadcast of broadcast_body 

val outbound_body_id: outbound_body -> string 
val outbound_body_of_yojson : Yojson.Safe.t -> outbound_body
val yojson_of_outbound_body : outbound_body -> Yojson.Safe.t

type inbound_msg = {
  id: int;
  src: string;
  dest: string;
  body: inbound_body;
}

type outbound_msg = {
  src: string;
  dest: string;
  body: outbound_body;
}

(* val make_outbound_msg: inbound_msg -> state -> outbound_body -> outbound_msg  *)

val type_of_inbound_msg: inbound_msg -> string

val type_of_outbound_msg : outbound_msg -> string

val msg_id_of_outbound_body : outbound_body -> int option

val msg_id_of_inbound_body : inbound_body -> int option

val yojson_of_outbound_msg : outbound_msg -> Yojson.Safe.t
val yojson_of_inbound_msg : inbound_msg -> Yojson.Safe.t

val inbound_msg_of_yojson :  Yojson.Safe.t -> inbound_msg