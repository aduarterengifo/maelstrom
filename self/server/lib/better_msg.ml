open Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* phantom types *)
type inbound
type outbound
type client
type peer

(* yojson helpers *)
let yojson_of_string_list l = `List (List.map (fun s -> `String s) l)

let yojson_of_map tbl =
  `Assoc
    (Hashtbl.fold (fun k v acc -> (k, yojson_of_string_list v) :: acc) tbl [])

(* client bodies *)
type init_body = { msg_id : int; node_id : string; node_ids : string list }
[@@deriving yojson]

type echo_body = { echo : string; msg_id : int } [@@deriving yojson]
type generate_body = { msg_id : int } [@@deriving yojson]

type topology_body = {
  topology : (string, string list) Hashtbl.t;
  msg_id : int option;
}

let yojson_of_topology_body { topology; msg_id } =
  `Assoc
    [
      ("topology", yojson_of_map topology);
      ("msg_id", match msg_id with Some id -> `Int id | None -> `Null);
    ]

let topology_body_of_yojson json =
  let open Yojson.Safe.Util in
  let topology =
    json |> member "topology" |> to_assoc
    |> List.fold_left
         (fun tbl (k, v) ->
           Hashtbl.add tbl k (v |> to_list |> List.map to_string);
           tbl)
         (Hashtbl.create 8)
  in
  let msg_id =
    match json |> member "msg_id" with
    | `Null -> None
    | `Int id -> Some id
    | _ -> None
  in
  { topology; msg_id }

type broadcast_body = { message : string; msg_id : int option }
[@@deriving yojson]

type read_body = { msg_id : int option } [@@deriving yojson]

type broadcast_ok_body = { in_reply_to : int; msg_id : int option }
[@@deriving yojson]

type echo_ok_body = { echo : string; msg_id : int option; in_reply_to : int }
[@@deriving yojson]

type init_ok_body = { in_reply_to : int; msg_id : int } [@@deriving yojson]

type generate_ok_body = { in_reply_to : int; msg_id : int option; id : string }
[@@deriving yojson]

type topology_ok_body = { in_reply_to : int; msg_id : int option }
[@@deriving yojson]

type read_ok_body = {
  messages : string list;
  in_reply_to : int;
  msg_id : int option;
}
[@@deriving yojson]

type error_body = { in_reply_to : int; code : int; text : string }
[@@deriving yojson]

type client_inbound_body =
  | Init of init_body
  | Echo of echo_body
  | Generate of generate_body
  | Topology of topology_body
  | Broadcast of broadcast_body
  | Read of read_body
[@@deriving yojson]

type client_outbound_body =
  | InitOk of init_ok_body
  | EchoOk of echo_ok_body
  | GenerateOk of generate_ok_body
  | TopologyOk of topology_ok_body
  | BroadcastOk of broadcast_ok_body
  | ReadOk of read_ok_body
  | Error of error_body
[@@deriving yojson]

type peer_body =
  | PeerBroadcastOk of broadcast_ok_body
  | PeerBroadcast of broadcast_body
[@@deriving yojson]

let peer_body_id = function
  | PeerBroadcastOk _ -> "broadcast_ok"
  | PeerBroadcast _ -> "broadcast"

let client_inbound_body_id = function
  | Init _ -> "init"
  | Echo _ -> "echo"
  | Generate _ -> "generate"
  | Topology _ -> "topology"
  | Broadcast _ -> "broadcast"
  | Read _ -> "read"

let peer_outbound_body_id = function
  | InitOk _ -> "init_ok"
  | EchoOk _ -> "echo_ok"
  | GenerateOk _ -> "generate_ok"
  | TopologyOk _ -> "topology_ok"
  | BroadcastOk _ -> "broadcast_ok"
  | ReadOk _ -> "read_ok"
  | Error _ -> "error"

type client_inbound_msg = {
  id : int;
  src : string;
  dest : string;
  body : client_inbound_body;
}
[@@deriving yojson]

type client_outbound_msg = {
  src : string;
  dest : string;
  body : client_outbound_body;
}
[@@deriving yojson]

type peer_msg = { src : string; dest : string; body : peer_body }
[@@deriving yojson]

type inbound_msg = ClientInbound of client_inbound_msg | Peer of peer_msg

let inbound_msg_of_yojson json =
  try ClientInbound (client_inbound_msg_of_yojson json)
  with _ -> (
    try Peer (peer_msg_of_yojson json)
    with _ -> failwith "Unknown inbound message format")
