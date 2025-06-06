open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type suberror =
  | Timeout
  | Node_not_found
  | Not_supported
  | Temporarily_unavailable
  | Malformed_request
  | Crash
  | Abort
  | Key_does_not_exist
  | Key_already_exists
  | Precondition_failed
  | Txn_conflict

let suberror_to_code = function
  | Timeout -> 0
  | Node_not_found -> 1
  | Not_supported -> 10
  | Temporarily_unavailable -> 11
  | Malformed_request -> 12
  | Crash -> 13
  | Abort -> 14
  | Key_does_not_exist -> 20
  | Key_already_exists -> 21
  | Precondition_failed -> 22
  | Txn_conflict -> 30

let code_to_suberror = function
  | 0 -> Some Timeout
  | 1 -> Some Node_not_found
  | 10 -> Some Not_supported
  | 11 -> Some Temporarily_unavailable
  | 12 -> Some Malformed_request
  | 13 -> Some Crash
  | 14 -> Some Abort
  | 20 -> Some Key_does_not_exist
  | 21 -> Some Key_already_exists
  | 22 -> Some Precondition_failed
  | 30 -> Some Txn_conflict
  | _ -> None

let suberror_to_string = function
  | Timeout -> "timeout"
  | Node_not_found -> "node-not-found"
  | Not_supported -> "not-supported"
  | Temporarily_unavailable -> "temporarily-unavailable"
  | Malformed_request -> "malformed-request"
  | Crash -> "crash"
  | Abort -> "abort"
  | Key_does_not_exist -> "key-does-not-exist"
  | Key_already_exists -> "key-already-exists"
  | Precondition_failed -> "precondition-failed"
  | Txn_conflict -> "txn-conflict"

let string_to_suberror = function
  | "timeout" -> Some Timeout
  | "node-not-found" -> Some Node_not_found
  | "not-supported" -> Some Not_supported
  | "temporarily-unavailable" -> Some Temporarily_unavailable
  | "malformed-request" -> Some Malformed_request
  | "crash" -> Some Crash
  | "abort" -> Some Abort
  | "key-does-not-exist" -> Some Key_does_not_exist
  | "key-already-exists" -> Some Key_already_exists
  | "precondition-failed" -> Some Precondition_failed
  | "txn-conflict" -> Some Txn_conflict
  | _ -> None

type inbound
type outbound
(* MSG *)
(* utils *)
let yojson_of_string_list l = `List (List.map (fun s -> `String s) l)
let yojson_of_map tbl =
  `Assoc (
    Hashtbl.fold (fun k v acc -> (k, yojson_of_string_list v) :: acc) tbl []
  )
(* utils *)
(* BODIES *)
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
} [@@deriving yojson]

type read_body = {
  msg_id: int option;
}

type broadcast_ok_body = {
  in_reply_to: int;
  msg_id: int option;
} [@@deriving yojson]

type echo_ok_body = {
  echo: string;
  msg_id: int option; 
  in_reply_to: int;
} [@@deriving yojson]

type init_ok_body = {
  in_reply_to: int;
  msg_id: int;
} [@@deriving yojson]

type generate_ok_body = {
  in_reply_to: int;
  msg_id: int option;
  id: string;
} [@@deriving yojson]

type topology_ok_body = {
  in_reply_to: int;
  msg_id: int option;
} [@@deriving yojson]

type read_ok_body = {
  messages: string list;
  in_reply_to: int;
  msg_id: int option;
} [@@deriving yojson]

type error_body = {
  in_reply_to: int;
  code: int;
  text: string;
} [@@deriving yojson]
(* BODIES *)
(* unions *)
type inbound_body =
  | Init of init_body
  | Echo of echo_body
  | Generate of generate_body
  | Topology of topology_body
  | Broadcast of broadcast_body
  | BroadcastOk of broadcast_ok_body
  | Read of read_body
  | Unknown of Yojson.Safe.t

type outbound_body =
  | InitOk of init_ok_body
  | EchoOk of echo_ok_body
  | GenerateOk of generate_ok_body
  | TopologyOk of topology_ok_body
  | BroadcastOk of broadcast_ok_body
  | ReadOk of read_ok_body
  | Error of error_body
  | Broadcast of broadcast_body 
[@@deriving yojson]
(* unions *)
(* union funcs *)
let inbound_body_id = function
  | Init _        -> "init"
  | Echo _        -> "echo"
  | Generate _    -> "generate"
  | Topology _    -> "topology"
  | Broadcast _   -> "broadcast"
  | BroadcastOk _ -> "broadcast_ok"
  | Read _        -> "read"
  | Unknown _     -> "unknown"
let inbound_body_of_yojson (json : Yojson.Safe.t) : inbound_body =
  (* let () = Printf.eprintf "DEBUG: request_body_of_yojson got: %s\n%!" (Yojson.Safe.to_string json) in *)
  let open Yojson.Safe.Util in
  let kind =
    match json |> member "type" |> to_string_option with
    | Some "init" -> `Init
    | Some "echo" -> `Echo
    | Some "generate" -> `Generate
    | Some "topology" -> `Topology
    | Some "broadcast" -> `Broadcast
    | Some "broadcast_ok" -> `BroadcastOk
    | Some "read" -> `Read
    | Some s      -> `Other s
    | None        -> `Other ""
  in
  match kind with
  | `Init ->
      let msg_id =
        match json |> member "msg_id" |> to_int_option with
        | Some v -> v
        | None -> failwith "Missing or invalid msg_id in init"
      in
      let node_id =
        match json |> member "node_id" |> to_string_option with
        | Some v -> v
        | None -> failwith "Missing or invalid node_id in init"
      in
      let node_ids =
        try json |> member "node_ids" |> to_list |> List.map to_string
        with _ -> failwith "Missing or invalid node_ids in init"
      in
      Init { msg_id; node_id; node_ids }
  | `Echo ->
      let echo = json |> member "echo" |> to_string in
      let msg_id = json |> member "msg_id" |> to_int in
      Echo { echo; msg_id }
  | `Generate -> 
      let msg_id = json |> member "msg_id" |> to_int in
      Generate { msg_id }
  | `Topology -> 
      let msg_id = json |> member "msg_id" |> to_int_option in
      let topology =
        let tbl = Hashtbl.create 8 in
        json |> member "topology" |> to_assoc |> List.iter (fun (k, v) ->
          let neighbors = v |> to_list |> List.map to_string in
          Hashtbl.add tbl k neighbors
        );
        tbl
      in
      Topology { msg_id; topology }
  | `Broadcast -> 
      let msg_id = json |> member "msg_id" |> to_int_option in
      let message =
        match json |> member "message" with
        | `String s -> s
        | `Int i -> string_of_int i
        | v -> failwith ("Unexpected type for message: " ^ Yojson.Safe.to_string v)
      in
      Broadcast { msg_id; message }
  | `BroadcastOk -> 
      let msg_id = json |> member "msg_id" |> to_int_option in
      let in_reply_to = json |> member "in_reply_to" |> to_int in
      BroadcastOk { msg_id; in_reply_to; }
  | `Read -> 
      let msg_id = json |> member "msg_id" |> to_int_option in
      Read { msg_id }
  | `Other _ ->
      Unknown json

let yojson_of_inbound_body = function
  | Init { msg_id; node_id; node_ids } ->
      `Assoc [
        ("type", `String "init");
        ("msg_id", `Int msg_id);
        ("node_id", `String node_id);
        ("node_ids", `List (List.map (fun s -> `String s) node_ids));
      ]
  | Echo { echo; msg_id } ->
      `Assoc [
        ("type", `String "echo");
        ("echo", `String echo);
        ("msg_id", `Int msg_id);
      ]
  | Generate { msg_id } ->
      `Assoc [
        ("type", `String "generate");
        ("msg_id", `Int msg_id);
      ]
  | Topology {msg_id; topology } -> 
      `Assoc [
        ("type", `String "topology");
        ("topology", yojson_of_map topology);
        ("msg_id", match msg_id with Some id -> `Int id | None -> `Null);
      ]
  | Broadcast {msg_id; message} -> 
      `Assoc [
        ("type", `String "broadcast");
        ("message", `String message);
        ("msg_id", match msg_id with Some id -> `Int id | None -> `Null);
      ]
  | BroadcastOk {msg_id; in_reply_to} -> 
      `Assoc [
        ("type", `String "broadcast_ok");
        ("in_reply_to", `Int in_reply_to);
        ("msg_id", match msg_id with Some id -> `Int id | None -> `Null);
      ]
  | Read { msg_id } ->
      `Assoc [
        ("type", `String "read");
        ("msg_id", match msg_id with Some id -> `Int id | None -> `Null);
      ]
  | Unknown j -> j

let outbound_body_id = function
| InitOk _      -> "init_ok"
| EchoOk _      -> "echo_ok"
| GenerateOk _  -> "generate_ok"
| TopologyOk _  -> "topology_ok"
| BroadcastOk _ -> "broadcast_ok"
| Broadcast _ -> "broadcast"
| ReadOk _      -> "read_ok"
| Error _     -> "error"

let outbound_body_of_yojson (json : Yojson.Safe.t) : outbound_body =
  let open Yojson.Safe.Util in
  let type_str = json |> member "type" |> to_string in
  match type_str with
  | "init_ok" -> InitOk (init_ok_body_of_yojson json)
  | "echo_ok" -> EchoOk (echo_ok_body_of_yojson json)
  | "generate_ok" -> GenerateOk (generate_ok_body_of_yojson json)
  | "topology_ok" -> TopologyOk (topology_ok_body_of_yojson json)
  | "broadcast_ok" -> BroadcastOk (broadcast_ok_body_of_yojson json)
  | "read_ok" -> ReadOk (read_ok_body_of_yojson json)
  | "error" -> Error (error_body_of_yojson json)
  | s -> failwith ("Unknown response type: " ^ s)

let yojson_of_outbound_body rb =
  let type_str = outbound_body_id rb in
  let base = match rb with
    | InitOk b      -> yojson_of_init_ok_body b
    | EchoOk b      -> yojson_of_echo_ok_body b
    | GenerateOk b  -> yojson_of_generate_ok_body b
    | TopologyOk b  -> yojson_of_topology_ok_body b
    | BroadcastOk b -> yojson_of_broadcast_ok_body b
    | Broadcast b   -> yojson_of_broadcast_body b
    | ReadOk b      -> yojson_of_read_ok_body b
    | Error b       -> yojson_of_error_body b
  in
  match base with
  | `Assoc fields -> `Assoc (("type", `String type_str) :: fields)
  | _ -> base

type inbound_msg = {
  id: int;
  src: string;
  dest: string;
  body: inbound_body;
}[@@deriving yojson]

type outbound_msg = {
  src: string;
  dest: string;
  body: outbound_body;
}[@@deriving yojson]

let type_of_inbound_msg (inbound_msg:inbound_msg) = inbound_body_id inbound_msg.body
let type_of_outbound_msg outbound_msg = outbound_body_id outbound_msg.body

let msg_id_of_inbound_body = function
  | Init b -> Some b.msg_id
  | Echo b -> Some b.msg_id
  | Generate b -> Some b.msg_id
  | Topology b -> b.msg_id
  | Broadcast b -> b.msg_id
  | BroadcastOk b -> b.msg_id
  | Read b -> b.msg_id
  | Unknown _ -> None

let msg_id_of_outbound_body = function
  | InitOk b -> Some b.msg_id
  | EchoOk b -> b.msg_id
  | GenerateOk b -> b.msg_id
  | TopologyOk b -> b.msg_id
  | BroadcastOk b -> b.msg_id
  | ReadOk b -> b.msg_id
  | Broadcast b -> b.msg_id
  | Error _ -> None
