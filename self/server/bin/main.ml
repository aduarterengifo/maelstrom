open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type request_kind =
  | Init
  | Echo
  | Other of string

type init_request_body = {
  msg_id: int;
  node_id: string;
  node_ids: string list;
}

type echo_request_body = {
  echo: string;
  msg_id: int;
}

type generate_request_body = {
  msg_id: int;
}

type topology_request_body = {
  topology: (string, string list) Hashtbl.t;
  msg_id: int;
}

type broadcast_request_body = {
  message: string;
  msg_id: int;
}

type read_request_body = {
  msg_id: int;
}

type request_body =
  | InitBody of init_request_body
  | EchoBody of echo_request_body
  | GenerateBody of generate_request_body
  | TopologyBody of topology_request_body
  | BroadcastBody of broadcast_request_body
  | ReadBody of read_request_body
  | UnknownBody of Yojson.Safe.t

let request_body_of_yojson (json : Yojson.Safe.t) : request_body =
  (* let () = Printf.eprintf "DEBUG: request_body_of_yojson got: %s\n%!" (Yojson.Safe.to_string json) in *)
  let open Yojson.Safe.Util in
  let kind =
    match json |> member "type" |> to_string_option with
    | Some "init" -> `Init
    | Some "echo" -> `Echo
    | Some "generate" -> `Generate
    | Some "topology" -> `Topology
    | Some "broadcast" -> `Broadcast
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
      InitBody { msg_id; node_id; node_ids }
  | `Echo ->
      let echo = json |> member "echo" |> to_string in
      let msg_id = json |> member "msg_id" |> to_int in
      EchoBody { echo; msg_id }
  | `Generate -> 
      let msg_id = json |> member "msg_id" |> to_int in
      GenerateBody { msg_id }
  | `Topology -> 
      let msg_id = json |> member "msg_id" |> to_int in
      let topology =
        let tbl = Hashtbl.create 8 in
        json |> member "topology" |> to_assoc |> List.iter (fun (k, v) ->
          let neighbors = v |> to_list |> List.map to_string in
          Hashtbl.add tbl k neighbors
        );
        tbl
      in
      TopologyBody { msg_id; topology }
  | `Broadcast -> 
      let msg_id = json |> member "msg_id" |> to_int in
      let message = json |> member "message" |> to_string in
      BroadcastBody { msg_id; message }
  | `Read -> 
      let msg_id = json |> member "msg_id" |> to_int in
      ReadBody { msg_id }
  | `Other _ ->
      UnknownBody json

let yojson_of_string_list l = `List (List.map (fun s -> `String s) l)
let yojson_of_map tbl =
  `Assoc (
    Hashtbl.fold (fun k v acc -> (k, yojson_of_string_list v) :: acc) tbl []
  )

let yojson_of_request_body = function
  | InitBody { msg_id; node_id; node_ids } ->
      `Assoc [
        ("type", `String "init");
        ("msg_id", `Int msg_id);
        ("node_id", `String node_id);
        ("node_ids", `List (List.map (fun s -> `String s) node_ids));
      ]
  | EchoBody { echo; msg_id } ->
      `Assoc [
        ("type", `String "echo");
        ("echo", `String echo);
        ("msg_id", `Int msg_id);
      ]
  | GenerateBody { msg_id } ->
      `Assoc [
        ("type", `String "generate");
        ("msg_id", `Int msg_id);
      ]
  | TopologyBody {msg_id; topology } -> 
      `Assoc [
        ("type", `String "topology");
        ("topology", yojson_of_map topology);
        ("msg_id", `Int msg_id);
      ]
  | BroadcastBody {msg_id; message} -> 
      `Assoc [
        ("type", `String "broadcast");
        ("message", `String message);
        ("msg_id", `Int msg_id);
      ]
  | ReadBody { msg_id } ->
      `Assoc [
        ("type", `String "read");
        ("msg_id", `Int msg_id);
      ]
  | UnknownBody j -> j

type echo_response_body = {
  type_: string [@yojson.key "type"];
  echo: string;
  msg_id: int option; 
  in_reply_to: int;
} [@@deriving yojson]

type init_response_body = {
  type_: string [@yojson.key "type"];
  in_reply_to: int;
  msg_id: int;
} [@@deriving yojson]

type generate_response_body = {
  type_: string [@yojson.key "type"];
  in_reply_to: int;
  msg_id: int option;
  id: string;
} [@@deriving yojson]

type topology_response_body = {
  type_: string [@yojson.key "type"];
  in_reply_to: int;
  msg_id: int option;
} [@@deriving yojson]

type broadcast_response_body = {
  type_: string [@yojson.key "type"];
  in_reply_to: int;
  msg_id: int option;
} [@@deriving yojson]

type read_response_body = {
  type_: string [@yojson.key "type"];
  messages: string list;
  in_reply_to: int;
  msg_id: int option;
} [@@deriving yojson]

type response_error_body = {
  type_: string [@yojson.key "type"];
  in_reply_to: int;
  code: int;
  text: string;
} [@@deriving yojson]

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

type body_base = { 
  type_: string [@yojson.key "type"];
  msg_id: string option;
  in_reply_to: string option;
} [@@deriving yojson]

type request = {
  id: int option;
  src: string;
  dest: string;
  body: request_body [@yojson.of_yojson request_body_of_yojson] [@yojson.to_yojson yojson_of_request_body];
} [@@deriving yojson]


type init_response = {
  src: string;
  dest: string;
  body: init_response_body
} [@@deriving yojson]

type echo_response = {
  src: string;
  dest: string;
  body: echo_response_body
} [@@deriving yojson]

type generate_response = {
  src: string;
  dest: string;
  body: generate_response_body
} [@@deriving yojson]

type topology_response = {
  src: string;
  dest: string;
  body: topology_response_body
} [@@deriving yojson]

type broadcast_response = {
  src: string;
  dest: string;
  body: broadcast_response_body
} [@@deriving yojson]


let main ~stdin ~stdout ~stderr =
  let buf = Eio.Buf_read.of_flow stdin ~max_size:4096 in
  Eio.Flow.copy_string "[INFO] Entering main loop\n" stderr;
  let rec loop node_id msg_id =
    match Eio.Buf_read.line buf with
    | line -> 
        Eio.Flow.copy_string (Printf.sprintf "[RECEIVED] %s\n" line) stderr;
        let json_str = Yojson.Safe.from_string line in
        let req = request_of_yojson json_str in 
        match req.body with
        | InitBody b -> 
            Eio.Flow.copy_string "[MATCH] InitBody\n" stderr;
            (* construct init response *) 
            let response_str = {
              src = b.node_id;
              dest = req.src;
              body = {
                type_ = "init_ok";
                msg_id = msg_id;
                in_reply_to = b.msg_id;
              }
            } 
              |> yojson_of_init_response 
              |> Yojson.Safe.to_string in
            (* log to stderr *)
            Eio.Flow.copy_string (Printf.sprintf "Initialized node %s\n" b.node_id) stderr;
            Eio.Flow.copy_string (Printf.sprintf "Init send %s\n" response_str) stderr;
            (* msg init response *) 
            Eio.Flow.copy_string (response_str ^ "\n") stdout;
            (* handle init, update node_id *)
            loop b.node_id (msg_id + 1)
        | EchoBody b ->
            Eio.Flow.copy_string "[MATCH] EchoBody\n" stderr;
            (* construct echo response *) 
            let response_str = {
              src = req.dest;
              dest = req.src;
              body = {
                type_ = "echo_ok";
                msg_id = Some msg_id;
                in_reply_to = b.msg_id;
                echo = b.echo;
              }
            } 
              |> yojson_of_echo_response 
              |> Yojson.Safe.to_string in
            (* log to stderr *)
            Eio.Flow.copy_string (Printf.sprintf "Echoing %s\n" response_str) stderr;
            (* msg echo response *)
            Eio.Flow.copy_string (response_str ^ "\n") stdout;
            loop node_id (msg_id + 1)
        | GenerateBody b ->             
            Eio.Flow.copy_string "[MATCH] GenerateBody\n" stderr;
            let response_str = {
              src = node_id;
              dest = req.src;
              body = {
                type_ = "generate_ok";
                msg_id = Some msg_id;
                in_reply_to = b.msg_id;
                id = node_id ^ string_of_int msg_id;
              }
            } 
              |> yojson_of_generate_response 
              |> Yojson.Safe.to_string in
            (* log to stderr *)
            Eio.Flow.copy_string (Printf.sprintf "[GENERATE RESPONSE] %s\n" response_str) stderr;
            (* msg echo response *)
            Eio.Flow.copy_string (response_str ^ "\n") stdout;
            loop node_id (msg_id + 1)
        | TopologyBody b ->            
            Eio.Flow.copy_string "[MATCH] TopologyBody\n" stderr;
            let response_str = {
              src = node_id;
              dest = req.src;
              body = {
                type_ = "topology_ok";
                msg_id = Some msg_id;
                in_reply_to = b.msg_id;
              }
            } 
              |> yojson_of_topology_response 
              |> Yojson.Safe.to_string in
            (* log response to stderr *)
            Eio.Flow.copy_string (Printf.sprintf "[TOPOLOGY RESPONSE] %s\n" response_str) stderr;
            (* msg echo response *)
            Eio.Flow.copy_string (response_str ^ "\n") stdout;
            loop node_id (msg_id + 1)
        | BroadcastBody b ->             
            Eio.Flow.copy_string "[MATCH] BroadcastBody\n" stderr;
            let response_str = {
              src = node_id;
              dest = req.src;
              body = {
                type_ = "broadcast_ok";
                msg_id = Some msg_id;
                in_reply_to = b.msg_id;
              }
            } 
              |> yojson_of_broadcast_response 
              |> Yojson.Safe.to_string in
            (* log to stderr *)
            Eio.Flow.copy_string (Printf.sprintf "[BROADCAST RESPONSE] %s\n" response_str) stderr;
            (* msg echo response *)
            Eio.Flow.copy_string (response_str ^ "\n") stdout;
            loop node_id (msg_id + 1)
          | ReadBody b ->             
            Eio.Flow.copy_string "[MATCH] ReadBody\n" stderr;
            let response_str = {
              src = node_id;
              dest = req.src;
              body = {
                type_ = "read_ok";
                msg_id = Some msg_id;
                in_reply_to = b.msg_id;
              }
            } 
              |> yojson_of_broadcast_response 
              |> Yojson.Safe.to_string in
            (* log to stderr *)
            Eio.Flow.copy_string (Printf.sprintf "[READ RESPONSE] %s\n" response_str) stderr;
            (* msg echo response *)
            Eio.Flow.copy_string (response_str ^ "\n") stdout;
            loop node_id (msg_id + 1)
        | UnknownBody b -> 
            Eio.Flow.copy_string "[MATCH] UnknownBody\n" stderr;
            (* maybe at some point implement the errors *)
        loop node_id (msg_id + 1)
    | exception End_of_file -> ()
  in
  loop "NOTSET" 0


let () =
  Eio_main.run @@ fun env ->
    main
      ~stdin:(Eio.Stdenv.stdin env)
      ~stdout:(Eio.Stdenv.stdout env)
      ~stderr:(Eio.Stdenv.stderr env)