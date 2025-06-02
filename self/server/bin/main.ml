open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module StringSet = Set.Make(String)

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
  msg_id: int option;
}

type broadcast_request_body = {
  message: string;
  msg_id: int option;
}

type read_request_body = {
  msg_id: int option;
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
      let msg_id = json |> member "msg_id" |> to_int_option in
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
      let msg_id = json |> member "msg_id" |> to_int_option in
    let message =
      match json |> member "message" with
      | `String s -> s
      | `Int i -> string_of_int i
      | v -> failwith ("Unexpected type for message: " ^ Yojson.Safe.to_string v)
      in
      BroadcastBody { msg_id; message }
  | `Read -> 
      let msg_id = json |> member "msg_id" |> to_int_option in
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
        ("msg_id", match msg_id with Some id -> `Int id | None -> `Null);
      ]
  | BroadcastBody {msg_id; message} -> 
      `Assoc [
        ("type", `String "broadcast");
        ("message", `String message);
        ("msg_id", match msg_id with Some id -> `Int id | None -> `Null);
      ]
  | ReadBody { msg_id } ->
      `Assoc [
        ("type", `String "read");
        ("msg_id", match msg_id with Some id -> `Int id | None -> `Null);
      ]
  | UnknownBody j -> j

type echo_response_body = {
  echo: string;
  msg_id: int option; 
  in_reply_to: int;
} [@@deriving yojson]

type init_response_body = {
  in_reply_to: int;
  msg_id: int;
} [@@deriving yojson]

type generate_response_body = {
  in_reply_to: int;
  msg_id: int option;
  id: string;
} [@@deriving yojson]

type topology_response_body = {
  in_reply_to: int;
  msg_id: int option;
} [@@deriving yojson]

type broadcast_response_body = {
  in_reply_to: int;
  msg_id: int option;
} [@@deriving yojson]

type read_response_body = {
  messages: string list;
  in_reply_to: int;
  msg_id: int option;
} [@@deriving yojson]

type response_error_body = {
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

type request = {
  id: int;
  src: string;
  dest: string;
  body: request_body [@yojson.of_yojson request_body_of_yojson] [@yojson.to_yojson yojson_of_request_body];
} [@@deriving yojson]


type response_body =
  | Init of init_response_body
  | Echo of echo_response_body
  | Generate of generate_response_body
  | Topology of topology_response_body
  | Broadcast of broadcast_response_body
  | Read of read_response_body
  | Error of response_error_body

let response_body_of_yojson (json : Yojson.Safe.t) : response_body =
  let open Yojson.Safe.Util in
  let type_str = json |> member "type" |> to_string in
  match type_str with
  | "init_ok" -> Init (init_response_body_of_yojson json)
  | "echo_ok" -> Echo (echo_response_body_of_yojson json)
  | "generate_ok" -> Generate (generate_response_body_of_yojson json)
  | "topology_ok" -> Topology (topology_response_body_of_yojson json)
  | "broadcast_ok" -> Broadcast (broadcast_response_body_of_yojson json)
  | "read_ok" -> Read (read_response_body_of_yojson json)
  | "error" -> Error (response_error_body_of_yojson json)
  | s -> failwith ("Unknown response type: " ^ s)

let yojson_of_response_body = function
  | Init b ->
      let base = yojson_of_init_response_body b in
      (match base with
      | `Assoc fields -> `Assoc (("type", `String "init_ok") :: fields)
      | _ -> base)
  | Echo b ->
      let base = yojson_of_echo_response_body b in
      (match base with
      | `Assoc fields -> `Assoc (("type", `String "echo_ok") :: fields)
      | _ -> base)
  | Generate b ->
      let base = yojson_of_generate_response_body b in
      (match base with
      | `Assoc fields -> `Assoc (("type", `String "generate_ok") :: fields)
      | _ -> base)
  | Topology b ->
      let base = yojson_of_topology_response_body b in
      (match base with
      | `Assoc fields -> `Assoc (("type", `String "topology_ok") :: fields)
      | _ -> base)
  | Broadcast b ->
      let base = yojson_of_broadcast_response_body b in
      (match base with
      | `Assoc fields -> `Assoc (("type", `String "broadcast_ok") :: fields)
      | _ -> base)
  | Read b ->
      let base = yojson_of_read_response_body b in
      (match base with
      | `Assoc fields -> `Assoc (("type", `String "read_ok") :: fields)
      | _ -> base)
  | Error b ->
      let base = yojson_of_response_error_body b in
      (match base with
      | `Assoc fields -> `Assoc (("type", `String "error") :: fields)
      | _ -> base)

type response = {
  src: string;
  dest: string;
  body: response_body [@yojson.of_yojson response_body_of_yojson] [@yojson.to_yojson yojson_of_response_body];
} [@@deriving yojson]

let response_body_id = function
  | Init _      -> "init"
  | Echo _      -> "echo"
  | Generate _  -> "generate"
  | Topology _  -> "topology"
  | Broadcast _ -> "broadcast"
  | Read _      -> "read"
  | Error _     -> "error"

let request_body_id = function
  | InitBody _      -> "init"
  | EchoBody _      -> "echo"
  | GenerateBody _  -> "generate"
  | TopologyBody _  -> "topology"
  | BroadcastBody _ -> "broadcast"
  | ReadBody _      -> "read"
  | UnknownBody _   -> "unknown"


type state = {
  mutable node_id : string;
  mutable messages : StringSet.t;
  mutable neighbors : string list;
}

let write_line_sync ~mutex ~flow line =
  Eio.Mutex.use_rw ~protect:true mutex (fun () ->
    Eio.Flow.copy_string (line ^ "\n") flow
  )

let add_common_fields ~msg_id ~in_reply_to body =
  { body with msg_id = Some msg_id; in_reply_to = in_reply_to }

let send_response_and_log ~stdout_mutex ~stdout ~stderr_mutex ~stderr response =
  let response_str = yojson_of_response response |> Yojson.Safe.to_string in
  let tag = response_body_id response.body in
  write_line_sync ~mutex:stderr_mutex ~flow:stderr ("[" ^ String.uppercase_ascii tag ^ "] " ^ response_str);
  write_line_sync ~mutex:stdout_mutex ~flow:stdout response_str

let msg_id_of_request_body = function
  | InitBody b -> Some b.msg_id
  | EchoBody b -> Some b.msg_id
  | GenerateBody b -> Some b.msg_id
  | TopologyBody b -> b.msg_id
  | BroadcastBody b -> b.msg_id
  | ReadBody b -> b.msg_id
  | UnknownBody _ -> None

let make_response (req: request) state body = {
 src = state.node_id; dest = req.src; body
}


let handle_message ~line ~msg_id ~state ~state_mutex ~stdout ~stderr ~stdout_mutex ~stderr_mutex = 
  Eio.Flow.copy_string (Printf.sprintf "[RECEIVED] %s\n" line) stderr;
        let json_str = Yojson.Safe.from_string line in
        let req = request_of_yojson json_str in 
        write_line_sync ~mutex:stderr_mutex ~flow:stderr ("[MATCH " ^ String.uppercase_ascii (request_body_id req.body) ^ "]");
        let body = match req.body with
        | InitBody b -> 
            Eio.Mutex.use_rw ~protect:true state_mutex (fun () ->
              state.node_id <- b.node_id;
            );
            let body = Init {
                msg_id = msg_id;
                in_reply_to = b.msg_id;
              } in 
            Eio.Mutex.use_rw ~protect:true state_mutex (fun () ->
              state.node_id <- b.node_id;
            );
            Some body
        | EchoBody b ->
            let body = Echo {
                msg_id = Some msg_id;
                in_reply_to = b.msg_id;
                echo = b.echo;
              } in
            Some body;
        | GenerateBody b ->             
            let body = Generate {
                msg_id = Some msg_id;
                in_reply_to = b.msg_id;
                id = state.node_id ^ string_of_int msg_id;
              } in 
            Some body;
        | TopologyBody b ->            
            let body = Topology {
                msg_id = Some msg_id;
                in_reply_to = (match b.msg_id with Some id -> id | None -> 0);
              }in

            Eio.Mutex.use_rw ~protect:true state_mutex (fun () ->
              state.neighbors <- (Hashtbl.find b.topology state.node_id);
            );

            Some body
            (* TODO: handle not found *)
        | BroadcastBody b ->             
            (* locks the state *)
            (* if message is not in messages *)
            (* add it to the set *)
            (* for each neighbor: send broadcast *)
            Eio.Mutex.use_rw ~protect:true state_mutex (fun () ->
                if not (StringSet.mem b.message state.messages) then (
                  state.messages <- StringSet.add b.message state.messages;
                  List.iter (fun neighbor -> 
                    let request: request = {
                      id = 0;
                      src = state.node_id;
                      dest = neighbor;
                      body = BroadcastBody { message = b.message; msg_id = None }
                    } in
                    let request_str = yojson_of_request request |> Yojson.Safe.to_string in
                    write_line_sync ~mutex:stderr_mutex ~flow:stderr ("[BROADCAST REQUEST: " ^ state.node_id ^ " -> " ^ neighbor ^ "] " ^ request_str);
                    write_line_sync ~mutex:stdout_mutex ~flow:stdout request_str;
                    ) state.neighbors;
                ) 
            );

            (match b.msg_id with
              | Some _ ->  
                  let body = Broadcast {
                      msg_id = Some msg_id;
                      in_reply_to = (match b.msg_id with Some id -> id | None -> 0);
                    } in
                  Some body;
              |  None -> None;
            );
        | ReadBody b ->             
            let body = Read {
                msg_id = Some msg_id;
                in_reply_to = (match b.msg_id with Some id -> id | None -> 0);
                messages = Eio.Mutex.use_ro state_mutex (fun () -> StringSet.elements state.messages)
              } in 
            Some body
        | UnknownBody b -> 
            None
        in 
        match body with
        | Some body -> send_response_and_log ~stdout_mutex ~stdout ~stderr_mutex ~stderr (make_response req state body)
        | None -> ();
        ()

        

let main ~stdin ~stdout ~stderr =
  let buf = Eio.Buf_read.of_flow stdin ~max_size:4096 in
  let state = { node_id= "NOTSET"; messages = StringSet.empty; neighbors = [] } in
  let state_mutex = Eio.Mutex.create () in
  let stdout_mutex = Eio.Mutex.create () in 
  let stderr_mutex = Eio.Mutex.create () in
  Eio.Switch.run (fun sw ->
    let rec loop msg_id =
      match Eio.Buf_read.line buf with
      | line ->
          Eio.Fiber.fork ~sw (fun () ->
            handle_message ~line ~msg_id ~state ~state_mutex ~stdout ~stderr ~stdout_mutex ~stderr_mutex
          );
          loop (msg_id + 1)
      | exception End_of_file -> ()
    in
    loop 0
  )

let () =
  Eio_main.run @@ fun env ->
    main
      ~stdin:(Eio.Stdenv.stdin env)
      ~stdout:(Eio.Stdenv.stdout env)
      ~stderr:(Eio.Stdenv.stderr env)