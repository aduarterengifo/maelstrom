open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Msg;;
(*
 idea is you have a table of promises, when you get the right msg you run them with the right body
*)

module StringSet = Set.Make(String)

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


type 'a env = 'a constraint 'a = <
  clock : _ Eio.Time.clock;   (** For time-related operations *)
  stdin : _ Eio.Flow.source;  (** For reading from standard input *)
  stdout : _ Eio.Flow.sink;
  stderr : _ Eio.Flow.sink;
> as 'a

type 'a state = {
  mutable node_id : string;
  msg_id : int Atomic.t;
  mutable messages : StringSet.t;
  mutable neighbors : StringSet.t;
  callbacks : (int, inbound_body -> unit) Hashtbl.t;
  env : 'a env;
}


let next_msg_id state =
  Atomic.fetch_and_add state.msg_id 1

let write_line_sync ~mutex ~flow line =
  Eio.Mutex.use_rw ~protect:true mutex (fun () ->
    Eio.Flow.copy_string (line ^ "\n") flow
  )

let add_common_fields ~msg_id ~in_reply_to body =
  { body with msg_id = Some msg_id; in_reply_to = in_reply_to }

let send ~stdout_mutex ~stdout ~stderr_mutex ~stderr msg =
  let response_str = yojson_of_msg msg |> Yojson.Safe.to_string in
  let tag = type_of_msg msg in
  write_line_sync ~mutex:stderr_mutex ~flow:stderr ("[" ^ String.uppercase_ascii tag ^ "] " ^ response_str);
  write_line_sync ~mutex:stdout_mutex ~flow:stdout response_str

let rpc ~stdout_mutex ~stdout ~stderr_mutex ~stderr ~state (msg_body:outbound_body) handler = 
  let msg_id = msg_id_of_outbound_body msg_body |> Option.get in
  Hashtbl.add state.callbacks msg_id (fun body ->
    Hashtbl.remove state.callbacks msg_id;
    handler body)

let make_response msg state body = {
  id = None;
  src = state.node_id; 
  dest = msg.src; 
  body
}

let gossip ~state ~state_mutex ~stdout_mutex ~stderr_mutex ~message ~msg =
  write_line_sync ~mutex:stderr_mutex ~flow:state.env#stderr  "BEGIN GOSSIP";
  let neighbors = StringSet.singleton msg.src
    |> StringSet.diff state.neighbors
    |> StringSet.elements
  in
  (* new switch *)
  Eio.Switch.run @@ fun sw ->
    (* new fiver under switch *)
    Eio.Fiber.fork ~sw @@ fun () ->
      (* in parallel *)
      Eio.Fiber.List.iter
        (fun dest ->
          let acked = ref false in 
          (* retry until acknowledged *)
          let rec retry () = 
            (* new msg_id *)
            let msg_id = next_msg_id state in 
            (* new body *)
            let body = Broadcast { message; msg_id = Some msg_id } in
            let msg = { 
              id = None; 
              src = state.node_id; 
              dest; 
              body = Outbound body 
            } in 
            let handler ~acked (request_body: inbound_body) =
              match request_body with
              | BroadcastOk _ -> acked := true
              | _ -> write_line_sync ~mutex:stderr_mutex ~flow:state.env#stderr "error"
            in
            rpc ~stdout_mutex ~stdout ~stderr_mutex ~stderr ~state body (handler ~acked);
            Eio.Time.sleep state.env#clock 1.0;
            if not !acked then retry ()
          in 
          retry ()
        ) neighbors;
  write_line_sync ~mutex:stderr_mutex ~flow:state.env#stderr  "END GOSSIP"

let parse_msg line  = 
  line 
  |> Yojson.Safe.from_string
  |> msg_of_yojson



let handle_message ~state ~state_mutex ~stdout_mutex ~stderr_mutex ~msg = 
  let msg_id_opt = msg_id_of_body msg.body in

  let handled_by_callback =
    match msg_id_opt with
    | Some msg_id ->
        Eio.Mutex.use_rw ~protect:true state_mutex (fun () ->
          match Hashtbl.find_opt state.callbacks msg_id with
          | Some callback ->
              write_line_sync ~mutex:stderr_mutex ~flow:state.env#stderr (Printf.sprintf "[CALLBACK MATCH] msg_id=%d" msg_id);
              callback msg.body;
              Hashtbl.remove state.callbacks msg_id;
              true
          | None -> false
        )
    | None -> false
  in

  write_line_sync ~mutex:stderr_mutex ~flow:state.env#stderr ("[MATCH " ^ String.uppercase_ascii (type_of_msg msg) ^ "]");
  let body = match msg.body with
  | Init b -> 
      Eio.Mutex.use_rw ~protect:true state_mutex (fun () ->
        state.node_id <- b.node_id;
      );
      let body = InitOk {
          msg_id = Atomic.get state.msg_id;
          in_reply_to = b.msg_id;
        } in 
      Some body
  | Echo b ->
      let body = EchoOk {
          msg_id = Some (Atomic.get state.msg_id);
          in_reply_to = b.msg_id;
          echo = b.echo;
        } in
      Some body;
  | Generate b ->             
      let body = GenerateOk {
          msg_id = Some (Atomic.get state.msg_id);
          in_reply_to = b.msg_id;
          id = state.node_id ^ string_of_int b.msg_id;
        } in 
      Some body;
  | Topology b ->            
      let body = TopologyOk {
          msg_id = Some (Atomic.get state.msg_id);
          in_reply_to = (match b.msg_id with Some id -> id | None -> 0);
        }in

      Eio.Mutex.use_rw ~protect:true state_mutex (fun () ->
        state.neighbors <- StringSet.of_list (Hashtbl.find b.topology state.node_id);
      );

      (* in a message with *some* callback id *)
      (* i keep an association from msg_ids to callbacks ids *)
      (* *)

      Some body
      (* TODO: handle not found *)
  | Broadcast b ->             
      (* locks the state *)
      (* if message is not in messages *)
      (* add it to the set *)
      (* for each neighbor (not including the one that sent it to us): send broadcast *)
      let should_gossip =
        Eio.Mutex.use_rw ~protect:true state_mutex (fun () ->
          if not (StringSet.mem b.message state.messages) then (
            state.messages <- StringSet.add b.message state.messages;
            true
          ) else false
        )
      in

      let response =
        match b.msg_id with
        | Some b_msg_id ->  
            let body = BroadcastOk {
                msg_id = Some (Atomic.get state.msg_id);
                in_reply_to = b_msg_id;
              } in
            Some body
        | None -> None
      in

      let () =
        if should_gossip then
          gossip ~state ~state_mutex ~stdout_mutex ~stderr_mutex ~message:b.message ~req 
      in

      response
  | Read b ->             
      let body = ReadOk {
          msg_id = Some (Atomic.get state.msg_id);
          in_reply_to = (match b.msg_id with Some id -> id | None -> 0);
          messages = Eio.Mutex.use_ro state_mutex (fun () -> StringSet.elements state.messages)
        } in 
      Some body
  (* handled by RPC (internal) *)
  | BroadcastOk b -> 
      None
  | Unknown b -> 
      None
  in
  match body with
  | Some body -> send ~stdout_mutex ~stdout:state.env#stdout ~stderr_mutex ~stderr:state.env#stderr (make_response msg state body)
  | None -> ();
  ()

let main ~state ~state_mutex ~stdout_mutex ~stderr_mutex =
  let buf = Eio.Buf_read.of_flow state.env#stdin ~max_size:4096 in
  Eio.Switch.run (fun sw ->
    let rec loop () =
      match Eio.Buf_read.line buf with
      | line ->
          Eio.Fiber.fork ~sw (fun () ->
            handle_message ~state ~state_mutex ~stdout_mutex ~stderr_mutex ~msg:(parse_msg line)
          );
          loop ()
      | exception End_of_file -> ()
    in
    loop ()
  )

let () =
  Eio_main.run @@ fun env ->
    let env_obj = object
      method stdin = Eio.Stdenv.stdin env
      method stdout = Eio.Stdenv.stdout env
      method stderr = Eio.Stdenv.stderr env
      method clock = Eio.Stdenv.clock env
    end in
    let state = { 
      node_id = "NOTSET"; 
      msg_id = Atomic.make 0; 
      messages = StringSet.empty; 
      neighbors = StringSet.empty; 
      callbacks = Hashtbl.create 0; 
      env = env_obj; 
    } in
    let state_mutex = Eio.Mutex.create () in
    let stdout_mutex = Eio.Mutex.create () in 
    let stderr_mutex = Eio.Mutex.create () in
    main
      ~state 
      ~state_mutex
      ~stdout_mutex
      ~stderr_mutex