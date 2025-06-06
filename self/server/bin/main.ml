open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Bag;;

let write_line_sync ~mutex ~flow line =
  Eio.Mutex.use_rw ~protect:true mutex (fun () ->
    Eio.Flow.copy_string (line ^ "\n") flow
  )

let log ~(state: _ State.state) line = 
  write_line_sync ~mutex:state.locks.stderr ~flow:state.env#stderr line 

let write ~(state: _ State.state) line = 
  write_line_sync ~mutex:state.locks.stdout ~flow:state.env#stdout line 

let send ~(state: _ State.state) outbound_msg =
  let response_str = outbound_msg |> Msg.yojson_of_outbound_msg  |> Yojson.Safe.to_string in
  let tag = Msg.type_of_outbound_msg outbound_msg in
  log ~state ("[" ^ String.uppercase_ascii tag ^ "] " ^ response_str);
  write ~state response_str

let rpc~(state: _ State.state) (msg_body:Msg.outbound_body) handler = 
  let msg_id = Msg.msg_id_of_outbound_body msg_body |> Option.get in
  Hashtbl.add state.callbacks msg_id (fun body ->
    Hashtbl.remove state.callbacks msg_id;
    handler body
  )

let gossip ~(state: _ State.state) ~message ~(inbound_msg:Msg.inbound_msg) =
  log ~state  "BEGIN GOSSIP";
  let neighbors =  inbound_msg.src
    |> State.StringSet.singleton
    |> State.StringSet.diff state.neighbors
    |> State.StringSet.elements
  in
  Eio.Switch.run @@ fun sw ->
    Eio.Fiber.fork ~sw @@ fun () ->
      Eio.Fiber.List.iter
        (fun dest ->
          let acked = ref false in 
          let rec retry () = 
            let msg_id = State.next_msg_id state in 
            let body = Msg.Broadcast { msg_id = Some msg_id; message } in
            let msg: Msg.outbound_msg = { 
              src = state.node_id; 
              dest; 
              body
            } in 
            let handler ~acked (request_body: Msg.inbound_body) =
              match request_body with
              | Msg.BroadcastOk _ -> acked := true
              | b -> log ~state ("error: " ^ (Yojson.Safe.to_string (Msg.yojson_of_inbound_body b)))
            in
            rpc ~state body (handler ~acked);
            Eio.Time.sleep state.env#clock 1.0;
            if not !acked then retry ()
          in 
          retry ()
        ) neighbors;
  log ~state  "END GOSSIP"

let (let*) x f = Option.bind x f

let handle_rpc ~(state:_ State.state) (inbound_msg:Msg.inbound_msg) =
  let* msg_id  = Msg.msg_id_of_inbound_body inbound_msg.body in
  let* callback =  Eio.Mutex.use_rw ~protect:true state.locks.state (fun () ->
        Hashtbl.find_opt state.callbacks msg_id
      ) in
  callback inbound_msg.body;
  Some ()

let handle_message ~(state:_ State.state) (inbound_msg:Msg.inbound_msg) = 
  let* body = match inbound_msg.body with
    | Msg.Init b -> 
        Eio.Mutex.use_rw ~protect:true state.locks.state (fun () ->
          state.node_id <- b.node_id;
        );
        Msg.InitOk {
            msg_id = Atomic.get state.msg_id;
            in_reply_to = b.msg_id;
        } 
        |> Option.some
    | Msg.Echo b ->
        Msg.EchoOk {
            msg_id = Some (Atomic.get state.msg_id);
            in_reply_to = b.msg_id;
            echo = b.echo;
        }
        |> Option.some 
    | Msg.Generate b ->             
        Msg.GenerateOk {
            msg_id = Some (Atomic.get state.msg_id);
            in_reply_to = b.msg_id;
            id = state.node_id ^ string_of_int b.msg_id;
        } 
        |> Option.some 
    | Msg.Topology b ->            
        Eio.Mutex.use_rw ~protect:true state.locks.state (fun () ->
          state.neighbors <- State.StringSet.of_list (Hashtbl.find b.topology state.node_id);
        );
        Msg.TopologyOk {
            msg_id = Some (Atomic.get state.msg_id);
            in_reply_to = (match b.msg_id with Some id -> id | None -> 0);
        }
        |> Option.some
    | Msg.Broadcast b ->             
        let should_gossip =
          Eio.Mutex.use_rw ~protect:true state.locks.state (fun () ->
            if not (State.StringSet.mem b.message state.messages) then (
              state.messages <- State.StringSet.add b.message state.messages;
              true
            ) else false
          )
        in

        let response =
          match b.msg_id with
          | Some b_msg_id ->  
              let body = Msg.BroadcastOk {
                  msg_id = Some (Atomic.get state.msg_id);
                  in_reply_to = b_msg_id;
                } in
              Some body
          | None -> None
        in

        if should_gossip then
            gossip ~state ~message:b.message ~inbound_msg;
        response
    | Msg.Read b ->             
        Msg.ReadOk {
            msg_id = Some (Atomic.get state.msg_id);
            in_reply_to = (match b.msg_id with Some id -> id | None -> 0);
            messages = Eio.Mutex.use_ro state.locks.state (fun () -> State.StringSet.elements state.messages)
        } 
        |> Option.some
    | Msg.BroadcastOk b -> 
        None
    | Msg.Unknown b -> 
        None
  in
  
  send ~state (Higher.make_outbound_msg inbound_msg state body);
  Some ()


let maelstrom ~(state: _ State.state) =
  let buf = Eio.Buf_read.of_flow state.env#stdin ~max_size:4096 in
  Eio.Switch.run (fun sw ->
    let rec loop () =
      match Eio.Buf_read.line buf with
      | line ->
          log ~state ("[INBOUND]" ^ line);
          Eio.Fiber.fork ~sw (fun () ->
            line 
            |> Yojson.Safe.from_string
            |> Msg.inbound_msg_of_yojson
            |> fun inbound_msg -> List.iter (fun f -> ignore (f ~state inbound_msg)) [handle_rpc; handle_message]
            |> ignore
          );
          loop ()
      | exception End_of_file -> ()
    in
    loop ()
  )

let () =
  Eio_main.run @@ fun env ->
    maelstrom
      ~state: { 
      node_id = String.empty; 
      msg_id = Atomic.make 0; 
      messages = State.StringSet.empty; 
      neighbors = State.StringSet.empty; 
      callbacks = Hashtbl.create 0; 
      env = object
        method stdin = Eio.Stdenv.stdin env
        method stdout = Eio.Stdenv.stdout env
        method stderr = Eio.Stdenv.stderr env
        method clock = Eio.Stdenv.clock env
      end; 
      locks = {
        state = Eio.Mutex.create ();
        stdout = Eio.Mutex.create ();
        stderr = Eio.Mutex.create ();
      };
    }