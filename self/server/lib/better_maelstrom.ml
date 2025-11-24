open Ppx_yojson_conv_lib.Yojson_conv.Primitives

let ( let* ) x f = Option.bind x f

let gossip ~(state : _ State.state) ~message ~(inbound_msg : Msg.inbound_msg) =
  let neighbors =
    inbound_msg.src |> State.StringSet.singleton
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
        let msg : Msg.outbound_msg = { src = state.node_id; dest; body } in
        send ~state msg;
        let handler ~acked (request_body : Msg.peer_msg) =
          match request_body with Msg.BroadcastOk _ -> acked := true
        in
        rpc ~state body (handler ~acked);
        Eio.Time.sleep state.env#clock 1.0;
        if not !acked then retry ()
      in
      retry ())
    neighbors;
  log ~state "END GOSSIP"

let handle_peer_msg ~sw ~(state : _ State.state)
    (peer_msg : Better_msg.peer_msg) =
  let* body =
    match peer_msg.body with
    | Better_msg.PeerBroadcast b -> None
    | Better_msg.PeerBroadcastOk b ->
        let* msg_id = b.msg_id in
        Eio.Mutex.use_rw ~protect:true state.locks.state (fun () ->
            Hashtbl.find_opt state.callbacks msg_id
            |> Option.iter (fun callback -> callback peer_msg));
        Some ()
  in
  Some ()

let handle_client_msg ~sw ~(state : _ State.state)
    (inbound_msg : Better_msg.client_inbound_msg) =
  let* body =
    match inbound_msg.body with
    | Better_msg.Init b ->
        Eio.Mutex.use_rw ~protect:true state.locks.state (fun () ->
            state.node_id <- b.node_id;
            state.nodes <- State.StringSet.of_list b.node_ids);
        Better_msg.InitOk
          { msg_id = Atomic.get state.msg_id; in_reply_to = b.msg_id }
        |> Option.some
    | Better_msg.Echo b ->
        Better_msg.EchoOk
          {
            msg_id = Some (Atomic.get state.msg_id);
            in_reply_to = b.msg_id;
            echo = b.echo;
          }
        |> Option.some
    | Better_msg.Generate b ->
        Better_msg.GenerateOk
          {
            msg_id = Some (Atomic.get state.msg_id);
            in_reply_to = b.msg_id;
            id = state.node_id ^ string_of_int b.msg_id;
          }
        |> Option.some
    | Better_msg.Topology b ->
        Eio.Mutex.use_rw ~protect:true state.locks.state (fun () ->
            state.neighbors <-
              State.StringSet.of_list (Hashtbl.find b.topology state.node_id));
        Better_msg.TopologyOk
          {
            msg_id = Some (Atomic.get state.msg_id);
            in_reply_to = (match b.msg_id with Some id -> id | None -> 0);
          }
        |> Option.some
    | Better_msg.Broadcast b ->
        let should_gossip =
          Eio.Mutex.use_rw ~protect:true state.locks.state (fun () ->
              if not (State.StringSet.mem b.message state.messages) then (
                state.messages <- State.StringSet.add b.message state.messages;
                true)
              else false)
        in

        let response =
          match b.msg_id with
          | Some b_msg_id ->
              let body =
                Msg.BroadcastOk
                  { msg_id = Some b_msg_id; in_reply_to = b_msg_id }
              in
              Some body
          | None -> None
        in
        (* handle this in pixie land where everything is pure and there is not dust and keep going *)
        (* if (should_gossip) then
          Eio.Fiber.fork_daemon ~sw (fun () -> gossip ~state ~message:b.message ~inbound_msg; `Stop_daemon);
        response *)
        None
    | Better_msg.Read b ->
        Better_msg.ReadOk
          {
            msg_id = Some (Atomic.get state.msg_id);
            in_reply_to = (match b.msg_id with Some id -> id | None -> 0);
            messages =
              Eio.Mutex.use_ro state.locks.state (fun () ->
                  State.StringSet.elements state.messages);
          }
        |> Option.some
  in

  (* send ~state (Higher.make_outbound_msg inbound_msg state body); *)
  Some ()

let maelstrom ~(state : _ State.state) =
  let buf = Eio.Buf_read.of_flow state.env#stdin ~max_size:4096 in
  Eio.Switch.run (fun sw ->
      let rec loop () =
        match Eio.Buf_read.line buf with
        | line ->
            Eio.Fiber.fork ~sw (fun () ->
                line |> Yojson.Safe.from_string
                |> Better_msg.inbound_msg_of_yojson
                |> function
                | ClientInbound c_i_msg ->
                    handle_client_msg ~sw ~state c_i_msg |> ignore
                | Peer p_msg -> handle_peer_msg ~sw ~state p_msg |> ignore);
            loop ()
        | exception End_of_file -> ()
      in
      loop ())

let cowboy =
  Eio_main.run @@ fun env ->
  maelstrom
    ~state:
      {
        node_id = String.empty;
        msg_id = Atomic.make 0;
        messages = State.StringSet.empty;
        neighbors = State.StringSet.empty;
        nodes = State.StringSet.empty;
        callbacks = Hashtbl.create 0;
        env =
          object
            method stdin = Eio.Stdenv.stdin env
            method stdout = Eio.Stdenv.stdout env
            method stderr = Eio.Stdenv.stderr env
            method clock = Eio.Stdenv.clock env
          end;
        locks =
          {
            state = Eio.Mutex.create ();
            stdout = Eio.Mutex.create ();
            stderr = Eio.Mutex.create ();
          };
      }
