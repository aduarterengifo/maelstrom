(* let main ~stdin ~stdout =
  let buf = Eio.Buf_read.of_flow stdin ~max_size:4096 in
  let rec loop id =
    match Eio.Buf_read.line buf with
    | line -> 
        let blah = Yojson.Safe.from_string line in
        (* let init_value = Msg_j.person_of_yojson line in *)
        (* let init_node_id = init_value.body.node_id in *)
        (* let init_reply : Msg_t.init_reply = {
          src = init_value.body.node_id; 
          dest = init_value.src; 
          body = {
            msg_id = 1;
            in_reply_to = init_value.body.msg_id;
            type_ = "init_ok";
          }; 
        } in *)
        Eio.Flow.copy_string (line ^ "\n") stdout;
        loop id
    | exception End_of_file -> ()
  in
  loop "NOTSET"


let () =
  Eio_main.run @@ fun env ->
    main
      ~stdin:(Eio.Stdenv.stdin env)
      ~stdout:(Eio.Stdenv.stdout env) *)
      