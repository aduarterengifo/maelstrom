let better ~(state : _ State.state) =
  let buf = Eio.Buf_read.of_flow state.env#stdin ~max_size:4096 in
  Eio.Switch.run (fun sw ->
      let rec loop () =
        match Eio.Buf_read.line buf with
        | line -> loop ()
        | exception End_of_file -> ()
      in
      loop ())
