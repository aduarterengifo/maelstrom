open Bag
module BetterMsg = Better_msg

let client_incoming_init_msg =
  let json_str =
    {|{
    "id": 1,
    "src": "c1", 
    "dest": "n1",
    "body": {
      "type": "init",
      "msg_id": 100,
      "node_id": "n1",
      "node_ids": ["n1", "n2", "n3"]
    }
  }|}
  in
  let json = Yojson.Safe.from_string json_str in
  let msg = BetterMsg.client_inbound_msg_of_yojson json in

  (* Verify the outer message structure *)
  assert (msg.id = 1);
  assert (msg.src = "c1");
  assert (msg.dest = "n1");

  (* Verify the init body *)
  match msg.body with
  | BetterMsg.Init { msg_id; node_id; node_ids } ->
      assert (msg_id = 100);
      assert (node_id = "n1");
      assert (node_ids = [ "n1"; "n2"; "n3" ]);
      print_endline "✓ client_incoming_init_msg test passed"
  | _ -> failwith "Expected Init message type"

let () = client_incoming_init_msg
