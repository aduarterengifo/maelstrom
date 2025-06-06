let make_outbound_msg (inbound_msg: Msg.inbound_msg) (state: _ State.state) body: Msg.outbound_msg = {
  src = state.node_id; 
  dest = inbound_msg.src; 
  body;
}