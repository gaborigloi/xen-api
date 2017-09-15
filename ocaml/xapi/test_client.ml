
(** Returns a [(rpc, session_id)] pair that can be passed to the
    functions within the [Client] module to make XenAPI calls. The
    calls can only succeed if they get forwarded to the local host
    by the message forwarding layer. Forwarding to slaves does not
    work in unit tests. *)
let make_client_params ~__context =
	let req = Xmlrpc_client.xmlrpc ~version:"1.1" "/" in
	let rpc = Api_server.Server.dispatch_call req Unix.stdout in
	let session_id =
		let session_id = Ref.make () in
		let now = Stdext.Date.of_float (Unix.time ()) in
    let _: _ API.Ref.t = Test_common.make_session ~__context ~ref:session_id ~this_host:(Helpers.get_localhost ~__context) ~last_active:now ~is_local_superuser:true ~validation_time:now ~auth_user_name:"root" ~originator:"test" () in
		session_id
	in
	(rpc, session_id)
