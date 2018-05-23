(** Module for parsing and accessing the command line arguments *)

let http = Xmlrpc_client.xmlrpc ~version:"1.1" "/"

let rpc_remote host xml =
  Xmlrpc_client.XMLRPC_protocol.rpc
    ~srcstr:"quicktest" ~dststr:"xapi"
    ~transport:(SSL(Xmlrpc_client.SSL.make(), host, 443))
    ~http xml

let rpc_unix_domain xml =
  Xmlrpc_client.XMLRPC_protocol.rpc
    ~srcstr:"quicktest" ~dststr:"xapi"
    ~transport:(Unix Xapi_globs.unix_domain_socket)
    ~http xml

let init_session rpc username password =
  Client.Client.Session.login_with_password ~rpc ~uname:username ~pwd:password ~version:Datamodel_common.api_version_string ~originator:"quick_test"

(** These used to be CLI arguments but now are hardcoded *)

let xe_path = "/opt/xensource/bin/xe"
let iso_sr_path = "/opt/xensource/packages/iso"

type args =
  { using_unix_domain_socket: bool
  ; host: string
  ; username: string
  ; password: string
  ; rpc: Rpc.call -> Rpc.response
  ; session_id: API.ref_session
  ; use_default_sr: bool
  }

let get_args use_default_sr host username password =
  let login_args =
    match host, username, password with
    | None, None, None -> Ok ("localhost", "root", "", true, rpc_unix_domain)
    | Some host, Some username, Some password -> Ok (host, username, password, false, rpc_remote host)
    | _ -> Error (`Msg "For remote server all of the <hostname>, <username>, and <password> arguments have to be supplied.")
  in
  match login_args with
  | Ok (host, username, password, using_unix_domain_socket, rpc) ->
    let session_id = init_session rpc username password in
    Ok { using_unix_domain_socket; host; username; password; rpc; session_id; use_default_sr }
  | Error _ as e -> e

let args =
  let open Cmdliner in
  let use_default_sr =
    let doc = "Only run SR tests on the pool's default SR" in
    Arg.(value & flag & info ["use-default-sr"] ~doc)
  in
  let host =
    let doc =
      "The address of the host to connect to. The default is to test localhost over a Unix socket. For remote server supply the <hostname>, <username>, and <password> arguments."
    in
    Arg.(value & opt (some string) None & info ["h"; "host"] ~doc)
  in
  let username =
    let doc =
      "Username for the remote server. The default is to test localhost over a Unix socket. For remote server supply the <hostname>, <username>, and <password> arguments."
    in
    Arg.(value & opt (some string) None & info ["u"; "uname"; "username"] ~doc)
  in
  let password =
    let doc =
      "Password for the remote server. The default is to test localhost over a Unix socket. For remote server supply the <hostname>, <username>, and <password> arguments."
    in
    Arg.(value & opt (some string) None & info ["u"; "pwd"; "password"] ~doc)
  in
  Term.(const get_args $ use_default_sr $ host $ username $ password |> term_result)
