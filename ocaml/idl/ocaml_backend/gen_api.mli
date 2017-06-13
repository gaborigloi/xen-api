(** Provides functions to auto-generate various OCaml modules from the datamodel *)

val oc : out_channel ref
(** The generated files will be written to this output channel *)

val add_set_enums : Datamodel_types.ty list -> Datamodel_types.ty list

val gen_client : Dm_api.api -> unit

val gen_client_types : Dm_api.api -> unit

val gen_server : Dm_api.api -> unit

val gen_custom_actions : Dm_api.api -> unit

val gen_db_actions : Dm_api.api -> unit

val gen_rbac : Dm_api.api -> unit
