(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
(** Module that defines API functions for Pool objects
 * @group XenAPI functions
*)

(** {2 (Fill in Title!)} *)

val join :
  __context:Context.t ->
  master_address:string ->
  master_username:string -> master_password:string -> unit
val join_force :
  __context:Context.t ->
  master_address:string ->
  master_username:string -> master_password:string -> unit
val emergency_transition_to_master : __context:'a -> unit
val emergency_reset_master : __context:'a -> master_address:string -> unit
val recover_slaves : __context:Context.t -> API.ref_host list
exception Cannot_eject_master
val eject : __context:Context.t -> host:API.ref_host -> unit
val sync_database : __context:Context.t -> unit
val designate_new_master : __context:Context.t -> host:API.ref_host -> unit
val management_reconfigure : __context:Context.t -> network:API.ref_network -> unit
val initial_auth : __context:'a -> string
val is_slave : __context:Context.t -> host:'b -> bool
val hello :
  __context:Context.t ->
  host_uuid:string ->
  host_address:string -> [> `cannot_talk_back | `ok | `unknown_host ]
val create_VLAN :
  __context:Context.t ->
  device:string -> network:API.ref_network -> vLAN:int64 -> API.ref_PIF list
val create_VLAN_from_PIF :
  __context:Context.t ->
  pif:[ `PIF ] Ref.t ->
  network:API.ref_network -> vLAN:int64 -> [ `PIF ] Ref.t list
val slave_network_report :
  __context:'a ->
  phydevs:'b -> dev_to_mac:'c -> dev_to_mtu:'d -> slave_host:'e -> 'f list

(** {2 High availability (HA)} *)

val enable_ha :
  __context:Context.t ->
  heartbeat_srs:API.ref_SR list ->
  configuration:(string * string) list ->
  unit
val disable_ha : __context:Context.t -> unit
val ha_prevent_restarts_for : __context:Context.t -> seconds:int64 -> unit
val ha_failover_plan_exists : __context:Context.t -> n:int64 -> bool
val ha_compute_max_host_failures_to_tolerate : __context:Context.t -> int64
val ha_compute_hypothetical_max_host_failures_to_tolerate :
  __context:Context.t -> configuration:(API.ref_VM * string) list -> int64
val ha_compute_vm_failover_plan :
  __context:Context.t ->
  failed_hosts:API.ref_host list ->
  failed_vms:API.ref_VM list -> (API.ref_VM * (string * string) list) list
val create_new_blob :
  __context:Context.t ->
  pool:[ `pool ] Ref.t -> name:string -> mime_type:string -> public:bool -> [ `blob ] Ref.t
val set_ha_host_failures_to_tolerate :
  __context:Context.t -> self:[ `pool ] Ref.t -> value:int64 -> unit
val ha_schedule_plan_recomputation : __context:'a -> unit

(** {2 (Fill in Title!)} *)

val enable_binary_storage : __context:Context.t -> unit
val disable_binary_storage : __context:Context.t -> unit

(** {2 Workload balancing} *)

val initialize_wlb :
  __context:Context.t ->
  wlb_url:string ->
  wlb_username:string ->
  wlb_password:string ->
  xenserver_username:string -> xenserver_password:string -> unit
val deconfigure_wlb : __context:Context.t -> unit
val send_wlb_configuration :
  __context:Context.t -> config:(string * string) list -> unit
val retrieve_wlb_configuration :
  __context:Context.t -> (string * string) list
val retrieve_wlb_recommendations :
  __context:Context.t -> ([ `VM ] Ref.t * string list) list

(** {2 (Fill in Title!)} *)

val send_test_post :
  __context:Context.t -> host:string -> port:int64 -> body:string -> string
val certificate_install :
  __context:Context.t -> name:string -> cert:string -> unit
val certificate_uninstall : __context:Context.t -> name:string -> unit
val certificate_list : __context:'a -> string list
val crl_install : __context:Context.t -> name:string -> cert:string -> unit
val crl_uninstall : __context:Context.t -> name:string -> unit
val crl_list : __context:'a -> string list
val certificate_sync : __context:Context.t -> unit
val enable_external_auth :
  __context:Context.t ->
  pool:'a ->
  config:API.string_to_string_map ->
  service_name:string -> auth_type:string -> unit
val disable_external_auth :
  __context:Context.t -> pool:'a -> config:API.string_to_string_map -> unit
val detect_nonhomogeneous_external_auth : __context:'a -> pool:'b -> unit

(** {2 Redo log} *)

(** Enable redo log, independently from HA *)
val enable_redo_log : __context:Context.t -> sr:[`SR] Ref.t -> unit

(** Disable HA-independent redo log *)
val disable_redo_log : __context:Context.t -> unit

(** VSwitch Controller *)
val set_vswitch_controller : __context:Context.t -> address:string -> unit
val audit_log_append : __context:Context.t -> line:string -> unit

val test_archive_target : __context:Context.t -> self:API.ref_pool -> config:API.string_to_string_map -> string
val enable_local_storage_caching : __context:Context.t -> self:API.ref_pool -> unit
val disable_local_storage_caching : __context:Context.t -> self:API.ref_pool -> unit

val get_license_state : __context:Context.t -> self:API.ref_pool -> (string * string) list
val apply_edition : __context:Context.t -> self:API.ref_pool -> edition:string -> unit

val disable_ssl_legacy : __context:Context.t -> self:API.ref_pool -> unit
val enable_ssl_legacy : __context:Context.t -> self:API.ref_pool -> unit

val has_extension : __context:Context.t -> self:API.ref_pool -> name:string -> bool

val add_to_guest_agent_config :
  __context:Context.t -> self:API.ref_pool -> key:string -> value:string -> unit
val remove_from_guest_agent_config :
  __context:Context.t -> self:API.ref_pool -> key:string -> unit
