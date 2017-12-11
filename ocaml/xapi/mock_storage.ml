
let register_smapiv2_server (module S: Storage_interface.Server_impl with type context = unit) sr_uuid =
  let module S = Storage_interface.Server(S) in
  let rpc = S.process () in
  let dummy_query_result = Storage_interface.({ driver=""; name=""; description=""; vendor=""; copyright=""; version=""; required_api_version=""; features=[]; configuration=[]; required_cluster_stack=[] }) in
  Storage_mux.register sr_uuid rpc "" dummy_query_result

let make_smapiv2_storage_server ?vdi_enable_cbt ?vdi_disable_cbt ?vdi_list_changed_blocks ?vdi_data_destroy ?vdi_snapshot ?vdi_clone ?sr_set_name_label ?sr_stat () =
  let default = Xapi_stdext_monadic.Opt.default in
  (module struct
    include (Storage_skeleton: module type of Storage_skeleton with module VDI := Storage_skeleton.VDI and module SR := Storage_skeleton.SR)
    module VDI = struct
      include Storage_skeleton.VDI
      let enable_cbt = default Storage_skeleton.VDI.enable_cbt vdi_enable_cbt
      let disable_cbt = default Storage_skeleton.VDI.disable_cbt vdi_disable_cbt
      let list_changed_blocks  = default Storage_skeleton.VDI.list_changed_blocks vdi_list_changed_blocks
      let data_destroy = default Storage_skeleton.VDI.data_destroy vdi_data_destroy
      let snapshot = default Storage_skeleton.VDI.snapshot vdi_snapshot
      let clone = default Storage_skeleton.VDI.clone vdi_snapshot
    end
    module SR = struct
      include Storage_skeleton.SR
      let set_name_label = default Storage_skeleton.SR.set_name_label sr_set_name_label
    end
  end : Storage_interface.Server_impl with type context = unit)

let register_smapiv2_server ?vdi_enable_cbt ?vdi_disable_cbt ?vdi_list_changed_blocks ?vdi_data_destroy ?vdi_snapshot ?vdi_clone ?sr_set_name_label ?sr_stat sr_uuid =
  let s = make_smapiv2_storage_server ?vdi_enable_cbt ?vdi_disable_cbt ?vdi_list_changed_blocks ?vdi_data_destroy ?vdi_snapshot ?vdi_clone ?sr_set_name_label ?sr_stat () in
  register_smapiv2_server s sr_uuid

(** Create host -> (SM) -> PBD -> SR infrastructure for mock storage layer, return SR. Assumes that there is already a local host set up in the DB. *)
let make_server_infrastructure ~__context =
  let host = Helpers.get_localhost ~__context in
  let sr_type = "test_sr_type" in
  (* This SM instance has CBT capabilities by default *)
  let _: _ API.Ref.t = Test_common.make_sm ~__context ~_type:sr_type () in
  let sR = Test_common.make_sr ~__context ~_type:sr_type ~is_tools_sr:false () in
  let _: _ API.Ref.t = Test_common.make_pbd ~__context ~host ~sR ~currently_attached:true () in
  sR
