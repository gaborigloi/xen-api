
(** CA-272147: verify that SR.set_name_label and set_name_description work *)
let test_set_name () =
  let __context = Test_common.make_test_database () in

  let sr = Mock_storage.make_server_infrastructure ~__context in
  Db.SR.set_name_label ~__context ~self:sr ~value:"old_label";
  Db.SR.set_name_description ~__context ~self:sr ~value:"old_desc";

  let sr_name_label = ref None in
  Mock_storage.register_smapiv2_server
    ~sr_set_name_label:(fun _ ~dbg ~sr ~new_name_label -> sr_name_label := Some new_name_label)
    ~sr_stat:(fun _ ~dbg ~sr ->
        let r = Db.SR.get_record ~__context ~self:sr in
        Storage_interface.{ name_label = r.API.sR_name_label; name_description = r.API.sR_name_description; health = Healthy; total_space = 0L; free_space = 0L; clustered = false }
      )
    (Db.SR.get_uuid ~__context ~self:sr);

  Api_server.Forwarder.SR.set_name_label ~__context ~sr ~value:"new_label";
  Api_server.Forwarder.SR.set_name_description ~__context ~sr ~value:"new_desc";

  Alcotest.(check string) "SR name_label" "new_label" (Db.SR.get_name_label ~__context ~self:sr);
  Alcotest.(check string) "SR name_description" "new_desc" (Db.SR.get_name_description ~__context ~self:sr)

let test =
  [ "test_set_name", `Quick, test_set_name ]
