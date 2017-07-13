(*
 * Copyright (C) 2006-2012 Citrix Systems Inc.
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

open OUnit
open Test_common

(* Helpers for testing Xapi_vdi.check_operation_error *)

let setup_test ~__context ?sm_fun ?vdi_fun () =
  let run f x = match f with Some f -> ignore(f x) | None -> () in

  let _sm_ref = make_sm ~__context () in
  let sr_ref = make_sr ~__context () in
  let (_: API.ref_PBD) = make_pbd ~__context ~sR:sr_ref () in
  let vdi_ref = make_vdi ~__context ~sR:sr_ref () in
  run sm_fun _sm_ref;
  run vdi_fun vdi_ref;
  let vdi_record = Db.VDI.get_record_internal ~__context ~self:vdi_ref in
  vdi_ref, vdi_record

let my_cmp a b = match a,b with
  | Some aa, Some bb	-> fst aa = fst bb
  | None, None -> a = b
  | _	-> false

let string_of_api_exn_opt = function
  | None -> "None"
  | Some (code, args) ->
    Printf.sprintf "Some (%s, [%s])" code (String.concat "; " args)

let run_assert_equal_with_vdi ~__context ?(cmp = my_cmp) ?(ha_enabled=false) ?sm_fun ?vdi_fun op exc =
  let vdi_ref, vdi_record = setup_test ~__context ?sm_fun ?vdi_fun () in
  assert_equal
    ~cmp
    ~printer:string_of_api_exn_opt
    exc (Xapi_vdi.check_operation_error ~__context ha_enabled vdi_record vdi_ref op)

(* This is to test Xapi_vdi.check_operation_error against CA-98944
   code. This DO NOT fully test the aforementionned function *)
let test_ca98944 () =
  let __context = Mock.make_context_with_new_db "Mock context" in
  (* Should raise vdi_in_use *)
  run_assert_equal_with_vdi ~__context
    ~vdi_fun:(fun vdi_ref ->
        make_vbd ~vDI:vdi_ref ~__context
          ~reserved:true ~currently_attached:false ~current_operations:["", `attach] ())
    `update (Some (Api_errors.vdi_in_use, []));

  (* Should raise vdi_in_use *)
  run_assert_equal_with_vdi ~__context
    ~vdi_fun:(fun vdi_ref ->
        make_vbd ~vDI:vdi_ref
          ~__context ~reserved:false ~currently_attached:true ~current_operations:["", `attach] ())
    `update (Some (Api_errors.vdi_in_use, []));

  (* Should raise vdi_in_use *)
  run_assert_equal_with_vdi ~__context
    ~vdi_fun:(fun vdi_ref -> make_vbd ~vDI:vdi_ref
                 ~__context ~reserved:true ~currently_attached:true ~current_operations:["", `attach] ())
    `update (Some (Api_errors.vdi_in_use, []));

  (* Should raise other_operation_in_progress *)
  run_assert_equal_with_vdi ~__context
    ~vdi_fun:(fun vdi_ref -> make_vbd ~vDI:vdi_ref
                 ~__context ~reserved:false ~currently_attached:false ~current_operations:["", `attach] ())
    `update (Some (Api_errors.other_operation_in_progress, []));

  (* Should pass *)
  run_assert_equal_with_vdi ~__context
    ~vdi_fun:(fun vdi_ref -> make_vbd ~vDI:vdi_ref
                 ~__context ~reserved:false ~currently_attached:false ~current_operations:[] ())
    `forget None

(* VDI.copy should be allowed if all attached VBDs are read-only. *)
let test_ca101669 () =
  let __context = Mock.make_context_with_new_db "Mock context" in

  (* Attempting to copy a RW-attached VDI should fail with VDI_IN_USE. *)
  run_assert_equal_with_vdi ~__context
    ~vdi_fun:(fun vdi_ref ->
        make_vbd ~__context ~vDI:vdi_ref ~currently_attached:true ~mode:`RW ())
    `copy (Some (Api_errors.vdi_in_use, []));

  (* Attempting to copy a RO-attached VDI should pass. *)
  run_assert_equal_with_vdi ~__context
    ~vdi_fun:(fun vdi_ref ->
        make_vbd ~__context ~vDI:vdi_ref ~currently_attached:true ~mode:`RO ())
    `copy None;

  (* Attempting to copy an unattached VDI should pass. *)
  run_assert_equal_with_vdi ~__context `copy None;

  (* Attempting to copy RW- and RO-attached VDIs should fail with VDI_IN_USE. *)
  run_assert_equal_with_vdi ~__context
    ~vdi_fun:(fun vdi_ref ->
        let (_: API.ref_VBD) = make_vbd ~__context ~vDI:vdi_ref ~currently_attached:true ~mode:`RW () in
        make_vbd ~__context ~vDI:vdi_ref ~currently_attached:true ~mode:`RO ())
    `copy (Some (Api_errors.vdi_in_use, []))

let test_ca125187 () =
  let __context = Test_common.make_test_database () in

  (* A VDI being copied can be copied again concurrently. *)
  run_assert_equal_with_vdi ~__context
    ~vdi_fun:(fun vdi_ref ->
        let (_: API.ref_VBD) = make_vbd ~__context ~vDI:vdi_ref ~currently_attached:true ~mode:`RO () in
        Db.VDI.set_current_operations ~__context
          ~self:vdi_ref
          ~value:["mytask", `copy])
    `copy None;

  (* A VBD can be plugged to a VDI which is being copied. This is required as
     	 * the VBD is plugged after the VDI is marked with the copy operation. *)
  let _, _ = setup_test ~__context
      ~vdi_fun:(fun vdi_ref ->
          let host_ref = Helpers.get_localhost ~__context in
          let vm_ref = Db.Host.get_control_domain ~__context ~self:host_ref in
          let vbd_ref = Ref.make () in
          let (_: API.ref_VBD) = make_vbd ~__context
              ~ref:vbd_ref
              ~vDI:vdi_ref
              ~vM:vm_ref
              ~currently_attached:false
              ~mode:`RO () in
          Db.VDI.set_current_operations ~__context
            ~self:vdi_ref
            ~value:["mytask", `copy];
          Db.VDI.set_managed ~__context
            ~self:vdi_ref
            ~value:true;
          Xapi_vbd_helpers.assert_operation_valid ~__context
            ~self:vbd_ref
            ~op:`plug) ()
  in ()

let test_ca126097 () =
  let __context = Mock.make_context_with_new_db "Mock context" in

  (* Attempting to clone a VDI being copied should fail with VDI_IN_USE. *)
  run_assert_equal_with_vdi ~__context
    ~vdi_fun:(fun vdi_ref ->
        let (_: API.ref_VBD) = make_vbd ~__context ~vDI:vdi_ref ~currently_attached:true ~mode:`RO () in
        Db.VDI.set_current_operations ~__context
          ~self:vdi_ref
          ~value:["mytask", `copy])
    `clone None;

  (* Attempting to snapshot a VDI being copied should be allowed. *)
  run_assert_equal_with_vdi ~__context
    ~vdi_fun:(fun vdi_ref ->
        let (_: API.ref_VBD) = make_vbd ~__context ~vDI:vdi_ref ~currently_attached:true ~mode:`RO () in
        Db.VDI.set_current_operations ~__context
          ~self:vdi_ref
          ~value:["mytask", `copy])
    `snapshot (Some (Api_errors.operation_not_allowed, []))

(** Tests for the checks related to changed block tracking *)
let test_cbt =
  let all_cbt_operations = [`enable_cbt; `disable_cbt] in
  let for_vdi_operations ops f () = ops |> List.iter f in
  let for_cbt_enable_disable = for_vdi_operations [`enable_cbt; `disable_cbt] in

  let test_cbt_operations_not_allowed_when_sm_lacks_feature = for_vdi_operations all_cbt_operations (fun op ->
      let __context = Mock.make_context_with_new_db "Mock context" in
      run_assert_equal_with_vdi
        ~__context
        ~sm_fun:(fun sm -> Db.SM.remove_from_features ~__context ~self:sm ~key:"VDI_CONFIG_CBT")
        op
        (Some (Api_errors.sr_operation_not_supported, []))
    )
  in

  let test_cbt_enable_disable_not_allowed_on_snapshot = for_cbt_enable_disable (fun op ->
      let __context = Mock.make_context_with_new_db "Mock context" in
      run_assert_equal_with_vdi
        ~__context
        ~vdi_fun:(fun vdi -> Db.VDI.set_is_a_snapshot ~__context ~self:vdi ~value:true)
        op
        (Some (Api_errors.operation_not_allowed, []))
    )
  in

  let test_cbt_enable_disable_only_allowed_on_user_and_system_vdis = for_cbt_enable_disable (fun op ->
      let __context = Mock.make_context_with_new_db "Mock context" in
      run_assert_equal_with_vdi
        ~__context
        ~vdi_fun:(fun vdi -> Db.VDI.set_type ~__context ~self:vdi ~value:`metadata)
        op
        (Some (Api_errors.vdi_incompatible_type, []));
      run_assert_equal_with_vdi
        ~__context
        ~vdi_fun:(fun vdi -> Db.VDI.set_type ~__context ~self:vdi ~value:`redo_log)
        op
        (Some (Api_errors.vdi_incompatible_type, []));
      run_assert_equal_with_vdi
        ~__context
        ~vdi_fun:(fun vdi -> Db.VDI.set_type ~__context ~self:vdi ~value:`user)
        op
        None;
      run_assert_equal_with_vdi
        ~__context
        ~vdi_fun:(fun vdi -> Db.VDI.set_type ~__context ~self:vdi ~value:`system)
        op
        None
    )
  in

  let test_cbt_enable_disable_not_allowed_when_vdi_is_reset_on_boot = for_cbt_enable_disable (fun op ->
      let __context = Mock.make_context_with_new_db "Mock context" in
      run_assert_equal_with_vdi
        ~__context
        ~vdi_fun:(fun vdi -> Db.VDI.set_on_boot ~__context ~self:vdi ~value:`reset)
        op
        (Some (Api_errors.vdi_on_boot_mode_incompatible_with_operation, []))
    )
  in

  let test_cbt_enable_disable_can_be_performed_live = for_cbt_enable_disable (fun op ->
      let __context = Mock.make_context_with_new_db "Mock context" in
      run_assert_equal_with_vdi
        ~__context
        ~vdi_fun:(fun vdi -> Test_common.make_vbd ~__context ~vDI:vdi ~currently_attached:true ~mode:`RW ())
        op
        None
    )
  in

  let test_operations_that_should_be_disallowed_on_cbt_metadata_vdi = for_vdi_operations
      [`snapshot; `clone; `resize; `resize_online; `copy; `set_on_boot]
      (fun op ->
         let __context = Mock.make_context_with_new_db "Mock context" in
         run_assert_equal_with_vdi
           ~__context
           ~sm_fun:(fun sm -> Db.SM.set_features ~__context ~self:sm ~value:["VDI_SNAPSHOT",1L; "VDI_CLONE",1L; "VDI_RESIZE",1L; "VDI_RESIZE_ONLINE",1L; "VDI_RESET_ON_BOOT",2L])
           ~vdi_fun:(fun vdi -> Db.VDI.set_type ~__context ~self:vdi ~value:`cbt_metadata)
           op
           (Some (Api_errors.vdi_incompatible_type, []))
      )
  in

  let test_operations_that_should_be_disallowed_when_cbt_is_enabled = for_vdi_operations
      [`mirror; `set_on_boot]
      (fun op ->
         let __context = Mock.make_context_with_new_db "Mock context" in
         run_assert_equal_with_vdi
           ~__context
           ~sm_fun:(fun sm -> Db.SM.set_features ~__context ~self:sm ~value:["VDI_MIRROR",1L; "VDI_RESET_ON_BOOT",2L])
           ~vdi_fun:(fun vdi -> Db.VDI.set_cbt_enabled ~__context ~self:vdi ~value:true)
           op
           (Some (Api_errors.vdi_cbt_enabled, []))
      )
  in

  "test_cbt" >:::
  [ "test_cbt_operations_not_allowed_when_sm_lacks_feature" >:: test_cbt_operations_not_allowed_when_sm_lacks_feature
  ; "test_cbt_enable_disable_not_allowed_on_snapshot" >:: test_cbt_enable_disable_not_allowed_on_snapshot
  ; "test_cbt_enable_disable_only_allowed_on_user_and_system_vdis" >:: test_cbt_enable_disable_only_allowed_on_user_and_system_vdis
  ; "test_cbt_enable_disable_not_allowed_when_vdi_is_reset_on_boot" >:: test_cbt_enable_disable_not_allowed_when_vdi_is_reset_on_boot
  ; "test_cbt_enable_disable_can_be_performed_live" >:: test_cbt_enable_disable_can_be_performed_live
  ; "test_operations_that_should_be_disallowed_on_cbt_metadata_vdi" >:: test_operations_that_should_be_disallowed_on_cbt_metadata_vdi
  ; "test_operations_that_should_be_disallowed_when_cbt_is_enabled" >:: test_operations_that_should_be_disallowed_when_cbt_is_enabled
  ]

let test =
  "test_vdi_allowed_operations" >:::
  [
    "test_ca98944" >:: test_ca98944;
    "test_ca101669" >:: test_ca101669;
    "test_ca125187" >:: test_ca125187;
    "test_ca126097" >:: test_ca126097;
    test_cbt;
  ]
