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
(*
---------------------------------------------------------------------------------

   Provides MTC-specific code to integrate with Citrix's XAPI Code

---------------------------------------------------------------------------------
*)

(*
 * -----------------------------------------------------------------------------
 *  Include other modules here.
 * -----------------------------------------------------------------------------
 *)
open Stdext
open Pervasiveext
open Printf

open Network

module DD=Debug.Make(struct let name="MTC:" end)
open DD

(*
 * -----------------------------------------------------------------------------
 *  Put global constants here.
 * -----------------------------------------------------------------------------
 *)


(*
 * -----------------------------------------------------------------------------
 *  Functions related to MTC peer and enabled feature.
 * -----------------------------------------------------------------------------
 *)
(*
 * MTC: Newly added value in a VM's other-config field that specifies (when true)
 * that this VM is protected.  By protection we mean high-availability or fault
 * tolerant protection.
 *)
let vm_protected_key = "vm_protected"
let vm_peer_uuid_key = "vm_peer_uuid"
let mtc_vdi_share_key = "mtc_vdi_shareable"

(*
* This function looks at the 'other-config' field in the VM's configuration
* database to determine if the UUID of its peer VM is specified.  If it is,
* then it returns that value, otherwise, it returns None.
*)
let get_peer_vm_uuid ~__context ~self =
  try Some (List.assoc vm_peer_uuid_key (Db.VM.get_other_config ~__context ~self)) with _ -> None


(*
 * This function looks in the configuration database and examines
 * the record of the provided VM to see if a peer VM is specified.
 * If a peer VM is specified, it returns its VM reference object
 * representation.  Otherwise, it returns a null VM.
 *)
let get_peer_vm ~__context ~self =

  (* Extract the UUID from the configuration. Returns None if not available *)
  let uuid_str_op = get_peer_vm_uuid ~__context ~self in

  (* If a VM peer was found, then look up in the database the VM's record
   * using the VM's UUID field as a key.
  *)
  match uuid_str_op with
    Some uuid ->
    (* debug "VM %s has a peer VM UUID of %s" (Db.VM.get_uuid ~__context ~self) uuid; *)
    Db.VM.get_by_uuid ~__context ~uuid
  | None ->
    Ref.null


(*
 * This function looks at the 'other-config' field in the VM's configuration
 * database to determine if the 'vm_protected' key is present AND set to true.
 * It will return true if both of these conditions exist.
 *)
let is_this_vm_protected ~__context ~self =
  try
    let other_config = Db.VM.get_other_config ~__context ~self in
    let protected = ((List.mem_assoc vm_protected_key other_config) &&
                     (List.assoc vm_protected_key other_config)="true") in
    protected
  with _ -> false

(*
 * This routine is invoke when a request for a migration is received
 * at the destination side.  It figures out the correct VM configuration
 * to be used to instantiate a VM to receive the migrated data.  The
 * logic says that if the source VM is a protected VM, then we'll
 * look up its peer VM (the destination) and return that VM to be instantiated.
 * If it's not protected, then the VM reference returned is that of the
 * source as this is a normal XenSource migration.
*)
let get_peer_vm_or_self ~__context ~self =
  try
    if (is_this_vm_protected ~__context ~self) then (
      let peer_vm = get_peer_vm ~__context ~self in
      if peer_vm <> Ref.null then
        peer_vm
      else (
        error "MTC: VM %s was found to be protected but it lacked its peer VM specification"
          (Db.VM.get_uuid ~__context ~self);
        self
      )
    )
    else self;
  with _ -> self


(*
 * -----------------------------------------------------------------------------
 *  Miscellaneous Functions
 * -----------------------------------------------------------------------------
 *)
(* Returns true if VDI is accessed by an MTC-protected VM *)
let is_vdi_accessed_by_protected_VM ~__context ~vdi =

  let uuid = Uuid.of_string (Db.VDI.get_uuid ~__context ~self:vdi) in

  let protected_vdi = List.mem_assoc mtc_vdi_share_key (Db.VDI.get_other_config ~__context ~self:vdi) in

  (* Return TRUE if this VDI is attached to a protected VM *)
  if protected_vdi then begin
    debug "VDI %s is attached to a Marathon-protected VM" (Uuid.to_string uuid);
    true
  end else
    false
