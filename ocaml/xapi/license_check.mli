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
(**
 * Verifying whether the current license is still valid
 * @group Licensing
*)

(** The expiry date that is considered to be "never". *)
val never : float

(** Returns (Some date) if the host's license has an expiry date,
 *  otherwise returns None. *)
val get_expiry_date : __context:Context.t -> host:API.ref_host -> Stdext.Date.iso8601 option
