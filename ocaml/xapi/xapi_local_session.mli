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
(** Represents local sessions, for use in emergency mode *)

val create: __context:Context.t -> pool:bool -> API.ref_session

val destroy: __context:Context.t -> self:API.ref_session -> unit

val local_session_hook: __context:Context.t -> session_id:API.ref_session -> bool
