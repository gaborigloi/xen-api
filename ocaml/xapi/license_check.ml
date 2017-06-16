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
module L = Debug.Make(struct let name="license" end)

let never, _ =
  let start_of_epoch = Unix.gmtime 0. in
  Unix.mktime {start_of_epoch with Unix.tm_year = 130}

let get_expiry_date ~__context ~host =
  let license = Db.Host.get_license_params ~__context ~self:host in
  if List.mem_assoc "expiry" license
  then Some (Stdext.Date.of_string (List.assoc "expiry" license))
  else None
