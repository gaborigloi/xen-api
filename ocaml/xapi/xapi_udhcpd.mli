
(* [maybe_add_lease __context vif]: if [vif] is on the Host internal
   management network then configure a DHCP lease via udhcpd *)
val maybe_add_lease: __context:Context.t -> API.ref_VIF -> unit

(* [init ()]: load in the saved leases database from disk, if there is one *)
val init: unit -> unit
