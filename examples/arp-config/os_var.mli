
val local_mac: Nettypes.ethernet_mac Froc_sa.t
val network_out: (Lwt_io.output_channel -> string -> unit Lwt.t) Froc_sa.t

val set_mac: Nettypes.ethernet_mac -> unit
val set_debug_mode: bool -> unit

