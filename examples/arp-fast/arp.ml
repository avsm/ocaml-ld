(* This is a faster version of ../arp/arp.ml. The interface is restricted
 * becauseof the difference in implementation.
 *)


(*TODO: only import the necessary bits*)
open Froc_sa
open Nettypes

module ARP : sig

  (*If not initialised, other functions of this module might raise [Failure]*)
  val init: output:(arp -> unit) -> get_mac:(unit -> ethernet_mac) -> unit


  (* Use this when receiving an [arp] *)
  val input: arp -> unit

  (* The set of ip adresses local to this system. *)
  val bound_ips: S.t Froc_sa.t

  (* Use these to change the set of local ip addresses. *)
  val set_bound_ips: S.t -> unit
  val add_bound_ip: ipv4_addr -> unit
  val rem_bound_ip: ipv4_addr -> unit

  (* queries a given ip address. The returned changeable might initially be
   * [None], in which case a request is sent and the changeable will change when
   * the replies come.
   *)
  val query: ipv4_addr -> ethernet_mac option Froc_sa.t

end = struct

sig
