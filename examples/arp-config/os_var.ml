(* This is a hand-written configuration file for the associated ARP module. In
 * the future, it is hoped to automatically generating the boiler-plate code
 * associated to configuration variables.
 *)

module F = Froc_sa
module L = Lwt
let (>>=) = Lwt.(>>=)

let (local_mac, local_mac_writer) =
  (* In reality, this is configured by the OS, not a magic constant *)
  F.changeable Nettypes.ethernet_mac_broadcast

let set_mac mac =
  F.write local_mac_writer mac;
  F.propagate ()


let (debug_mode, debug_mode_writer) = F.changeable false
let set_debug_mode d = F.write debug_mode_writer d

let normal_network_out interface s =
  (* Normaly it is provided by the OS. *)
  Lwt_io.fprintf interface "%s\n" s

let debug_network_out interface s =
  Lwt_io.eprintf "NETWORK_OUT: %s\n" s >>= fun () ->
  normal_network_out interface s

let network_out =
  (* In reality, this is provided by the OS. *)
  F.bind ~eq:(==) debug_mode (fun b ->
    if b then
      F.return debug_network_out
    else
      F.return normal_network_out
  )



