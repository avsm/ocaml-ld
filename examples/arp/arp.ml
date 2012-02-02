(*TODO: only import the necessary bits*)
open Froc_sa
open Nettypes



module ARP : sig

  module M : (Map.S with type key = ipv4_addr)
  module S : (Set.S with type elt = ipv4_addr)

  val init: output:(arp -> unit) -> get_mac:(unit -> ethernet_mac) -> unit
  val cache: ethernet_mac M.t Froc_sa.t
  val input: arp -> unit
  val bound_ips: S.t Froc_sa.t
  val set_bound_ips: S.t -> unit
  val add_bound_ip: ipv4_addr -> unit
  val rem_bound_ip: ipv4_addr -> unit
  val send_request: ipv4_addr -> unit

end = struct

  module M =
  Map.Make (struct
    type t = ipv4_addr
    let compare = Pervasives.compare
  end)
  module S =
  Set.Make (struct
    type t = ipv4_addr
    let compare = Pervasives.compare
  end)

  let output_r = ref (fun _ -> ())
  let get_mac_r = ref (fun _ -> failwith "Not initialized")

  let init ~output ~get_mac =
    output_r := output;
    get_mac_r := get_mac

  let (bound_ips, bound_ips_u) = changeable ~eq:S.equal S.empty
  let (cache, cache_u) = changeable ~eq:(M.equal (=)) M.empty

  let input arp = match arp.op with
    |`Request ->
      (* Received ARP request, check if we can satisfy it from
        our own IPv4 list *)
      let req_ipv4 = arp.tpa in
      Printf.printf "ARP: who-has %s?\n%!" (ipv4_addr_to_string req_ipv4);
      if S.mem req_ipv4 (read bound_ips) then begin
        (* We own this IP, so reply with our MAC *)
        let sha = !get_mac_r () in
        let tha = arp.sha in
        let spa = arp.tpa in (* the requested address *)
        let tpa = arp.spa in (* the requesting host IPv4 *)
        !output_r { op=`Reply; sha; tha; spa; tpa }
      end else ()
    |`Reply ->
      Printf.printf "ARP: updating %s -> %s\n%!"
        (ipv4_addr_to_string arp.spa) (ethernet_mac_to_string arp.sha);
      write cache_u (M.add arp.spa arp.sha (read cache));
      propagate ()
    |`Unknown n ->
      Printf.printf "ARP: Unknown message %d ignored\n%!" n


  let set_bound_ips l =
      write bound_ips_u l; propagate ()
  let add_bound_ip ip =
      write bound_ips_u (S.add ip (read bound_ips)); propagate ()
  let rem_bound_ip ip =
      write bound_ips_u (S.remove ip (read bound_ips)); propagate ()

  let send_request tpa =
    Printf.printf "ARP: transmitting probe -> %s\n%!" (ipv4_addr_to_string tpa);
    let tha = ethernet_mac_broadcast in
    let sha = !get_mac_r () in
    (* Source protocol address, pick one of our IP addresses *)
    let spa =
      try S.choose (read bound_ips)
      with Not_found -> ipv4_blank
    in
    !output_r { op=`Request; tha; sha; tpa; spa }

end

