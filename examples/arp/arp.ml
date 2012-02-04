(*TODO: only import the necessary bits*)
open Froc_sa
open Nettypes

(*TODO: Lwt-ise*)



module ARP : sig

  module M : (Map.S with type key = ipv4_addr)
  module S : (Set.S with type elt = ipv4_addr)

  (*If not initialised, other functions of this module might raise [Failure]*)
  val init: output:(arp -> unit) -> get_mac:(unit -> ethernet_mac) -> unit


  (* This is exposed for now, but should probably be hidden. As it is read-only,
   * the icentive to hide it is small.
   * Prefer [query] for querying a specific value.
   *)
  val cache: ethernet_mac M.t Froc_sa.t

  (* Use this when receiving an [arp] *)
  val input: arp -> unit

  (* The set of ip adresses local to this system. *)
  val bound_ips: S.t Froc_sa.t

  (* Use these to change the set of local ip addresses. *)
  val set_bound_ips: S.t -> unit
  val add_bound_ip: ipv4_addr -> unit
  val rem_bound_ip: ipv4_addr -> unit

  (* sends a arp request for the given ip address. This should be hidden as it
   * is automatically called when using [query].
   *)
  val send_request: ipv4_addr -> unit

  (* queries a given ip address. The returned changeable might initially be
   * [None], in which case a request is sent and the changeable will change when
   * the replies come.
   *)
  val query: ipv4_addr -> ethernet_mac option Froc_sa.t

  (* queries a given ip address and returns the current value in the arp cache.
   * If the value is not there (returning [None]) then a request is sent, except
   * if [with_req] is [false] (which is not by default).
   *)
  val query1: ?with_req:bool -> ipv4_addr -> ethernet_mac option

  val query_lwt: ipv4_addr -> ethernet_mac Lwt.t

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

  let never_eq _ _ = false

  (*Alternative: store changeables in the map*)
  let query_table table =
    return (fun eth ->
      try Some (M.find eth table)
      with Not_found ->
        send_request eth;
        None
    )

  let query = Memo.fn (bind ~eq:never_eq cache query_table)

  let query1 ?(with_req = true) eth =
    try Some (M.find eth (read cache))
    with Not_found ->
      (if with_req then send_request eth);
      None

  let query_lwt eth =
    Lwt_stream.next
      (Lwt_stream.filter_map (fun x -> x)
        (Froc_lwt.stream_of_changeable
          (query eth)
        )
      )

end

