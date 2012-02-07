(* This is a faster version of ../arp/arp.ml. It is also more Lwt-ised *)


(*TODO: only import the necessary bits*)
open Froc_sa
open Nettypes

module ARP : sig

  (*If not initialised, other functions of this module might raise [Failure]*)
  val init: output:(arp -> unit) -> get_mac:(unit -> ethernet_mac) -> unit


  (* Use this when receiving an [arp] *)
  val input: arp -> unit

  (* The set of ip adresses local to this system. *)
  val bound_ips: ipv4_addr list Froc_sa.t

  (* Use these to change the set of local ip addresses. *)
  val set_bound_ips: ipv4_addr list -> unit
  val add_bound_ip: ipv4_addr -> unit
  val rem_bound_ip: ipv4_addr -> unit

  (* queries a given ip address. The returned changeable might initially be
   * [None], in which case a request is sent and the changeable will change when
   * the replies come.
   *)
  val query: ?timeout:float -> ipv4_addr -> ethernet_mac option Froc_sa.t

end = struct

  (*TODO? propose a patch to Lwt for this combinator?*)
  (*TODO? propose a patch to Lwt for 'a t list -> ('a -> 'a t list -> 'b t) -> 'b t *)
  let pick_and_do t1 t2 h1 h2 =
    let open Lwt in
    choose
      [ (t1 >|= fun x1 -> `One x1)
      ; (t2 >|= fun x2 -> `Two x2)
      ] >>= function
    | `One x1 -> h1 x1 t2
    | `Two x2 -> h2 t1 x2


  module H = Hashtbl

  let output_r = ref (fun _ -> ())
  let get_mac_r = ref (fun _ -> failwith "Not initialized")

  let init ~output ~get_mac =
    output_r := output;
    get_mac_r := get_mac

  let (bound_ips, bound_ips_u) = changeable []
  let cache = H.create 10

  let set_bound_ips l =
      write bound_ips_u l; propagate ()
  let add_bound_ip ip = (* No duplication list *)
      write bound_ips_u (ip :: (read bound_ips)); propagate ()
  let rem_bound_ip ip =
      write bound_ips_u (List.filter ((<>) ip) (read bound_ips)); propagate ()

  let send_request tpa =
    Printf.printf "ARP: transmitting probe -> %s\n%!" (ipv4_addr_to_string tpa);
    let tha = ethernet_mac_broadcast in
    let sha = !get_mac_r () in
    (* Source protocol address, pick one of our IP addresses *)
    let spa = match read bound_ips with
      | [] -> ipv4_blank
      | h :: _ -> h
    in
    !output_r { op=`Request; tha; sha; tpa; spa }

  let remove eth =
    try
      let (_, u) = H.find cache eth in
      H.remove cache eth;
      write u None;
      propagate ()
    with Not_found ->
      ()

  let query ?(timeout = 10.) eth =
    try fst (H.find cache eth)
    with Not_found ->
      let (t,u) as tu = changeable None in
      H.add cache eth tu;
      send_request eth;
      Lwt.ignore_result (
        pick_and_do
          (Lwt_stream.next
            (Lwt_stream.filter_map (fun x -> x)
              (Froc_lwt.stream_of_changeable t)
            )
          )
          (Lwt_unix.sleep timeout)
          (fun _ tt -> Lwt.cancel tt; Lwt.return ())
          (fun rt t -> remove eth; Lwt.cancel rt; Lwt.return ())
      );
      t

  let input arp = match arp.op with
    |`Request ->
      (* Received ARP request, check if we can satisfy it from
        our own IPv4 list *)
      let req_ipv4 = arp.tpa in
      Printf.printf "ARP: who-has %s?\n%!" (ipv4_addr_to_string req_ipv4);
      if List.mem req_ipv4 (read bound_ips) then begin
        (* We own this IP, so reply with our MAC *)
        let sha = !get_mac_r () in
        let tha = arp.sha in
        let spa = arp.tpa in (* the requested address *)
        let tpa = arp.spa in (* the requesting host IPv4 *)
        !output_r { op=`Reply; sha; tha; spa; tpa }
      end
    |`Reply ->
      Printf.printf "ARP: updating %s -> %s\n%!"
        (ipv4_addr_to_string arp.spa) (ethernet_mac_to_string arp.sha);
      begin
        try
          (write (snd (H.find cache arp.spa)) (Some arp.sha));
          propagate ()
        with Not_found ->
          let (t,u) as tu = changeable (Some arp.sha) in
          H.add cache arp.spa tu
      end
    |`Unknown n ->
      Printf.printf "ARP: Unknown message %d ignored\n%!" n


  (*FIXME: currently relies on the number of bound ips to be small. *)
      (* Solution: have two implementations (one for less than 20 bound ip
       * addresses and one for more than 15) and dynamically swap between tham.
       *)


end
