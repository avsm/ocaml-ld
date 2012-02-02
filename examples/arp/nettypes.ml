(*
 * Copyright (c) 2010 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

type bytes = string (* to differentiate from pretty-printed strings *)
type ethernet_mac = string (* length 6 only *)

(* Raw MAC address off the wire (network endian) *)
let ethernet_mac_of_bytes x =
    assert(String.length x = 6);
    x

(* Read a MAC address colon-separated string *)
let ethernet_mac_of_string x =
    try
      let s = String.create 6 in
      Scanf.sscanf x "%2x:%2x:%2x:%2x:%2x:%2x"
       (fun a b c d e f ->
         s.[0] <- Char.chr a;
         s.[1] <- Char.chr b;
         s.[2] <- Char.chr c;
         s.[3] <- Char.chr d;
         s.[4] <- Char.chr e;
         s.[5] <- Char.chr f;
       );
       Some s
    with _ -> None

let ethernet_mac_to_string x =
    let chri i = Char.code x.[i] in
    Printf.sprintf "%02x:%02x:%02x:%02x:%02x:%02x"
       (chri 0) (chri 1) (chri 2) (chri 3) (chri 4) (chri 5)

let ethernet_mac_to_bytes x = x

let ethernet_mac_broadcast = String.make 6 '\255'

type ipv4_addr = int32

let ipv4_addr_of_tuple (a,b,c,d) =
  let in_range x = Int32.zero <= x && x <= 255l in
  assert (in_range a);
  assert (in_range b);
  assert (in_range c);
  assert (in_range d);
   let (+) = Int32.add in
   (Int32.shift_left a 24) +
   (Int32.shift_left b 16) + 
   (Int32.shift_left c 8) + d
 
(* Read an IPv4 address dot-separated string *)
let ipv4_addr_of_string x =
    let ip = ref None in
    (try Scanf.sscanf x "%ld.%ld.%ld.%ld"
      (fun a b c d -> ip := Some (ipv4_addr_of_tuple (a,b,c,d)));
    with _ -> ());
    !ip

(* Blank 0.0.0.0 IPv4 address *)
let ipv4_blank = 0l
(* Broadcast 255.255.255.255 IPv4 address *)
let ipv4_broadcast = ipv4_addr_of_tuple (255l,255l,255l,255l)
(* Localhost 127.0.0.1 ipv4 address  *)
let ipv4_localhost = ipv4_addr_of_tuple (127l,0l,0l,1l)

let ipv4_addr_to_string s =
    let (>!) x y = Int32.to_int (Int32.logand (Int32.shift_right x y) 255l) in
    Printf.sprintf "%d.%d.%d.%d" (s >! 24) (s >! 16) (s >! 8) (s >! 0)

external ipv4_addr_of_uint32: int32 -> ipv4_addr = "%identity"
external ipv4_addr_to_uint32: ipv4_addr -> int32 = "%identity"

type ipv4_src = ipv4_addr option * int
type ipv4_dst = ipv4_addr * int

type arp = {
  op: [ `Request |`Reply |`Unknown of int ];
  sha: ethernet_mac;
  spa: ipv4_addr;
  tha: ethernet_mac;
  tpa: ipv4_addr;
}


