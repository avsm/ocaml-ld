let ( >>= ) = Lwt.( >>= )

type eqtok =
  | Debug
  | Plain

module type INTERFACE = sig

  val f : string -> string

  val eqtok : unit -> eqtok

end

let eq m1 m2 =
  let module M1 = (val m1: INTERFACE) in
  let module M2 = (val m2: INTERFACE) in
  M1.eqtok () = M2.eqtok ()

(*
module Eq = struct

  type 'a t = (module S)

  let equal m1 m2 =
    let module M1 = (val m1: INTERFACE) in
    let module M2 = (val m2: INTERFACE) in
    M1.eq_token () = M2.eq_token ()

end

module S = React.S.Make(Eq)
*)

module Debug : INTERFACE = struct

  let f s =
    Lwt.ignore_result (Lwt_io.eprintf "Echoing: %s" s);
    s

  let eqtok () = Debug

end

module Plain : INTERFACE = struct

  external f : string -> string = "%identity"

  let eqtok () = Plain

end

let m_of_debug d =
  if d then
    (module Debug: INTERFACE)
  else
    (module Plain: INTERFACE)

let m =
  React.S.hold
    ~eq
    (m_of_debug (React.S.value Conf.debug))
    (React.E.map m_of_debug (React.S.changes Conf.debug))


let linewise_map ic f oc =
  Lwt_io.write_lines oc (Lwt_stream.map f (Lwt_io.read_lines ic))


let f (ic, oc) =
  (* right now, we don't switch the mode of already openned connections. *)
  let module M = (val (React.S.value m): INTERFACE) in
  Lwt.ignore_result (linewise_map ic M.f oc)

