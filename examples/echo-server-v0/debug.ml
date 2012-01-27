let (>>=) = Lwt.(>>=)

type eqtok =
  | Full
(*   | Before *)
(*   | After *)
  | None

module type INTERFACE = sig

  val before: string -> unit Lwt.t
  val after : string -> unit Lwt.t

  val eqtok: unit -> eqtok

end

module Full : INTERFACE = struct

  let before s =
    Lwt_io.eprintf "Input: %s\n" s >>= fun () ->
    Lwt_io.flush Lwt_io.stderr

  let after s =
    Lwt_io.eprintf "Output: %s\n" s >>= fun () ->
    Lwt_io.flush Lwt_io.stderr

  let eqtok () = Full

end

module None : INTERFACE = struct

  let before _ = Lwt.return ()
  let after  _ = Lwt.return ()

  let eqtok () = None

end

let eq m1 m2 =
  let module M1 = (val m1: INTERFACE) in
  let module M2 = (val m2: INTERFACE) in
  M1.eqtok () = M2.eqtok ()

let m_of_debug d =
  if d then
    (module Full: INTERFACE)
  else
    (module None: INTERFACE)

let m =
  React.S.hold
    ~eq
    (m_of_debug (React.S.value Conf.debug))
    (React.E.map m_of_debug (React.S.changes Conf.debug))


let before s =
  let module M = (val (React.S.value m): INTERFACE) in
  M.before s

let after s =
  let module M = (val (React.S.value m): INTERFACE) in
  M.after s

