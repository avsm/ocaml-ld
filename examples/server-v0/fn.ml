let ( >>= ) = Lwt.( >>= )

(* This type is used to compare different implementations. Implementations are
 * transmitted as first-class modules (which have no structural equality). The
 * generic way to write eqtok is to have one variant per implementation with
 * names matching. Variants of implementations that maintain a state have an
 * argument for further equality. *)
type eqtok =
  | Echo
  | Uppercase
  | Lowercase


(* The spec of a reply server *)
module type INTERFACE = sig

  val f : string -> string

  val eqtok : unit -> eqtok

end

let eq m1 m2 =
  let module M1 = (val m1: INTERFACE) in
  let module M2 = (val m2: INTERFACE) in
  M1.eqtok () = M2.eqtok ()


module Echo : INTERFACE = struct

  external f : string -> string = "%identity"

  let eqtok () = Echo

end

module Uppercase : INTERFACE = struct

  let f = String.uppercase

  let eqtok () = Uppercase

end

module Lowercase : INTERFACE = struct

  let f = String.lowercase

  let eqtok () = Lowercase

end

let m_of_fn fn = match fn with
  | Conf.Echo -> (module Echo: INTERFACE)
  | Conf.Uppercase -> (module Uppercase: INTERFACE)
  | Conf.Lowercase -> (module Lowercase: INTERFACE)

let m =
  React.S.hold
    ~eq
    (m_of_fn (React.S.value Conf.fn))
    (React.E.map m_of_fn (React.S.changes Conf.fn))


let f s =
  let module M = (val (React.S.value m): INTERFACE) in
  M.f s

