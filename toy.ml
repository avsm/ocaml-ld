(* Compilation:
 * ocamlbuild -pkg lwt.unix -pkg react toy.native
 *
 * Or, in vim,
 * :!ocamlbuild -pkg lwt.unix -pkg react %:r.native
 *)


module Ld_helper = struct

  let make_val x =
    let (v, setter) = React.S.create x in
    let getter () = React.S.value v in
    (v, getter, setter)

end



(* This should be automatically generated from the conf file and default values
 * should be overwritten by command line arguments.
 *)
module Conf = struct

  let (o1, get_o1, set_o1) = Ld_helper.make_val 42

  type oneortwo = One | Two
  let (o2, get_o2, set_o2) = Ld_helper.make_val One

end


(* This section is listening to reconfiguration events. In the real world, this
 * would come from a named pipe (à la Ocsigen), a network interface or
 * stdin (as a control console).
 *)

module Reconf_event = struct
  open Lwt

  type ev =
    | First of int
    | Second of Conf.oneortwo

  let f (x, y) = Lwt_unix.sleep x >|= fun () -> match y with
    | First i -> Conf.set_o1 i
    | Second ot -> Conf.set_o2 ot

  let () = ignore_result (
    Lwt_list.iter_s f
      [ (1.0, First 22)
      ; (0.2, Second Conf.Two)
      ; (1.0, First 21)
      ; (1.0, First 19)
      ; (1.0, First 17)
      ; (1.0, First 15)
      ; (1.0, Second Conf.One)
      ; (1.0, First 4)
      ]
  )


end


(* This next part is advanced configuration. It contains secondary conf options.
 * Secondary conf options cannot be changed directly, they are automattically
 * inferred from other conf options (either primary or secondary).
 *)


module Foo : sig

  val get : unit -> string

end = struct

  (* To implement equality *)
  type eq_token =
    | Mod1 of int
    | Mod2 of string

  (*The interface*)
  module type SIG = sig
    val init : int -> unit
    val show_state: unit -> string
    val eq_token: unit -> eq_token
    val handover: unit -> int
  end

  (*The two implementations*)
  module ModSig1 : SIG = struct
    let state = ref 0
    let init i = state := i
    let show_state () = string_of_int !state
    let eq_token () = Mod1 !state
    let handover () = !state
  end

  module ModSig2 : SIG = struct
    let state = ref ""
    let init i = state := String.make i 'V'
    let show_state () = !state
    let eq_token () = Mod2 !state
    let handover () = String.length !state
  end

  let meq m1 m2 =
    let module M1 = (val m1:SIG) in
    let module M2 = (val m2:SIG) in
    (M1.eq_token ()) = (M2.eq_token ())


  (* Here is a time varying value. At anytime it is coherent with the
   * configuration. If [Conf.o2] changes, implementations are substituted. If
   * [Conf.o1] changes, the current implementation is (re-)initialised.
   *)

  type changes =
    | Implem of Conf.oneortwo
    | State of int

  (* [m] is not exported and doesn't have any (reactive) dependencies so
   * defining this correct [eq] function is not necessary.
   * [(fun _ _ -> false)] does the same.
   *)
  let m : (module SIG) React.S.t =
    React.S.fold
      ~eq:meq
      (fun m ev -> match ev with
        | State s ->
          let module M = (val m:SIG) in
          M.init s;
          m
        | Implem r ->
          let module M = (val m:SIG) in
          let h = M.handover () in
          match r with
          | Conf.One ->
            ModSig1.init h;
            (module ModSig1: SIG)
          | Conf.Two ->
            ModSig2.init h;
            (module ModSig2: SIG)
      )

      (match React.S.value Conf.o2 with
        | Conf.One ->
          (module ModSig1: SIG)
        | Conf.Two ->
          (module ModSig2: SIG)
      )

      (React.E.select
        [ React.E.map (fun x -> Implem x) (React.S.changes Conf.o2)
        ; React.E.map (fun x -> State  x) (React.S.changes Conf.o1)
        ]
      )


  (* Alternative: make another signal and just get the current value. *)
  let get () =
    let m = React.S.value m in
    let module M = (val m:SIG) in
    M.show_state ()

end


(* This is the part of the code that uses the configuration variables. *)

module Main = struct
  open Lwt

  let f x = Lwt_unix.sleep x >>= fun () -> Lwt_io.eprintl (Foo.get ())

  let () = Lwt_main.run (
    Lwt_list.iter_s f [ 1.0; 1.0; 2.0; 0.5; 1.0; 0.5; 1.0; 2.0; 0.5; 1.0; 0.5]
  )

end


