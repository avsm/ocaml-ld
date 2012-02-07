
(*DOs and DONTs (although, only DONTs right now
 *
 * DONT expose writables (because it exposes the next DONT to the user)
 * DONT change and yield (or otherwise switch thread)
 *      This can lead to partial changes being propagated. If you need to change
 *      several values before propagation, treat all the changes as a critical
 *      section, and make sure you propagate before yielding.
 * DONT bypass Froc_sa.bind by creating a new changeable, manually write and
 *      propagate.
 * DONT implement events over changeables. This restriction ensures that the
 *      whole system (in particullar, the data-plan) will be reactive (although
 *      theoretically appealing, it is better, for performance reasons, to
 *      restrict the reactive part to the configuration-plan).
 *)


let stream_of_changeable c =
  let (s, push) = Lwt_stream.create () in
  let really_push x = push (Some x) in
  let forward = Froc_sa.lift really_push c in
  Gc.finalise (fun _ -> ignore forward) s; (*To avoid garbage collection*)
  s

(*
let stream_of_changeable_while c f =
  let (s, push) = Lwt_stream.create () in
  let do_push x =
    if f x then
      push (Some x)
    else
      push None
  in
  let forward = Froc_sa.lift really_push c in (*How to make it uncollectable?*)
  s
*)

