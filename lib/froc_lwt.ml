
let stream_of_changeable c =
  let (s, push) = Lwt_stream.create () in
  let really_push x = push (Some x) in
  let forward = Froc_sa.lift really_push c in (*How to make it uncollectable?*)
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

