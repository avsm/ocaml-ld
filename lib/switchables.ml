open Froc_sa

(* Is it possinble w/ calling neither changeable, read, nor write? *)
(* Yes! Now, is it possible without a ref? *)
let adapting ?eq adapt a b =
  let copy = ref a in
  bind ?eq b (fun b ->
    let next = adapt !copy b in
    copy := next;
    return next
  )

let selfcleaning = lift

