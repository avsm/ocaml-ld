(* Memoisation with event triggered cache invalidation *)

(* Memoizing a signal function. *)
let memo_fun f =
  let cache = Hashtbl.create 42 in
  let e = React.E.map (fun _ -> Hashtbl.clear cache) (React.S.changes f) in
  fun arg ->
    try Hashtbl.find cache arg
    with Not_found ->
      let v = (React.S.value f) arg in
      Hashtbl.add cache arg v;
      ignore e;
      v

(* Memoizing a value: ~ making a ref of it. *)
(* Is it faster?
   The original [value] function is
   [
   let value = function 
     | Const v | Smut { sv = Some v }  -> v
     | Smut { sv = None } -> failwith err_sig_undef
   ]

   Tests needed.
 *)
let ref_of_signal v =
  let r = ref (React.S.value v) in
  let e = React.E.map (fun v -> r := v) (React.S.changes v) in
  let `R c = React.S.retain v (fun () -> ()) in
  let `R _ = React.S.retain v (fun () -> c (); ignore e) in
  r


(* Memoizing a hash-table. *)
let memo_memo h =
  let cache = Hashtbl.create 42 in
  let e = React.E.map (fun _ -> Hashtbl.clear cache) (React.S.changes h) in
  fun k ->
    try Hashtbl.find cache k
    with Not_found ->
      let v = Hashtbl.find (React.S.value h) k in
      Hashtbl.add cache k v;
      ignore e;
      v

