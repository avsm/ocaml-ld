
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



(* Starting and Restarting *)

(* start something and restart it when the given signal changes. *)
let start_restart ?eq ~start ~restart s =
  React.S.fold ?eq
    restart
    (start (React.S.value s))
    (React.S.changes s)

(* start something and restart it when the given signal changes. The restart
 * procedure is two fold: stoping and starting afresh. Some information is
 * transmitted from the stop to the start. *)
let start_and_stop_state ?eq ~start ~stop i s =
  React.S.fold ?eq
    (fun x s -> let h = stop x in start h s)
    (start i (React.S.value s))
    (React.S.changes s)

(* start something and restart it when the given signal changes. The restart
 * procedure is two fold: stoping and starting afresh. No information is
 * transmitted from the stop to the start. *)
let start_and_stop ?eq ~start ~stop s =
  React.S.fold ?eq
    (fun x s -> (stop x : unit); start s)
    (start (React.S.value s))
    (React.S.changes s)

(* Start something and start a new instance each time the signal changes. The
 * old values are not stoped. One can combine the resulting signal with fold or
 * accum to keep a list of started things. *)
let start_and_ ?eq ~start s =
  React.S.map ?eq start s

(* Same as start_restart but for use when restarting is not needed. *)
let start_and_adapt ?eq ~start ~adapt s =
  React.S.fold ?eq
    adapt
    (start (React.S.value s))
    (React.S.changes s)


type ('running, 'suspended) running =
  | Running of 'running
  | Suspended of 'suspended

let eq_running (eqr: 'a -> 'a -> bool) (eqs: 'b -> 'b -> bool) =
  fun r1 r2 -> match r1, r2 with
  | Running _, Suspended _ | Suspended _, Running _ -> false
  | Running r1,   Running r2   -> eqr r1 r2
  | Suspended s1, Suspended s2 -> eqs s1 s2

(* Runs when the given signal is true. suspend and recover functions are used *)
let run_when ?eq ~start ~suspend ~recover s =
  React.S.fold ?eq
    (fun v s -> match v with
      | Running v   -> assert (not s); Suspended (suspend v)
      | Suspended v -> assert s;       Running (recover v)
    )
    (Running (start ()))
    (React.S.changes s)

(* Like run_when but uses an option signal and adapt to change in the value. *)
let run_opt ?eq ~start ~adapt ~suspend ~recover s =
  React.S.fold ?eq
    (fun v s -> match s with
      | None -> begin match  v with
        | Suspended _ -> assert false
        | Running v -> Suspended (suspend v)
      end
      | Some s -> match v with
        | Suspended v -> Running (recover s v)
        | Running v  -> Running (adapt s v)
    )
    (start (React.S.value s))
    (React.S.changes s)





(*TODO: one fuinction with all the options*)
let map ?eq ?(before = ignore) ?(after = ignore) f s =
  let ev =
    React.E.map
      (fun y -> after y; y)
      (React.E.map
        (fun x -> before x; f x)
        (React.S.changes s)
      )
  in
  React.S.hold
    ?eq
    (f (React.S.value s))
    ev

let map_ ?eq ?(on_change = (fun _ _ -> ())) f s =
  let ev =
    React.E.map
      (fun x ->
        let y = f x in
        on_change x y;
        y
      )
      (React.S.changes s)
  in
  React.S.hold
    ?eq
    (f (React.S.value s))
    ev

