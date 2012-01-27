

(*Memoisation*)
val memo_fun : ('a -> 'b) React.signal -> 'a -> 'b
val ref_of_signal : 'a React.signal -> 'a ref
val memo_memo : ('a, 'b) Hashtbl.t React.signal -> 'a -> 'b

(*Starting and restarting*)

(**
[c = start_and_restart starter restarter signal] listens to changes of [signal]
and call [restarter v s] where [v] is the previous value of [c] and [s] is the
new value of [signal] (the one that triggered the change).

[c]'s starting value is [starter s] where [s] is the current value of [signal].
 *)
val start_and_restart :
  ?eq:('a -> 'a -> bool) ->
  ('b -> 'a) -> ('a -> 'b -> 'a) -> 'b React.signal -> 'a React.signal

(**
[c = start_stop_and_restart starter stoper state signal] listens to changes of
[signal] and calls [starter (stoper v) s] where [v] is the previous value of [c]
and [s] is the new value of [signal] (the one that triggered the change).
Information can flow from the previous instance (being stopped) and the new one
(being started) by having the [stoper] returning some state and having the
[starter] using it.

[c]'s starting value is [starter state s] where [s] is the current value of
[signal].
 *)
val start_stop_and_restart :
  ?eq:('a -> 'a -> bool) ->
  ('b -> 'c -> 'a) -> ('a -> 'b) -> 'b -> 'c React.signal -> 'a React.signal

(**
[c = start_stop_and_start_again starter stoper signal] listens to changes of
[signal] and calls [stoper v; starter s] where [v] is the previous value of [c]
and [s] is the new value of [signal] (the one that triggered the change).
No information can flow from the previous instance to the new one.

[c]'s starting value is [starter s] where [s] is the current value of [signal].
 *)
val start_stop_and_start_again :
  ?eq:('a -> 'a -> bool) ->
  ('b -> 'a) -> ('a -> 'c) -> 'b React.signal -> 'a React.signal

(**
[c = start_and_let_die starter signal] listens to changes of [signal] and calls
[starter s] where [s] is the new value of [signal] (the one that triggered the
change). No information can flow from the previous instance to the new one and
old instances have to be stoped by some other way if necessary (/!\ beware of
leaks /!\)

[c]'s starting value is [starter s] where [s] is the current value of [signal].
 *)
val start_and_let_die :
  ?eq:('a -> 'a -> bool) -> ('b -> 'a) -> 'b React.signal -> 'a React.signal


(** A type that keeps track of the state (running or suspended) of a.. A what
  exactly?
  *)
type ('a, 'b) running =
  | Running of 'a
  | Suspended of 'b

val eq_running: ('a -> 'a -> bool) -> ('b -> 'b -> bool)
  -> (('a, 'b) running -> ('a, 'b) running -> bool)


(**
[run_when starter suspender recoverer signal] listens for changes in [signal]
and calls [suspender] (resp [recoverer]) whenever it switches to [true] (resp
[false]).

Information can flow from [suspender] to [recoverer].

The initial value of the returned signal is [start ()].
 *)
val run_when :
  ?eq:(('a, 'b) running -> ('a, 'b) running -> bool) ->
  (unit -> 'a) ->
  ('a -> 'b) ->
  ('b -> 'a) ->
  bool React.signal -> ('a, 'b) running React.signal


(**
[run_opt starter adapter suspender recoverer signal] listens for changes in
[signal] and calls [suspender] (resp [recoverer]) (resp [adapter] whenever it
switches from [Some _] to [None] (resp from [None] to [Some _]) (resp from [Some
_] to [Some _]).

The initial value of the returned signal is [start s] where [s] is the current
value of [signal].
 *)
val run_opt :
  ?eq:(('run, 'susp) running -> ('run, 'susp) running -> bool) ->
  ('c option -> ('run, 'susp) running) ->
  ('c -> 'run -> 'run) ->
  ('run -> 'susp) ->
  ('c -> 'susp -> 'run) ->
  'c option React.signal ->
  ('run, 'susp) running React.signal
