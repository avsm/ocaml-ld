

(*Memoisation*)
val memo_fun : ('a -> 'b) React.signal -> 'a -> 'b
val ref_of_signal : 'a React.signal -> 'a ref
val memo_memo : ('a, 'b) Hashtbl.t React.signal -> 'a -> 'b

(*Starting and restarting*)

(**
[c = start_restart start restart signal] listens to changes of [signal]
and call [restart v s] where [v] is the previous value of [c] and [s] is the
new value of [signal] (the one that triggered the change).

[c]'s starting value is [start s] where [s] is the current value of [signal].
 *)
val start_restart :
  ?eq:('a -> 'a -> bool) ->
  start:('b -> 'a) ->
  restart:('a -> 'b -> 'a) ->
  'b React.signal ->
  'a React.signal

(**
[c = start_and_adapt start restart signal] listens to changes of [signal]
and call [restart v s] where [v] is the previous value of [c] and [s] is the
new value of [signal] (the one that triggered the change).

[c]'s starting value is [start s] where [s] is the current value of [signal].
 *)
val start_and_adapt :
  ?eq:('a -> 'a -> bool) ->
  start:('b -> 'a) ->
  adapt:('a -> 'b -> 'a) ->
  'b React.signal ->
  'a React.signal

(**
[c = start_and_stop_state start stop state signal] listens to changes of
[signal] and calls [start (stop v) s] where [v] is the previous value of [c]
and [s] is the new value of [signal] (the one that triggered the change).
Information can flow from the previous instance (being stopped) and the new one
(being started) by having the [stop] returning some state and having the
[start] using it.

[c]'s starting value is [start state s] where [s] is the current value of
[signal].
 *)
val start_and_stop_state :
  ?eq:('a -> 'a -> bool) ->
  start:('b -> 'c -> 'a) ->
  stop:('a -> 'b) ->
  'b ->
  'c React.signal ->
  'a React.signal
(**
[c = start_and_stop start stop signal] listens to changes of
[signal] and calls [stop v; start s] where [v] is the previous value of [c]
and [s] is the new value of [signal] (the one that triggered the change).
No information can flow from the previous instance to the new one.

[c]'s starting value is [start s] where [s] is the current value of [signal].
 *)
val start_and_stop :
  ?eq:('a -> 'a -> bool) ->
  start:('b -> 'a) ->
  stop:('a -> unit) ->
  'b React.signal ->
  'a React.signal


(**
[c = start_and_let_die start signal] listens to changes of [signal] and calls
[start s] where [s] is the new value of [signal] (the one that triggered the
change). No information can flow from the previous instance to the new one and
old instances have to be stoped by some other way if necessary (/!\ beware of
leaks /!\)

[c]'s starting value is [start s] where [s] is the current value of [signal].
 *)
val start_and_ :
  ?eq:('a -> 'a -> bool) ->
  start:('b -> 'a) -> 'b React.signal -> 'a React.signal

(** A type that keeps track of the state (running or suspended) of a.. A what
  exactly?
  *)
type ('a, 'b) running =
  | Running of 'a
  | Suspended of 'b

val eq_running: ('a -> 'a -> bool) -> ('b -> 'b -> bool)
  -> (('a, 'b) running -> ('a, 'b) running -> bool)


(**
[run_when start suspend recover signal] listens for changes in [signal]
and calls [suspend] (resp [recover]) whenever it switches to [true] (resp
[false]).

Information can flow from [suspend] to [recover].

The initial value of the returned signal is [start ()].
 *)
val run_when :
  ?eq:(('a, 'b) running -> ('a, 'b) running -> bool) ->
  start:(unit -> 'a) ->
  suspend:('a -> 'b) ->
  recover:('b -> 'a) ->
  bool React.signal ->
  ('a, 'b) running React.signal


(**
[run_opt start adapter suspend recover signal] listens for changes in
[signal] and calls [suspend] (resp [recover]) (resp [adapter] whenever it
switches from [Some _] to [None] (resp from [None] to [Some _]) (resp from [Some
_] to [Some _]).

The initial value of the returned signal is [start s] where [s] is the current
value of [signal].
 *)
val run_opt :
  ?eq:(('run, 'susp) running -> ('run, 'susp) running -> bool) ->
  start:('t option -> ('run, 'susp) running) ->
  adapt:('t -> 'run -> 'run) ->
  suspend:('run -> 'susp) ->
  recover:('t -> 'susp -> 'run) ->
  't option React.signal ->
  ('run, 'susp) running React.signal



(*WIP/TODO*)
val map :
  ?eq:('a -> 'a -> bool) ->
  ?before:('b -> unit) ->
  ?after:('a -> unit) -> ('b -> 'a) -> 'b React.signal -> 'a React.signal
val map_ :
  ?eq:('a -> 'a -> bool) ->
  ?on_change:('b -> 'a -> unit) ->
  ('b -> 'a) -> 'b React.signal -> 'a React.signal
