
(* just a fold *)
val adapting: ?eq:('a -> 'a -> bool) ->
  ('a -> 'b -> 'a) -> 'a -> 'b Froc_sa.t -> 'a Froc_sa.t

(* Just a lift! *)
val selfcleaning: ?eq:('a -> 'a -> bool) ->
  ('b -> 'a) -> 'b Froc_sa.t -> 'a Froc_sa.t

