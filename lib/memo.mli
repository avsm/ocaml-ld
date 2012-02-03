
val fn: ?eq:('b -> 'b -> bool) ->
  ('a -> 'b) Froc_sa.t -> ('a -> 'b Froc_sa.t)

val app: ?eq:('b -> 'b -> bool) ->
  ('a -> 'b) Froc_sa.t -> 'a Froc_sa.t -> 'b Froc_sa.t

val lift: ?eq:('b -> 'b -> bool) ->
  ('a -> 'b) -> 'a Froc_sa.t -> 'b Froc_sa.t

