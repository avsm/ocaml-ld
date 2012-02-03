open Froc_sa

let never_eq _ _ = false

let app ?eq f =
  let cache = Hashtbl.create 42 in
  let f_clear =
    bind ~eq:never_eq f (fun f ->
      Printf.printf "UPDATE!\n%!";
      Hashtbl.clear cache;
      return f
    )
  in
  (fun x ->
    lift2 ?eq (fun f x ->
        Printf.printf "QUERY!\n%!";
        try Hashtbl.find cache x
        with Not_found ->
          let v = f x in
          Hashtbl.add cache x v;
          v
    )
    f_clear
    x
  )

let fn ?eq f =
  let f = app ?eq f in
  (fun x -> f (return x))

let lift ?eq f = app ?eq (return f)

