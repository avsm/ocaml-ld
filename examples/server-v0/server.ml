let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)


let linewise_map ic f oc =
  Lwt_io.write_lines oc (Lwt_stream.map_s f (Lwt_io.read_lines ic))

let f (ic, oc) =
  Lwt.ignore_result (
    linewise_map
      ic
      (fun s ->
        Debug.before s      >>= fun () ->
        Lwt.return (Fn.f s) >>= fun s  ->
        Debug.after s       >>= fun () ->
        Lwt.return s
      )
      oc
  )

let s =
  React.S.fold
    (fun old (backlog, addr) ->
      Lwt_io.shutdown_server old;
      Lwt_io.establish_server
        ~backlog
        addr
        f
    )
    (Lwt_io.establish_server
      ~backlog:(React.S.value Conf.backlog)
      (React.S.value Conf.addr)
      f
    )
    (React.S.changes (React.S.l2 (fun x y -> (x, y)) Conf.backlog Conf.addr))


    (*TODO: shutdown event*)
let () = Lwt_main.run (

  fst (Lwt.task ())

)

