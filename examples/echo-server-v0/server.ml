
let s =
  React.S.fold
    (fun old (backlog, addr) ->
      Lwt_io.shutdown_server old;
      Lwt_io.establish_server
        ~backlog
        addr
        Echo.f
    )
    (Lwt_io.establish_server
      ~backlog:(React.S.value Conf.backlog)
      (React.S.value Conf.addr)
      Echo.f
    )
    (React.S.changes (React.S.l2 (fun x y -> (x, y)) Conf.backlog Conf.addr))


    (*TODO: shutdown event*)
let () = Lwt_main.run (

  fst (Lwt.task ())

)

