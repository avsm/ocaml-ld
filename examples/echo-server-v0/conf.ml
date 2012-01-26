
let (debug, set_debug) = React.S.create false

let (backlog, set_backlog) = React.S.create 10

let (port, set_port) = React.S.create 8080

let addr =
  React.S.map (fun p -> Unix.ADDR_INET (Unix.inet_addr_loopback, p)) port


(* Reconfiguration *)

let ( >>= ) = Lwt.( >>= )

(* Simple parsing for now *)
let unsafe_parse s = match s with
  | "printconf" ->
    Lwt.ignore_result (
      Lwt_io.printf "Debug: %b, Backlog: %d, Port: %d\n"
        (React.S.value debug)
        (React.S.value backlog)
        (React.S.value port) >>= fun () ->
      Lwt_io.flush Lwt_io.stdout
    )
  | "debug" -> set_debug true
  | "nodebug" -> set_debug false
  | s when String.sub s 0 4 = "port" ->
    set_port (int_of_string (String.sub s 5 (String.length s - 5)))
  | s when String.sub s 0 7 = "backlog" ->
    set_port (int_of_string (String.sub s 8 (String.length s - 8)))
  | _ -> raise (Invalid_argument "")


let parse s = try unsafe_parse s with Invalid_argument _ -> ()

let () = Lwt.ignore_result (
  Lwt_stream.iter parse (Lwt_io.read_lines Lwt_io.stdin)
)


