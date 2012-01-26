
let (debug, set_debug) = React.S.create false

let (backlog, set_backlog) = React.S.create 10

let (port, set_port) = React.S.create 8080

let addr =
  React.S.map (fun p -> Unix.ADDR_INET (Unix.inet_addr_loopback, p)) port

