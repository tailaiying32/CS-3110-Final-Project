open Lwt

type config = {
  port : int;
  host : string;
  max_connections : int;
}

type t = {
  config : config;
  mutable is_running : bool;
  mutable socket : Lwt_unix.file_descr option;
}

let create config = { config; is_running = false; socket = None }

let start server handler =
  let sock = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt sock Unix.SO_REUSEADDR true;
  let addr = Unix.ADDR_INET (Unix.inet_addr_any, server.config.port) in
  Lwt_unix.bind sock addr >>= fun () ->
  Lwt_unix.listen sock server.config.max_connections;
  server.socket <- Some sock;
  server.is_running <- true;

  Printf.printf "Server listening on port %d\n%!" server.config.port;

  let rec accept_loop () =
    if not server.is_running then Lwt.return_unit
    else
      Lwt_unix.accept sock >>= fun (client_sock, _) ->
      Lwt.async (fun () ->
          handler client_sock >>= fun () -> Lwt_unix.close client_sock);
      accept_loop ()
  in
  accept_loop ()

let stop server =
  match server.socket with
  | None -> Lwt.return_unit
  | Some sock ->
      server.is_running <- false;
      server.socket <- None;
      Printf.printf "Server shutting down\n%!";
      Lwt_unix.close sock

let is_running server = server.is_running
let get_config server = server.config
