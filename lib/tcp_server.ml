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

let read_request client_sock =
  let buffer = Bytes.create 4096 in
  let rec read_loop acc =
    Lwt_unix.read client_sock buffer 0 4096 >>= function
    | 0 -> Lwt.return acc
    | n ->
        let chunk = Bytes.sub_string buffer 0 n in
        read_loop (acc ^ chunk)
  in
  read_loop ""

let write_response client_sock response =
  let response_str = Response.string_of_response response in
  Lwt_unix.write client_sock
    (Bytes.of_string response_str)
    0
    (String.length response_str)
  >>= fun _ -> Lwt.return_unit

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
          read_request client_sock >>= fun request_str ->
          let request =
            Request.request_of "GET" "/" (Headers.t_of "" "")
              (Body.t_of_assoc_lst [])
          in
          let response = handler request in
          write_response client_sock response >>= fun () ->
          Lwt_unix.close client_sock);
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
