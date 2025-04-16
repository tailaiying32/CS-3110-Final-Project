open Lwt
open ANSITerminal

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

(* Color formatting functions *)
let format_method method_str =
  match String.uppercase_ascii method_str with
  | "GET" -> ANSITerminal.sprintf [ ANSITerminal.green ] "%s" method_str
  | "POST" -> ANSITerminal.sprintf [ ANSITerminal.yellow ] "%s" method_str
  | _ -> ANSITerminal.sprintf [ ANSITerminal.red ] "%s" method_str

let format_status_code code =
  if code >= 200 && code < 300 then
    ANSITerminal.sprintf [ ANSITerminal.green ] "%d" code
  else if code >= 400 && code < 600 then
    ANSITerminal.sprintf [ ANSITerminal.red ] "%d" code
  else ANSITerminal.sprintf [ ANSITerminal.yellow ] "%d" code

(* Simplified request parsing function *)
let parse_request request_str =
  (* Just extract method and path from the first line *)
  let lines = String.split_on_char '\n' request_str in
  match lines with
  | first_line :: _ -> (
      let parts = String.split_on_char ' ' first_line in
      match parts with
      | method_str :: path :: _ ->
          Request.request_of method_str path (Headers.t_of "" "")
            (Body.t_of_assoc_lst [])
      | _ ->
          Request.request_of "UNKNOWN" "/" (Headers.t_of "" "")
            (Body.t_of_assoc_lst []))
  | [] ->
      Request.request_of "UNKNOWN" "/" (Headers.t_of "" "")
        (Body.t_of_assoc_lst [])

let read_request client_sock =
  let buffer = Bytes.create 4096 in
  let rec read_loop acc =
    Lwt_unix.read client_sock buffer 0 4096 >>= function
    | 0 -> Lwt.return acc
    | n ->
        let chunk = Bytes.sub_string buffer 0 n in
        let new_acc = acc ^ chunk in
        (* Check if we've received the end of headers (double newline) *)
        if
          String.contains new_acc '\n'
          && (String.contains new_acc '\r' || String.contains new_acc '\n')
        then Lwt.return new_acc
        else read_loop new_acc
  in
  (* Add a timeout to prevent hanging *)
  Lwt.pick
    [ read_loop ""; (Lwt_unix.sleep 5.0 >>= fun () -> Lwt.return "TIMEOUT") ]

let write_response client_sock response request =
  let response_str = Response.string_of_response response in

  (* Log the outgoing response with colorized status code *)
  let status = Response.status_code response in
  let colored_status = format_status_code status in
  Printf.printf "--> %s %s\n%!" colored_status (Request.url request);

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
          let request = parse_request request_str in

          (* Log the incoming request with colorized method *)
          let method_str = Request.request_method request in
          let colored_method = format_method method_str in
          Printf.printf "<-- %s %s\n%!" colored_method (Request.url request);

          let response = handler request in
          write_response client_sock response request >>= fun () ->
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
