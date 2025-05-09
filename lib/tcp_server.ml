open Lwt
open ANSITerminal
open Lwt.Infix
open Json

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
  let first_line = List.hd lines in
  let parts = String.split_on_char ' ' first_line in
  let body_json =
    match extract_json_block request_str with
    | Some json -> json
    | None -> ""
  in
  match parts with
  | method_str :: path :: _ ->
      Request.request_of method_str path (Headers.t_of "" "")
        (Body.t_of_assoc_lst (assoc_of_json_string body_json))
  | _ ->
      Request.request_of "UNKNOWN" "/" (Headers.t_of "" "")
        (Body.t_of_assoc_lst (assoc_of_json_string body_json))

let read_request client_sock =
  let buffer = Bytes.create 4096 in
  let rec read_loop acc =
    Lwt_unix.read client_sock buffer 0 (Bytes.length buffer) >>= function
    | 0 -> Lwt.return acc
    | n ->
        let acc = acc ^ Bytes.sub_string buffer 0 n in
        if String.contains acc '\n' then Lwt.return acc else read_loop acc
  in
  read_loop ""

let write_response client_sock response request duration_ms =
  let response_str = Response.string_of_response response in

  (* Log the outgoing response with colorized status code and duration *)
  let status = Response.status_code response in
  let colored_status = format_status_code status in
  let method_str = Request.request_method request in
  let url = Request.url request in
  Printf.printf "--> %s %s %s %dms\n%!" method_str url colored_status
    duration_ms;

  Lwt_unix.write client_sock
    (Bytes.of_string response_str)
    0
    (String.length response_str)
  >>= fun _ -> Lwt.return_unit

(* Helper for safe socket operations with proper error handling *)
let handle_socket_error f =
  Lwt.catch f (function
    | Unix.Unix_error (Unix.EBADF, _, _) -> Lwt.return_unit
    | exn -> Lwt.fail exn)

(* Extract connection handling to make the main loop cleaner *)
let handle_connection handler client =
  Lwt.finalize
    (fun () ->
      let start_time = Unix.gettimeofday () in
      let%lwt req_s = read_request client in
      let req = parse_request req_s in
      (* Log request *)
      let m = Request.request_method req and u = Request.url req in
      Printf.printf "<-- %s %s\n%!" (format_method m) u;
      let resp = handler req in
      let end_time = Unix.gettimeofday () in
      let duration_ms = int_of_float ((end_time -. start_time) *. 1000.0) in
      write_response client resp req duration_ms)
    (fun () -> Lwt_unix.close client)

let start server handler =
  (* 1) Guard the port synchronously *)
  if server.config.port < 0 || server.config.port > 65_535 then
    invalid_arg "port must be 0-65535";

  (* 2) Set up and listen *)
  let sock = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt sock Unix.SO_REUSEADDR true;
  let addr = Unix.ADDR_INET (Unix.inet_addr_any, server.config.port) in
  Lwt_unix.bind sock addr >>= fun () ->
  Lwt_unix.listen sock server.config.max_connections;

  (* Update server state *)
  server.socket <- Some sock;
  server.is_running <- true;
  Printf.printf "Server listening on port %d\n%!" server.config.port;

  (* 3) Main connection acceptance loop - CHECK is_running BEFORE accepting *)
  let rec accept_loop () =
    if not server.is_running then Lwt.return_unit
    else
      handle_socket_error (fun () ->
          Lwt_unix.accept sock >>= fun (client, _) ->
          Lwt.async (fun () -> handle_connection handler client);
          accept_loop ())
  in
  accept_loop ()

let stop server =
  match server.socket with
  | None -> Lwt.return_unit
  | Some sock ->
      (* Update server state FIRST to stop the accept loop *)
      server.is_running <- false;

      (* Then close the socket - IMPORTANT: Keep a reference to sock *)
      Printf.printf "Server shutting down\n%!";
      let close_promise = handle_socket_error (fun () -> Lwt_unix.close sock) in

      (* Only set socket to None AFTER closing completes *)
      close_promise >>= fun () ->
      server.socket <- None;
      Lwt.return_unit

let is_running server = server.is_running
let get_config server = server.config
