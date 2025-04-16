open Final_project
open ANSITerminal
open Lwt

let router = Router.init ()

let router =
  Router.add router "/hello" (fun _ ->
      Response.response_of 200 "OK"
        (Headers.t_of "localhost" "text/plain")
        (Body.t_of_assoc_lst [ ("message", "Hello, World!") ]))

let router =
  Router.add router "/time" (fun _ ->
      Response.response_of 200 "OK"
        (Headers.t_of "localhost" "text/plain")
        (Body.t_of_assoc_lst
           [
             ( "time",
               let time = Unix.localtime (Unix.time ()) in
               Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d"
                 (time.tm_year + 1900) (time.tm_mon + 1) time.tm_mday
                 time.tm_hour time.tm_min time.tm_sec );
           ]))

let router =
  Router.add router "/cs3110" (fun _ ->
      Response.response_of 200 "OK"
        (Headers.t_of "localhost" "text/plain")
        (Body.t_of_assoc_lst [ ("message", "Welcome to 3110!") ]))

let extract_path request_str =
  try
    let lines = String.split_on_char '\n' request_str in
    match lines with
    | first_line :: _ -> (
        let parts = String.split_on_char ' ' first_line in
        match parts with
        | _ :: path :: _ -> path
        | _ -> "/")
    | [] -> "/"
  with _ -> "/"

let handle_request request =
  let request_str = Request.string_of_request request in
  let path = extract_path request_str in
  Router.get_response router path ""

let () =
  let config =
    { Tcp_server.port = 8080; host = "localhost"; max_connections = 10 }
  in
  let server = Tcp_server.create config in

  print_endline "Starting web server...";
  print_endline "Available routes:";
  print_endline "  /hello - Get a hello message";
  print_endline "  /time  - Get current time";
  print_endline "  /cs3110 - Get a welcome message";
  print_newline ();

  Lwt_main.run (Tcp_server.start server handle_request)
