open Final_project
open ANSITerminal

let rec read_input () =
  print_string [ Bold; blue ] "Enter a path (or 'quit' to exit): ";
  let input = read_line () in
  print_string [ Reset ] "";
  input

let display_response response =
  let response_str = Response.string_of_response response in
  let lines = String.split_on_char '\n' response_str in
  ignore
    (List.iter
       (fun line ->
         if String.starts_with ~prefix:"=====" line then
           print_string [ Bold; green ] (line ^ "\n")
         else if String.contains line ':' then (
           let parts = String.split_on_char ':' line in
           match parts with
           | [] -> print_endline line (* Shouldn't happen *)
           | [ single ] -> print_endline single (* No colon found *)
           | key :: rest ->
               let value = String.concat ":" rest in
               print_string [ Bold; green ] (key ^ ":");
               print_endline value)
         else print_endline line)
       lines);
  print_string [ Reset ] "";
  print_newline ()

let () =
  let router = Router.init () in
  (* Example routes *)
  let router =
    Router.add router "/hello" (fun _ ->
        Response.response_of 200 "OK"
          (Headers.t_of "localhost" "text/plain")
          (Body.t_of_assoc_lst [ ("message", "Hello, World!") ]))
  in

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
  in

  print_endline "Welcome to the Web Server CLI!";
  print_endline "Available routes:";
  print_endline "  /hello - Get a hello message";
  print_endline "  /time  - Get current time";
  print_newline ();

  let rec loop () =
    let input = read_input () in
    if input = "quit" then print_endline "Goodbye!"
    else
      let response = Router.get_response router input "" in
      display_response response;
      loop ()
  in
  loop ()
