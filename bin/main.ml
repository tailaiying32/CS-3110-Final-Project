open Final_project
open ANSITerminal
open Lwt
open Final_project.SpellingDictionary

(* Record the server start time *)
let start_time = Unix.gettimeofday ()

(* Track the number of requests handled *)
let requests_handled = ref 0

(* Route details map *)
let route_details =
  [
    ( "hello",
      {|
    Method: GET
    Description: Returns a simple hello message
    Response Format: JSON
    Example Response: {"message": "Hello, World!"}
  |}
    );
    ( "time",
      {|
    Method: GET
    Description: Returns the current server time
    Response Format: JSON
    Example Response: {"time": "2025-03-28 01:51:29"}
  |}
    );
    ( "random",
      {|
    Method: GET
    Description: Returns a random integer between 0 and 1,000,000
    Response Format: JSON
    Example Response: {"message": "42"}
  |}
    );
    ( "cs3110",
      {|
    Method: GET
    Description: Returns a welcome message for CS 3110
    Response Format: JSON
    Example Response: {"message": "Welcome to 3110!"}
  |}
    );
    ( "uppercase",
      {|
    Method: POST
    Description: Converts input text to uppercase
    Request Format: JSON
    Required Parameters:
      - text: The text to convert
    Example Request: {"text": "hello"}
    Example Response: {"message": "HELLO"}
  |}
    );
    ( "lowercase",
      {|
    Method: POST
    Description: Converts input text to lowercase
    Request Format: JSON
    Required Parameters:
      - text: The text to convert
    Example Request: {"text": "HELLO"}
    Example Response: {"message": "hello"}
  |}
    );
    ( "capitalize",
      {|
    Method: POST
    Description: Capitalizes the first letter of input text
    Request Format: JSON
    Required Parameters:
      - text: The text to capitalize
    Example Request: {"text": "hello"}
    Example Response: {"message": "Hello"}
  |}
    );
    ( "spell-check",
      {|
    Method: POST
    Description: Performs spell checking on input text
    Request Format: JSON
    Required Parameters:
      - text: The text to spell check
    Example Request: {"text": "helllo"}
    Example Response: {"message": "hello"}
  |}
    );
    ( "reverse",
      {|
    Method: POST
    Description: Reverses the input text
    Request Format: JSON
    Required Parameters:
      - text: The text to reverse
    Example Request: {"text": "hello"}
    Example Response: {"message": "olleh"}
  |}
    );
    ( "server-status",
      {|
    Method: GET
    Description: Returns basic server statistics
    Response Format: JSON
    Example Response: {
      "up-time": "Elapsed time: 123.456 seconds",
      "requests-handled": "42"
    }
  |}
    );
    ( "wordle",
      {|
    Method: POST
    Description: Submit a guess for today's Wordle game
    Request Format: JSON
    Required Parameters:
      - text: Your Wordle guess
    Example Request: {"text": "hello"}
    Example Response: {"message": "Game feedback"}
  |}
    );
    ( "wordle/reset",
      {|
    Method: DELETE
    Description: Reset your current Wordle game
    Response Format: JSON
    Example Response: {
      "message": "Game reset successfully",
      "attempts_cleared": "3"
    }
  |}
    );
    ( "wordle/last-attempt",
      {|
    Method: DELETE
    Description: Remove your most recent Wordle attempt
    Response Format: JSON
    Example Response: {
      "message": "Last attempt deleted successfully",
      "deleted_attempt": "hello"
    }
  |}
    );
  ]

(* wrapper function to handle errors in route handlers *)
let with_error_handling handler request_body =
  try handler request_body with
  | Failure msg ->
      Printf.eprintf "Error in route handler: %s\n" msg;
      Response.response_of 500 "Internal Server Error"
        (Headers.t_of "localhost" "application/json")
        (Body.t_of_assoc_lst [ ("error", msg) ])
  | Not_found ->
      Printf.eprintf "Resource not found\n";
      Response.not_found ()
  | e ->
      let error_msg = Printexc.to_string e in
      Printf.eprintf "Unexpected error in route handler: %s\n" error_msg;
      Response.response_of 500 "Internal Server Error"
        (Headers.t_of "localhost" "application/json")
        (Body.t_of_assoc_lst [ ("error", error_msg) ])

(** [suggest_and_change_split split spell_dict] is the user's choice on the
    corrections for [split], with a potentially updated spelling dictionary.
    Raises: [EarlyQuitting] if an invalid option is chosen for [split]. *)
let suggest_and_change_split split spell_dict =
  let word_str = str_of_split split in
  (* If not a valid word or in dictionary *)
  if
    (not (is_word split))
    || spell_check word_str spell_dict
    || spell_check (String.lowercase_ascii word_str) spell_dict
  then (word_str, spell_dict)
  else
    (* Get corrections *)
    let corrections = list_of_t (lookup_corrections word_str spell_dict) in
    let _ = print_corrections word_str corrections in
    (* Take user input *)
    let choice =
      read_line
        (Printf.printf
           "Choose a number from above for correction, 0 to leave unchanged, \
            \"+\" to add to user dictionary, and anything else to exit editing.\n")
    in
    (* Add to user dictionary *)
    if choice = "+" then (word_str, add_to_dict word_str spell_dict)
    else (* Quit early or choose *)
      let num_choice = try int_of_string choice with Failure msg -> -1 in
      if num_choice > List.length corrections || num_choice < 0 then
        raise EarlyQuitting
      else if num_choice = 0 then (word_str, spell_dict)
      else (List.nth corrections (num_choice - 1), spell_dict)

(** [suggest_and_change_text_aux (edits most_recent_dict) splt] is
    [(edits, most_recent_dict)], which contain the most recent spelling
    dictionary and all of the user's edits after processing [splt].
    Post-condition: [most_recent_dict] is always the most up-to-date spelling
    dictionary. *)
let suggest_and_change_text_aux (edits, most_recent_dict) splt =
  let edit, updated_dict = suggest_and_change_split splt most_recent_dict in
  (edits ^ edit, updated_dict)

(** [suggest_and_change_text text spell_dict] is the most recent spelling
    dictionary and the edits for [text] using [spell_dict]. *)
let suggest_and_change_text text spell_dict =
  List.fold_left suggest_and_change_text_aux ("", spell_dict) text

let spell_dict =
  ref (combined_dictionary "data/SINGLE.TXT" "data/user_dictionary.txt")

let router = Router.init ()

let reverse_string str =
  let char_arr = Array.of_seq (String.to_seq str) in
  let len = String.length str in
  for i = 0 to len / 2 do
    let temp = char_arr.(i) in
    char_arr.(i) <- char_arr.(len - 1 - i);
    char_arr.(len - 1 - i) <- temp
  done;
  String.of_seq (Array.to_seq char_arr)

(* Basic routes *)
let router =
  Router.add router "GET" "/hello" (fun body query_params ->
      Response.response_of 200 "OK"
        (Headers.t_of "localhost" "application/json")
        (Body.t_of_assoc_lst [ ("message", "Hello, World!") ]))

let router =
  Router.add router "GET" "/time" (fun body query_params ->
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
  Router.add router "GET" "/cs3110" (fun body query_params ->
      Response.response_of 200 "OK"
        (Headers.t_of "localhost" "text/plain")
        (Body.t_of_assoc_lst [ ("message", "Welcome to 3110!") ]))

let router =
  Router.add router "GET" "/random" (fun body query_params ->
      Response.response_of 200 "OK"
        (Headers.t_of "localhost" "text/plain")
        (Body.t_of_assoc_lst
           [ ("message", string_of_int (Random.int 1000000)) ]))

let router =
  Router.add router "GET" "/server-status" (fun body query_params ->
      Response.response_of 200 "OK"
        (Headers.t_of "localhost" "text/plain")
        (Body.t_of_assoc_lst
           [
             ( "up-time",
               Printf.sprintf "Elapsed time: %.3f seconds"
                 (Unix.gettimeofday () -. start_time) );
             ("requests-handled", Printf.sprintf "%d" !requests_handled);
           ]))

let router =
  Router.add router "GET" "/help" (fun body query_params ->
      match Query_params.get "route" query_params with
      | Some route_name -> (
          (* Try to find the specific route *)
          match List.assoc_opt route_name route_details with
          | Some details ->
              Response.response_of 200 "OK"
                (Headers.t_of "localhost" "application/json")
                (Body.t_of_assoc_lst
                   [ ("route", "/" ^ route_name); ("details", details) ])
          | None ->
              Response.response_of 404 "Not Found"
                (Headers.t_of "localhost" "application/json")
                (Body.t_of_assoc_lst
                   [
                     ("error", "Route not found");
                     ("message", "The specified route does not exist");
                     ( "available_routes",
                       String.concat ", "
                         (List.map
                            (fun r -> "/" ^ r)
                            (List.map fst route_details)) );
                   ]))
      | None ->
          (* Return list of all available routes *)
          Response.response_of 200 "OK"
            (Headers.t_of "localhost" "application/json")
            (Body.t_of_assoc_lst
               [
                 ( "message",
                   "Use the 'route' query parameter to get detailed \
                    information about a specific route" );
                 ("example", "/help?route=hello");
                 ( "available_routes",
                   String.concat ", "
                     (List.map (fun r -> "/" ^ r) (List.map fst route_details))
                 );
               ]))

(* Text transformation routes with safe lookup *)
let router =
  Router.add router "POST" "/capitalize" (fun body query_params ->
      match Body.safe_lookup "text" body with
      | None ->
          Response.response_of 400 "Bad Request"
            (Headers.t_of "localhost" "application/json")
            (Body.t_of_assoc_lst [ ("error", "Missing 'text' parameter") ])
      | Some text ->
          Response.response_of 200 "OK"
            (Headers.t_of "localhost" "text/plain")
            (Body.t_of_assoc_lst [ ("message", String.capitalize_ascii text) ]))

let router =
  Router.add router "POST" "/uppercase" (fun body query_params ->
      match Body.safe_lookup "text" body with
      | None ->
          Response.response_of 400 "Bad Request"
            (Headers.t_of "localhost" "application/json")
            (Body.t_of_assoc_lst [ ("error", "Missing 'text' parameter") ])
      | Some text ->
          Response.response_of 200 "OK"
            (Headers.t_of "localhost" "text/plain")
            (Body.t_of_assoc_lst [ ("message", String.uppercase_ascii text) ]))

let router =
  Router.add router "POST" "/lowercase" (fun body query_params ->
      match Body.safe_lookup "text" body with
      | None ->
          Response.response_of 400 "Bad Request"
            (Headers.t_of "localhost" "application/json")
            (Body.t_of_assoc_lst [ ("error", "Missing 'text' parameter") ])
      | Some text ->
          Response.response_of 200 "OK"
            (Headers.t_of "localhost" "text/plain")
            (Body.t_of_assoc_lst [ ("message", String.lowercase_ascii text) ]))

let router =
  Router.add router "POST" "/reverse" (fun body query_params ->
      match Body.safe_lookup "text" body with
      | None ->
          Response.response_of 400 "Bad Request"
            (Headers.t_of "localhost" "application/json")
            (Body.t_of_assoc_lst [ ("error", "Missing 'text' parameter") ])
      | Some text ->
          Response.response_of 200 "OK"
            (Headers.t_of "localhost" "text/plain")
            (Body.t_of_assoc_lst [ ("message", reverse_string text) ]))

(* Spell check route with safe lookup *)
let router =
  Router.add router "POST" "/spell-check" (fun body query_params ->
      match Body.safe_lookup "text" body with
      | None ->
          Response.response_of 400 "Bad Request"
            (Headers.t_of "localhost" "application/json")
            (Body.t_of_assoc_lst [ ("error", "Missing 'text' parameter") ])
      | Some text -> (
          try
            let splits = get_splits text in
            let corrections, updated_dict =
              suggest_and_change_text splits !spell_dict
            in
            spell_dict := updated_dict;
            Response.response_of 200 "OK"
              (Headers.t_of "localhost" "text/plain")
              (Body.t_of_assoc_lst [ ("message", corrections) ])
          with
          | EarlyQuitting ->
              Response.response_of 400 "Bad Request"
                (Headers.t_of "localhost" "application/json")
                (Body.t_of_assoc_lst
                   [ ("error", "Spell check operation cancelled") ])
          | e ->
              let error_msg = Printexc.to_string e in
              Response.response_of 500 "Internal Server Error"
                (Headers.t_of "localhost" "application/json")
                (Body.t_of_assoc_lst [ ("error", error_msg) ])))

(* Wordle routes with safe lookup *)
let router =
  Router.add router "POST" "/wordle" (fun body query_params ->
      match Body.safe_lookup "text" body with
      | None ->
          Response.response_of 400 "Bad Request"
            (Headers.t_of "localhost" "application/json")
            (Body.t_of_assoc_lst [ ("error", "Missing 'text' parameter") ])
      | Some attempt -> (
          try
            let result = Wordle.add_attempt attempt in
            Response.response_of 200 "OK"
              (Headers.t_of "localhost" "text/plain")
              (Body.t_of_assoc_lst [ ("message", result) ])
          with e ->
            let error_msg = Printexc.to_string e in
            Response.response_of 500 "Internal Server Error"
              (Headers.t_of "localhost" "application/json")
              (Body.t_of_assoc_lst [ ("error", error_msg) ])))

(* New DELETE endpoints for Wordle *)
let router =
  Router.add router "DELETE" "/wordle/reset" (fun body query_params ->
      try
        let old_attempts = Wordle.reset_game () in
        Response.response_of 200 "OK"
          (Headers.t_of "localhost" "application/json")
          (Body.t_of_assoc_lst
             [
               ("message", "Game reset successfully");
               ("attempts_cleared", string_of_int (List.length old_attempts));
             ])
      with e ->
        let error_msg = Printexc.to_string e in
        Response.response_of 500 "Internal Server Error"
          (Headers.t_of "localhost" "application/json")
          (Body.t_of_assoc_lst [ ("error", error_msg) ]))

let router =
  Router.add router "DELETE" "/wordle/last-attempt" (fun body query_params ->
      try
        match Wordle.delete_last_attempt () with
        | None ->
            Response.response_of 404 "Not Found"
              (Headers.t_of "localhost" "application/json")
              (Body.t_of_assoc_lst [ ("error", "No attempts to delete") ])
        | Some attempt ->
            Response.response_of 200 "OK"
              (Headers.t_of "localhost" "application/json")
              (Body.t_of_assoc_lst
                 [
                   ("message", "Last attempt deleted successfully");
                   ("deleted_attempt", attempt);
                 ])
      with e ->
        let error_msg = Printexc.to_string e in
        Response.response_of 500 "Internal Server Error"
          (Headers.t_of "localhost" "application/json")
          (Body.t_of_assoc_lst [ ("error", error_msg) ]))

let handle_request request =
  requests_handled := !requests_handled + 1;
  let path = Request.url request in
  let method_str = Request.request_method request in
  let body = Request.body request in
  Router.get_response router method_str path body

let rec run_local_mode () : unit Lwt.t =
  Printf.printf "\n> Enter request line (e.g. GET /hello): ";
  flush stdout;

  (* Ensure prompt shows immediately *)
  let line = read_line () in

  if line = "exit" then (
    Printf.printf "Exiting local mode.\n";
    flush stdout;
    exit 0);

  let parts = String.split_on_char ' ' line in
  let formatted_request =
    match parts with
    | [ method_; path ] ->
        Printf.sprintf "%s %s HTTP/1.1\r\nHost: localhost\r\n\r\n" method_ path
    | _ ->
        Printf.printf "Invalid format. Use: METHOD /path\n";
        flush stdout;
        "" (* Dummy value, won't be used *)
  in

  let%lwt () =
    if formatted_request = "" then Lwt.return_unit
    else
      let sock = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      let sockaddr = Unix.ADDR_INET (Unix.inet_addr_loopback, 8080) in
      let%lwt () = Lwt_unix.connect sock sockaddr in
      let%lwt _ =
        Lwt_unix.write_string sock formatted_request 0
          (String.length formatted_request)
      in
      let buf = Bytes.create 4096 in
      let%lwt len = Lwt_unix.read sock buf 0 4096 in
      let resp = Bytes.sub_string buf 0 len in
      Printf.printf "Response from server:\n%s\n" resp;
      flush stdout;
      Lwt.return_unit
  in

  run_local_mode ()

let () =
  let config =
    { Tcp_server.port = 8080; host = "localhost"; max_connections = 10 }
  in
  let server = Tcp_server.create config in

  print_endline "Starting web server...";
  print_endline "Available routes:";
  print_endline "  GET /hello - Get a hello message";
  print_endline "  GET /time - Get current time";
  print_endline "  GET /random - Get a random integer between 0 and 1,000,000";
  print_endline "  GET /cs3110 - Get a welcome message";
  print_endline "  POST /uppercase - Convert your text into all-caps";
  print_endline "  POST /lowercase - Convert your text into all-lowercase";
  print_endline "  POST /capitalize - Capitalize your text";
  print_endline "  POST /spell-check - Spell check your text";
  print_endline "  POST /reverse - Reverse your text";
  print_endline "  GET /server-status - Get basic statistics about the server";
  print_endline "  GET /help - For information about how to structure requests";
  print_endline "  POST /wordle - Submit a guess for today's Wordle";
  print_endline "  DELETE /wordle/reset - Reset your Wordle game";
  print_endline
    "  DELETE /wordle/last-attempt - Remove your most recent Wordle attempt";
  print_newline ();

  let server_thread = Tcp_server.start server handle_request in

  if Array.length Sys.argv > 1 && Sys.argv.(1) = "local" then (
    print_endline "You have succesffuly launched local mode";
    print_endline "Local mode only supports get request";
    Lwt_main.run (Lwt.join [ server_thread; run_local_mode () ]))
  else Lwt_main.run server_thread
