open Final_project
open ANSITerminal
open Lwt
open Final_project.SpellingDictionary

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

let router =
  Router.add router "POST" "/spell-check" (fun body ->
      let text = Body.lookup "text" body in
      let splits = get_splits text in
      let corrections, updated_dict =
        suggest_and_change_text splits !spell_dict
      in
      spell_dict := updated_dict;
      Response.response_of 200 "OK"
        (Headers.t_of "localhost" "text/plain")
        (Body.t_of_assoc_lst [ ("message", Printf.sprintf "%s" corrections) ]))

let router =
  Router.add router "POST" "/wordle" (fun body ->
      let attempt = Body.lookup "text" body in
      let result = Wordle.add_attempt attempt in
      Response.response_of 200 "OK"
        (Headers.t_of "localhost" "text/plain")
        (Body.t_of_assoc_lst [ ("message", result) ]))

let router =
  Router.add router "POST" "/capitalize" (fun body ->
      let text = Body.lookup "text" body in
      Response.response_of 200 "OK"
        (Headers.t_of "localhost" "text/plain")
        (Body.t_of_assoc_lst
           [ ("message", Printf.sprintf "%s" (String.capitalize_ascii text)) ]))

let router =
  Router.add router "POST" "/uppercase" (fun body ->
      let text = Body.lookup "text" body in
      Response.response_of 200 "OK"
        (Headers.t_of "localhost" "text/plain")
        (Body.t_of_assoc_lst
           [ ("message", Printf.sprintf "%s" (String.uppercase_ascii text)) ]))

let router =
  Router.add router "POST" "/lowercase" (fun body ->
      let text = Body.lookup "text" body in
      Response.response_of 200 "OK"
        (Headers.t_of "localhost" "text/plain")
        (Body.t_of_assoc_lst
           [ ("message", Printf.sprintf "%s" (String.lowercase_ascii text)) ]))

let reverse_string str =
  let char_arr = Array.of_seq (String.to_seq str) in
  let len = String.length str in
  for i = 0 to len / 2 do
    let temp = char_arr.(i) in
    char_arr.(i) <- char_arr.(len - 1 - i);
    char_arr.(len - 1 - i) <- temp
  done;
  String.of_seq (Array.to_seq char_arr)

let router =
  Router.add router "POST" "/reverse" (fun body ->
      let text = Body.lookup "text" body in
      Response.response_of 200 "OK"
        (Headers.t_of "localhost" "text/plain")
        (Body.t_of_assoc_lst
           [ ("message", Printf.sprintf "%s" (reverse_string text)) ]))

let router =
  Router.add router "GET" "/hello" (fun _ ->
      Response.response_of 200 "OK"
        (Headers.t_of "localhost" "text/plain")
        (Body.t_of_assoc_lst [ ("message", "Hello, World!") ]))

let router =
  Router.add router "GET" "/time" (fun _ ->
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
  Router.add router "GET" "/cs3110" (fun _ ->
      Response.response_of 200 "OK"
        (Headers.t_of "localhost" "text/plain")
        (Body.t_of_assoc_lst [ ("message", "Welcome to 3110!") ]))

let router =
  Router.add router "GET" "/random" (fun _ ->
      Response.response_of 200 "OK"
        (Headers.t_of "localhost" "text/plain")
        (Body.t_of_assoc_lst
           [ ("message", string_of_int (Random.int 1000000)) ]))

let requests_handled = ref 0
let start_time = Unix.gettimeofday ()

let router =
  Router.add router "GET" "/server-status" (fun _ ->
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
  Router.add router "GET" "/help" (fun _ ->
      Response.response_of 200 "OK"
        (Headers.t_of "localhost" "text/plain")
        (Body.t_of_assoc_lst
           [ ("info", Printf.sprintf "Elapsed time:seconds") ]))

(* DELETE endpoint to reset the game *)
let router =
  Router.add router "DELETE" "/wordle/reset" (fun _ ->
      let old_attempts = Wordle.reset_game () in
      Response.response_of 200 "OK"
        (Headers.t_of "localhost" "application/json")
        (Body.t_of_assoc_lst
           [
             ("message", "Game reset successfully");
             ("attempts_cleared", string_of_int (List.length old_attempts));
           ]))

(* DELETE endpoint to remove the last attempt *)
let router =
  Router.add router "DELETE" "/wordle/last-attempt" (fun _ ->
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
               ]))

let handle_request request =
  requests_handled := !requests_handled + 1;
  let path = Request.url request in
  let method_str = Request.request_method request in
  let body = Request.body request in
  Router.get_response router method_str path body

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

  Lwt_main.run (Tcp_server.start server handle_request)
