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
  Router.add router "/spell-check" (fun body ->
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

let router =
  Router.add router "/random" (fun _ ->
      Response.response_of 200 "OK"
        (Headers.t_of "localhost" "text/plain")
        (Body.t_of_assoc_lst
           [ ("message", string_of_int (Random.int 1000000)) ]))

let handle_request request =
  let path = Request.url request in
  let body = Request.body request in
  Router.get_response router path body

let () =
  let config =
    { Tcp_server.port = 8080; host = "localhost"; max_connections = 10 }
  in
  let server = Tcp_server.create config in

  print_endline "Starting web server...";
  print_endline "Available routes:";
  print_endline "  /hello - Get a hello message";
  print_endline "  /time  - Get current time";
  print_endline "  /random - Get a random integer between 0 and 1,000,000";
  print_endline "  /cs3110 - Get a welcome message";
  print_endline "  /spell-check - Spell check your text";
  print_newline ();

  Lwt_main.run (Tcp_server.start server handle_request)
