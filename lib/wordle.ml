(* Reference: https://ocaml.org/manual/5.3/api/Unix.html *)
let seed_from_date () =
  let time_data = Unix.localtime (Unix.time ()) in
  let year = time_data.tm_year + 1900 in
  let month = time_data.tm_mon + 1 in
  let day = time_data.tm_mday in
  (year * 10000) + (month * 100) + day

let word_bag =
  BatFile.lines_of "data/SINGLE.TXT"
  |> BatList.of_enum
  |> List.filter (fun x -> String.length x = 5)
  |> List.sort compare |> Array.of_list

let word_today =
  let () = Random.init (seed_from_date ()) in
  let idx = Random.int (Array.length word_bag) in
  word_bag.(idx) |> String.uppercase_ascii

let letters_of word = word |> String.to_seq |> Array.of_seq
let letters_today = letters_of word_today

let valid_words =
  SpellingDictionary.combined_dictionary "data/SINGLE.TXT"
    "data/user_dictionary.txt"

let color_word guess =
  let result = Array.make 5 "Gray" in
  let used = Array.make 5 false in
  let guess_chars = Array.init 5 (fun idx -> String.get guess idx) in
  for i = 0 to 4 do
    if guess_chars.(i) = letters_today.(i) then begin
      result.(i) <- "Green";
      used.(i) <- true
    end
  done;

  for i = 0 to 4 do
    if result.(i) = "Gray" then
      for j = 0 to 4 do
        if (not used.(j)) && guess_chars.(i) = letters_today.(j) then begin
          result.(i) <- "Yellow";
          used.(j) <- true
        end
      done
  done;
  result

let check_word test =
  let check = test |> String.uppercase_ascii in
  if String.length check <> 5 then "Word must be 5 letters."
  else if check = word_today then "Correct!"
  else if not (SpellingDictionary.spell_check check valid_words) then
    "Word does not exist in dictionary."
  else
    let colors = color_word check in
    let indexed =
      Array.mapi
        (fun idx s -> Printf.sprintf "Letter %d : %s\n" (idx + 1) s)
        colors
    in
    Array.fold_left (fun acc s -> acc ^ s) "" indexed

(* Store user attempts in a mutable reference *)
let user_attempts = ref []

(* Add a function to record an attempt *)
let add_attempt attempt =
  user_attempts := attempt :: !user_attempts;
  check_word attempt

(* Add a function to clear all attempts *)
let reset_game () =
  let old_attempts = !user_attempts in
  user_attempts := [];
  old_attempts

(* Add a function to remove the most recent attempt *)
let delete_last_attempt () =
  try
    match !user_attempts with
    | [] -> None
    | x :: xs ->
        user_attempts := xs;
        Some x
  with _ ->
    Printf.eprintf "Error in delete_last_attempt\n";
    None
