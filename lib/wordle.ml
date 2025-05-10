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

let check_word test =
  let check = test |> String.uppercase_ascii in
  if String.length check <> 5 then "Word must be 5 letters."
  else if check = word_today then "Correct!"
  else
    let check_letters = letters_of check in
    let returned_string = ref "" in
    for i = 0 to 4 do
      if check_letters.(i) = letters_today.(i) then
        returned_string :=
          !returned_string ^ Printf.sprintf "Letter %d : Correct\n" (i + 1)
      else
        let letter_exists =
          Array.fold_left
            (fun acc elt -> acc || elt)
            false
            (Array.mapi
               (fun idx correct_letter -> correct_letter = check_letters.(i))
               letters_today)
        in
        if letter_exists then
          returned_string :=
            !returned_string
            ^ Printf.sprintf "Letter %d : Letter Misplaced\n" (i + 1)
        else
          returned_string :=
            !returned_string
            ^ Printf.sprintf "Letter %d : Letter Incorrect\n" (i + 1)
    done;
    !returned_string ^ "\nCorrect word is: " ^ word_today
