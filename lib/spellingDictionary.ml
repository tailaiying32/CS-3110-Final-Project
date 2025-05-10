open BatSet.String

exception InvalidPath of string
exception EarlyQuitting

type t = BatSet.String.t

let sprint_t t_val =
  if BatList.length (BatList.of_enum (BatSet.String.enum t_val)) > 0 then
    "["
    ^ BatList.reduce (fun s t -> s ^ "; " ^ t) (BatList.of_enum (enum t_val))
    ^ "]"
  else "[]"

let t_of_list lst = of_list lst
let list_of_t t_var = to_list t_var
let union s t = union s t
let diff s t = diff s t

let dict_from_file file_path =
  try of_enum (BatFile.lines_of file_path)
  with Sys_error msg -> raise (InvalidPath msg)

let combined_dictionary sys_dict_file_path user_dict_file_path =
  union (dict_from_file sys_dict_file_path) (dict_from_file user_dict_file_path)

let spell_check word spell_dict =
  mem word spell_dict || mem (String.lowercase_ascii word) spell_dict

let are_anagrams word1 word2 =
  let n = String.length word1 in
  if n <> String.length word2 then false
  else
    let count = Array.make 256 0 in
    for i = 0 to n - 1 do
      let c_idx1 = Char.code word1.[i] in
      let c_idx2 = Char.code word2.[i] in
      count.(c_idx1) <- count.(c_idx1) + 1;
      count.(c_idx2) <- count.(c_idx2) - 1
    done;
    Array.for_all (fun cnt -> cnt = 0) count

let lookup_corrections word spell_dict =
  filter
    (fun elt -> BatString.edit_distance word elt = 1 || are_anagrams word elt)
    spell_dict

let print_corrections word corrections =
  let corr_prints =
    List.mapi
      (fun x y ->
        Printf.sprintf "%d) %s" (x + 1) y
        ^ if x mod 5 = 4 then "\n" else "\t\t\t")
      corrections
  in
  Printf.printf "\n===== Options for \"%s\" =====\n\n%s\n\n" word
    (if List.length corr_prints > 0 then
       BatList.reduce (fun x y -> x ^ y) corr_prints
     else "\n")

let add_to_dict word (spell_dict : t) = add word spell_dict

let update_user_dict file_path user_dict =
  BatFile.write_lines file_path (enum user_dict)

type split =
  | Word of string
  | NonWord of string

let word_of_str str = Word str
let non_word_of_str str = NonWord str

let str_of_split = function
  | NonWord s -> s
  | Word s -> s

let sprint_splits splts =
  if List.length splts > 0 then
    "["
    ^ BatList.reduce (fun s t -> s ^ "; " ^ t) (List.map str_of_split splts)
    ^ "]"
  else "[]"

let is_word = function
  | Word _ -> true
  | NonWord _ -> false

(* Regular expression for words *)
let word_regex = Str.regexp "[a-zA-Z]+"

(** [get_splits_aux acc start_pos line] is the [acc] with the accumulation of
    words and non-words of [line] starting at position [start_pos]. *)
let rec get_splits_aux acc start_pos line =
  if start_pos >= String.length line then List.rev acc
  else
    match line with
    | "" -> List.rev acc
    | str -> (
        try
          let fst_match_pos = Str.search_forward word_regex str start_pos in
          let fst_match = Str.matched_string str in
          let non_word = String.sub str start_pos (fst_match_pos - start_pos) in
          if non_word <> "" then
            get_splits_aux
              (Word fst_match :: NonWord non_word :: acc)
              (fst_match_pos + String.length fst_match)
              line
          else
            get_splits_aux (Word fst_match :: acc)
              (fst_match_pos + String.length fst_match)
              line
        with Not_found ->
          List.rev
            (NonWord (String.sub str start_pos (String.length str - start_pos))
            :: acc))

(** [get_splits line] is the words and non-words of [line]. *)
let get_splits line = get_splits_aux [] 0 line
