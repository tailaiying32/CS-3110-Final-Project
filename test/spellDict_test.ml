open OUnit2
open Final_project.SpellingDictionary
open BatSet.String

(** [make_combined_dict_test expected_output sys_dict_file_path
     user_dict_file_path] is an assert_equal test for the [expected_output] and
    the combined_dictionary function called on [sys_dict_file_path] and
    [user_dict_file_path]. *)
let make_combined_dict_test expected_output sys_dict_file_path
    user_dict_file_path =
  Printf.sprintf
    "Spelling dictionary creation test for\n\
     System dictionary file : %s\n\
     User dictionary file : %s"
    sys_dict_file_path user_dict_file_path
  >:: fun _ ->
  assert_equal expected_output
    (combined_dictionary sys_dict_file_path user_dict_file_path)
    ~printer:sprint_t

(** [make_combined_dict_failure_test expected_output sys_dict_file_path
     user_dict_file_path] is an assert_raises test for the [expected_exception]
    and the combined_dictionary function called on [sys_dict_file_path] and
    [user_dict_file_path]. *)
let make_combined_dict_failure_test expected_exception sys_dict_file_path
    user_dict_file_path =
  Printf.sprintf
    "Spelling dictionary creation failure test for\n\
     System dictionary file : %s\n\
     User dictionary file : %s"
    sys_dict_file_path user_dict_file_path
  >:: fun _ ->
  assert_raises expected_exception (fun () ->
      combined_dictionary sys_dict_file_path user_dict_file_path)

let combined_dict_tests =
  "Test suite for [combined_dictionary]"
  >::: [
         make_combined_dict_failure_test
           (InvalidPath "blah: No such file or directory") "blahblah" "blah";
         make_combined_dict_failure_test
           (InvalidPath "blahblah: No such file or directory") "blah" "blahblah";
         make_combined_dict_failure_test
           (InvalidPath "data/test_dictionary1.txt: No such file or directory")
           "data/test_dictionary1.txt" "../data/SINGLE.TXT";
       ]

(** [make_spell_check_test expected_output word spell_dict] is an assert_equal
    test for the [expected_output] and the spell_check function called on [word]
    and [spell_dict]*)
let make_spell_check_test expected_output word spell_dict =
  Printf.sprintf "Spell check test for word : %s\n" word >:: fun _ ->
  assert_equal expected_output
    (spell_check word spell_dict)
    ~printer:string_of_bool

(* Note that I have not implemented test cases involving dictionary files that
   do not exist, because the logic for my [spell_check] function directly
   accepts a spelling dictionary as one of its arguments. In other words, if a
   dictionary file does not exist, the [InvalidPath] exception would be raised
   elsewhere.*)
let spell_check_tests =
  "Test suite for [spell_check]"
  >::: [
         make_spell_check_test true "hellohellohello"
           (combined_dictionary "../data/test_dictionary1.txt"
              "../data/test_dictionary1.txt");
         make_spell_check_test false "hellohellahello"
           (combined_dictionary "../data/test_dictionary1.txt"
              "../data/test_dictionary1.txt");
         make_spell_check_test true "hallahallahalla"
           (combined_dictionary "../data/test_dictionary1.txt"
              "../data/test_dictionary2.txt");
         make_spell_check_test false "hallahallahalla"
           (combined_dictionary "../data/test_dictionary1.txt"
              "../data/test_dictionary1.txt");
         make_spell_check_test true "abjuration"
           (combined_dictionary "../data/SINGLE.TXT" "../data/SINGLE.TXT");
       ]

(** [make_lookup_corrections_test expected_output word spell_dict] is an
    assert_equal test for the [expected_output] and the lookup_corrections
    function called on [word] and [spell_dict]*)
let make_lookup_corrections_test expected_output word spell_dict =
  Printf.sprintf "Corrections for word : %s\n" word >:: fun _ ->
  assert_equal expected_output
    (lookup_corrections word spell_dict)
    ~printer:sprint_t

(* Note that I have not implemented test cases involving dictionary files that
   do not exist, because the logic for my [lookup_corrections] function directly
   accepts a spelling dictionary as one of its arguments. In other words, if a
   dictionary file does not exist, the [InvalidPath] exception would be raised
   elsewhere.*)
let lookup_corrections_tests =
  "Test suite for [lookup_corrections]"
  >:::
  let comb_dict =
    combined_dictionary "../data/test_dictionary1.txt"
      "../data/test_dictionary1.txt"
  in
  [
    make_lookup_corrections_test (t_of_list []) "abjuration" comb_dict;
    make_lookup_corrections_test (t_of_list []) "helloherrohello" comb_dict;
    make_lookup_corrections_test
      (t_of_list [ "hellohellohello" ])
      "hellohellohell" comb_dict;
    make_lookup_corrections_test
      (t_of_list [ "hellohellohello" ])
      "hellohellohelloa" comb_dict;
    make_lookup_corrections_test
      (t_of_list [ "hellohellohello" ])
      "hellohellohella" comb_dict;
    make_lookup_corrections_test
      (t_of_list [ "hellohellohello" ])
      "bellohellohello" comb_dict;
    make_lookup_corrections_test
      (t_of_list [ "hellohellohello" ])
      "hellohallohello" comb_dict;
  ]

(** [make_get_splits_test expected_output line] is an assert_equal test for the
    [expected_output] and the get_splits function called on [line] *)
let make_get_splits_test expected_output line =
  Printf.sprintf "Splits for line : \"%s\"\n" line >:: fun _ ->
  assert_equal expected_output (get_splits line) ~printer:sprint_splits

let get_splits_tests =
  "Test suite for [get_splits]\n"
  >::: [
         make_get_splits_test [] "";
         make_get_splits_test [ non_word_of_str ";" ] ";";
         make_get_splits_test [ word_of_str "Cornell" ] "Cornell";
         make_get_splits_test
           [ word_of_str "Cornell"; non_word_of_str "'" ]
           "Cornell'";
         make_get_splits_test
           [ word_of_str "Cornell"; non_word_of_str "'"; word_of_str "s" ]
           "Cornell's";
         make_get_splits_test
           [
             word_of_str "Cornell";
             non_word_of_str " ";
             word_of_str "is";
             non_word_of_str " ";
             word_of_str "great";
             non_word_of_str "!";
           ]
           "Cornell is great!";
         make_get_splits_test
           [
             word_of_str "Isn";
             non_word_of_str "'";
             word_of_str "t";
             non_word_of_str " ";
             word_of_str "she";
             non_word_of_str " ";
             word_of_str "lovely";
           ]
           "Isn't she lovely";
       ]

let _ = run_test_tt_main combined_dict_tests
let _ = run_test_tt_main spell_check_tests
let _ = run_test_tt_main lookup_corrections_tests
let _ = run_test_tt_main get_splits_tests
