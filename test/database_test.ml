open OUnit2
open Final_project
open Csv_database

(* create a temporary test database *)
let temp_db_path = Filename.temp_file "csvdb_test" ".csv"

(* Setup for testing *)
let setup () =
  (try Unix.unlink temp_db_path with Unix.Unix_error _ -> ());
  create temp_db_path

(* Comprehensive test focused on student records with name and netid *)
let test_student_database _ =
  (* Initialize test database *)
  let db = setup () in

  (* Test empty database *)
  let initial_records = get_all db in
  assert_equal 0
    (List.length initial_records)
    ~msg:"Database should start empty";

  (* Add student records with name and netid fields *)
  add db [ ("name", "Tailai Ying"); ("netid", "tty6") ];
  add db [ ("name", "Andrew Park"); ("netid", "acp225") ];
  add db [ ("name", "Deniz Sanchez"); ("netid", "ds2472") ];

  (* Verify record count *)
  let records = get_all db in
  assert_equal 3 (List.length records) ~msg:"Should have 3 records";

  (* Get the ID of first student *)
  let alice_record =
    List.find (fun r -> List.assoc "name" r = "Tailai Ying") records
  in
  let alice_id = List.assoc "id" alice_record in

  (* Test get_by_id for Alice *)
  match get_by_id db alice_id with
  | None -> assert_failure "Student record not found by ID"
  | Some record ->
      assert_equal "Tailai Ying" (List.assoc "name" record)
        ~msg:"Name should match";
      assert_equal "tty6" (List.assoc "netid" record) ~msg:"NetID should match";

      (* Test query by netid *)
      let bob_query = get_by_query db [ ("netid", "acp225") ] in
      assert_equal 1 (List.length bob_query)
        ~msg:"Query should find exactly 1 student";
      assert_equal "Andrew Park"
        (List.assoc "name" (List.hd bob_query))
        ~msg:"Name should match Bob";

      (* Test update a student record *)
      add db [ ("id", alice_id); ("name", "Tailai Ting"); ("netid", "tty6") ];
      let updated_record =
        match get_by_id db alice_id with
        | None -> assert_failure "Updated record not found"
        | Some r -> r
      in
      assert_equal "Tailai Ting"
        (List.assoc "name" updated_record)
        ~msg:"Name should be updated";
      assert_equal "tty6"
        (List.assoc "netid" updated_record)
        ~msg:"NetID should be unchanged";

      (* Test delete by ID *)
      assert_bool "Delete should succeed" (delete db alice_id);
      assert_equal 2
        (List.length (get_all db))
        ~msg:"Should have 2 records after deletion";

      (* Test delete by query (netid) *)
      let deleted_count = delete_by_query db [ ("netid", "acp225") ] in
      assert_equal 1 deleted_count ~msg:"Should delete exactly 1 record";
      assert_equal 1
        (List.length (get_all db))
        ~msg:"Should have 1 record remaining";

      (* Test adding record with additional fields beyond name and netid *)
      add db
        [ ("name", "Lucas Doell"); ("netid", "lmd248"); ("year", "sophomore") ];

      (* Check record retrieval with additional field *)
      let dave_query = get_by_query db [ ("netid", "lmd248") ] in
      let dave_record = List.hd dave_query in
      assert_equal "Lucas Doell"
        (List.assoc "name" dave_record)
        ~msg:"Name should match";
      assert_equal "sophomore"
        (List.assoc "year" dave_record)
        ~msg:"Year field should be present";

      (* Create new database instance to test persistence *)
      let db2 = create temp_db_path in
      let records2 = get_all db2 in
      assert_equal 2 (List.length records2)
        ~msg:"Data should persist between instances";

      (* Clean up test file *)
      Unix.unlink temp_db_path

(* Test suite *)
let suite =
  "Student CSV Database Test Suite"
  >::: [ "Student Database Operations" >:: test_student_database ]

let () = run_test_tt_main suite
