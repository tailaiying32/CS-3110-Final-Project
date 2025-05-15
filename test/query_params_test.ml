open OUnit2
open Final_project

let tests =
  "Query Parameters test suite"
  >::: [
         (* Testing empty *)
         ( "empty query params should be an empty list" >:: fun _ ->
           assert_equal "" (Query_params.to_string Query_params.empty) );
         (* Testing parse_query_string *)
         ( "parse_query_string with empty string returns empty" >:: fun _ ->
           assert_equal Query_params.empty (Query_params.parse_query_string "")
         );
         ( "parse_query_string with single parameter" >:: fun _ ->
           let params = Query_params.parse_query_string "name=john" in
           assert_equal (Some "john") (Query_params.get "name" params) );
         ( "parse_query_string with multiple parameters" >:: fun _ ->
           let params =
             Query_params.parse_query_string "name=john&age=25&city=ithaca"
           in
           assert_equal (Some "john") (Query_params.get "name" params);
           assert_equal (Some "25") (Query_params.get "age" params);
           assert_equal (Some "ithaca") (Query_params.get "city" params) );
         ( "parse_query_string with URL encoded characters" >:: fun _ ->
           let params =
             Query_params.parse_query_string "name=john%20doe&city=new%20york"
           in
           assert_equal (Some "john doe") (Query_params.get "name" params);
           assert_equal (Some "new york") (Query_params.get "city" params) );
         ( "parse_query_string ignores malformed parameters" >:: fun _ ->
           let params =
             Query_params.parse_query_string "valid=param&invalid&name=john"
           in
           assert_equal (Some "param") (Query_params.get "valid" params);
           assert_equal (Some "john") (Query_params.get "name" params);
           assert_equal None (Query_params.get "invalid" params) );
         (* Testing of_string *)
         ( "of_string with no query string returns empty" >:: fun _ ->
           assert_equal Query_params.empty (Query_params.of_string "/path") );
         ( "of_string with empty query string returns empty" >:: fun _ ->
           assert_equal Query_params.empty (Query_params.of_string "/path?") );
         ( "of_string with query parameters" >:: fun _ ->
           let params = Query_params.of_string "/path?name=john&age=25" in
           assert_equal (Some "john") (Query_params.get "name" params);
           assert_equal (Some "25") (Query_params.get "age" params) );
         (* Testing get *)
         ( "get returns None for non-existent key" >:: fun _ ->
           let params = Query_params.parse_query_string "name=john" in
           assert_equal None (Query_params.get "age" params) );
         ( "get returns first value for duplicate keys" >:: fun _ ->
           let params =
             Query_params.parse_query_string "name=john&name=jane&name=bob"
           in
           assert_equal (Some "john") (Query_params.get "name" params) );
         (* Testing get_all *)
         ( "get_all returns empty list for non-existent key" >:: fun _ ->
           let params = Query_params.parse_query_string "name=john" in
           assert_equal [] (Query_params.get_all "age" params) );
         ( "get_all returns all values for duplicate keys" >:: fun _ ->
           let params =
             Query_params.parse_query_string "name=john&name=jane&name=bob"
           in
           assert_equal [ "john"; "jane"; "bob" ]
             (Query_params.get_all "name" params) );
         (* Testing to_string *)
         ( "to_string with empty params returns empty string" >:: fun _ ->
           assert_equal "" (Query_params.to_string Query_params.empty) );
         ( "to_string with single parameter" >:: fun _ ->
           let params = Query_params.parse_query_string "name=john" in
           assert_equal "?name=john" (Query_params.to_string params) );
         ( "to_string with multiple parameters" >:: fun _ ->
           let params =
             Query_params.parse_query_string "name=john&age=25&city=ithaca"
           in
           let str = Query_params.to_string params in
           assert_bool "contains name=john"
             (String.contains str 'n' && String.contains str 'j');
           assert_bool "contains age=25"
             (String.contains str 'a' && String.contains str '2');
           assert_bool "contains city=ithaca"
             (String.contains str 'c' && String.contains str 'i') );
         ( "to_string with URL encoded characters" >:: fun _ ->
           let params =
             Query_params.parse_query_string "name=john%20doe&city=new%20york"
           in
           let str = Query_params.to_string params in
           assert_bool "contains encoded space in name"
             (String.contains str '%' && String.contains str '2'
            && String.contains str '0');
           assert_bool "starts with ?" (str.[0] = '?') );
       ]

let _ = run_test_tt_main tests
