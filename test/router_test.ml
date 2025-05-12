open OUnit2
open Final_project

(*[TODO: ask team ] this test suite was supposed to be for just response but i
  cant test it without using body.ml headers.ml and response.ml Should we turn
  those into just one big interface? *)

(*empty initialized router*)
let empty_router = Router.init ()

(*Basic made up header. *)
let header = Headers.t_of "example.com" "text/plain"

(*Basic function to test add with. *)
let f1 text_body =
  Response.response_of 200 "ok" header
    (Body.t_of_assoc_lst [ ("user", Body.lookup "text" text_body) ])

let f2 text_body =
  Response.response_of 200 "ok" header
    (Body.t_of_assoc_lst [ ("user2", Body.lookup "text2" text_body) ])

(*empty router but with one response added to it with GET method *)
let router_add1 = Router.(add empty_router "GET" "/foo" f1)
let exp_string = "Host: example.com\r\nContent-Type: sample"
let basic_header = Headers.t_of "example.com" "sample"

let tests =
  "Router test suite"
  >::: [
         ( "A newly initialized router should return 404 error for any path."
         >:: fun _ ->
           assert_equal (Response.not_found ())
             (Router.get_response empty_router "GET" "/foo"
                (Body.t_of_assoc_lst [ ("nonsense", "nonsense") ]))
             ~printer:Response.string_of_response );
         ( "Should be able to properly call an added function." >:: fun _ ->
           assert_equal
             (f1 (Body.t_of_assoc_lst [ ("text", "yay") ]))
             Router.(
               get_response router_add1 "GET" "/foo"
                 (Body.t_of_assoc_lst [ ("text", "yay") ]))
             ~printer:Response.string_of_response );
         ( "Should be case sensitive for path" >:: fun _ ->
           assert_equal (Response.not_found ())
             (Router.get_response router_add1 "GET" "/FoO"
                (Body.t_of_assoc_lst [ ("text", "yay") ]))
             ~printer:Response.string_of_response );
         ( "Should be case sensitive for method" >:: fun _ ->
           assert_equal (Response.not_found ())
             (Router.get_response router_add1 "get" "/foo"
                (Body.t_of_assoc_lst [ ("text", "yay") ]))
             ~printer:Response.string_of_response );
         ( "Method not matching should return 404" >:: fun _ ->
           assert_equal (Response.not_found ())
             (Router.get_response router_add1 "POST" "/foo"
                (Body.t_of_assoc_lst [ ("text", "yay") ]))
             ~printer:Response.string_of_response );
         ( "Adding same path with different method should not overwrite"
         >:: fun _ ->
           let router_with_both = Router.add router_add1 "POST" "/foo" f2 in
           assert_equal
             (f1 (Body.t_of_assoc_lst [ ("text", "yay") ]))
             (Router.get_response router_with_both "GET" "/foo"
                (Body.t_of_assoc_lst [ ("text", "yay") ]))
             ~printer:Response.string_of_response;
           assert_equal
             (f2 (Body.t_of_assoc_lst [ ("text2", "yay") ]))
             (Router.get_response router_with_both "POST" "/foo"
                (Body.t_of_assoc_lst [ ("text2", "yay") ]))
             ~printer:Response.string_of_response );
         ( "Adding same path and method twice should overwrite previous"
         >:: fun _ ->
           assert_equal
             (f2 (Body.t_of_assoc_lst [ ("text2", "yay") ]))
             Router.(
               get_response
                 (add router_add1 "GET" "/foo" f2)
                 "GET" "/foo"
                 (Body.t_of_assoc_lst [ ("text2", "yay") ]))
             ~printer:Response.string_of_response );
         ( "Adding empty string path should give 404 not found" >:: fun _ ->
           assert_equal (Response.not_found ())
             (Router.get_response router_add1 "GET" ""
                (Body.t_of_assoc_lst [ ("text2", "yay") ]))
             ~printer:Response.string_of_response );
         ( "Empty method should give 404 not found" >:: fun _ ->
           assert_equal (Response.not_found ())
             (Router.get_response router_add1 "" "/foo"
                (Body.t_of_assoc_lst [ ("text2", "yay") ]))
             ~printer:Response.string_of_response );
         ( "Test construction of a new header" >:: fun _ ->
           assert_equal exp_string (Headers.string_of_t basic_header)
             ~printer:(fun x -> x) );
       ]

let _ = run_test_tt_main tests
