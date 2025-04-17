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
let f1 (input : string) =
  Response.response_of 200 "ok" header (Body.t_of_assoc_lst [ ("user", input) ])

let f2 (input : string) =
  Response.response_of 200 "ok" header
    (Body.t_of_assoc_lst [ ("user2", input) ])

(*empty router but with one response added to it. *)
let router_add1 = Router.(add empty_router "/foo" f1)
let exp_string = "Host: example.com\r\nContent-Type: sample"
let basic_header = Headers.t_of "example.com" "sample"

let tests =
  "Router test suite"
  >::: [
         ( "A newly initialized router should return 404 error for any path."
         >:: fun _ ->
           assert_equal (Response.not_found ())
             (Router.get_response empty_router "/foo" "something")
             ~printer:Response.string_of_response );
         ( "Should be able to properly call an added function." >:: fun _ ->
           assert_equal (f1 "yay")
             Router.(get_response router_add1 "/foo" "yay")
             ~printer:Response.string_of_response );
         (* ( "trailing slashes still works" >:: fun _ -> assert_equal (f1
            "yay") Router.(get_response router_add1 "/foo/" "yay")
            ~printer:Response.string_of_response ); *)
         ( "Should be case sensitive" >:: fun _ ->
           assert_equal (Response.not_found ())
             (Router.get_response router_add1 "/FoO" "yay")
             ~printer:Response.string_of_response );
         (* ( "Handle white space properly" >:: fun _ -> assert_equal (f1 "yay")
            (Router.get_response router_add1 "/foo " "yay")
            ~printer:Response.string_of_response ); *)

         (*[TODO: ask team how we handle adding same path twice, for now i
           assumed it will overwrite previous. ]*)
         ( "Adding path twice" >:: fun _ ->
           assert_equal (f2 "yay")
             Router.(get_response (add router_add1 "/foo" f2) "/foo" "yay")
             ~printer:Response.string_of_response );
         ( "adding empty string path should give 404 not found" >:: fun _ ->
           assert_equal (Response.not_found ())
             (Router.get_response router_add1 "" "yay")
             ~printer:Response.string_of_response );
         ( "test construction of a new header" >:: fun _ ->
           assert_equal exp_string (Headers.string_of_t basic_header)
             ~printer:(fun x -> x) );
       ]

let _ = run_test_tt_main tests
