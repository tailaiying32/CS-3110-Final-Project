open OUnit2
open Lwt
open Final_project
open Tcp_server

let test_create_server _ =
  let config = { port = 8080; host = "localhost"; max_connections = 10 } in
  let server = create config in
  let server_config = get_config server in
  assert_equal config.port server_config.port;
  assert_equal config.host server_config.host;
  assert_equal config.max_connections server_config.max_connections;
  assert_equal false (is_running server)

let test_parse_request _ =
  let request_str = "GET /test HTTP/1.1\r\nHost: localhost\r\n\r\n" in
  let request = parse_request request_str in
  assert_equal "GET" (Request.request_method request);
  assert_equal "/test" (Request.url request)

let test_parse_malformed_request _ =
  (* Test request with just a method - should be treated as malformed *)
  let malformed_str = "GET\r\nHost: localhost\r\n\r\n" in
  let request = parse_request malformed_str in
  assert_equal "UNKNOWN" (Request.request_method request);
  assert_equal "/" (Request.url request);

  (* Test request with empty first line - should be treated as malformed *)
  let empty_first_line = "\r\nHost: localhost\r\n\r\n" in
  let request2 = parse_request empty_first_line in
  assert_equal "UNKNOWN" (Request.request_method request2);
  assert_equal "/" (Request.url request2);

  (* Test request with too many parts - should still parse first two parts
     correctly *)
  let extra_parts = "GET /test HTTP/1.1 extra\r\nHost: localhost\r\n\r\n" in
  let request3 = parse_request extra_parts in
  assert_equal "GET" (Request.request_method request3);
  assert_equal "/test" (Request.url request3)

let test_parse_empty_request _ =
  (* Test empty request *)
  let empty_str = "" in
  let request = parse_request empty_str in
  assert_equal "UNKNOWN" (Request.request_method request);
  assert_equal "/" (Request.url request)

let test_parse_incomplete_request _ =
  (* Test request with missing parts *)
  let incomplete_str = "GET" in
  let request = parse_request incomplete_str in
  assert_equal "UNKNOWN" (Request.request_method request);
  assert_equal "/" (Request.url request)

let get_random_port () =
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_any, 0));
  Unix.listen sock 1;
  let port =
    match Unix.getsockname sock with
    | Unix.ADDR_INET (_, port) -> port
    | _ -> failwith "Failed to get port number"
  in
  Unix.close sock;
  port

let test_server_lifecycle _ =
  let port = get_random_port () in
  let config = { port; host = "localhost"; max_connections = 10 } in
  let server = create config in
  let handler _ =
    Response.response_of 200 "OK" (Headers.t_of "" "") (Body.t_of_assoc_lst [])
  in

  (* Start the server in a separate thread *)
  let server_thread = start server handler in

  (* Create a timeout to prevent hanging *)
  let timeout = Lwt_unix.sleep 2.0 >>= fun () -> Lwt.return_unit in

  (* Wait for server to start and verify it's running *)
  Lwt_unix.sleep 0.1 >>= fun () ->
  assert_equal true (is_running server);

  (* Stop the server *)
  stop server >>= fun () ->
  (* Wait for server to stop and verify it's stopped *)
  Lwt_unix.sleep 0.1 >>= fun () ->
  assert_equal false (is_running server);

  (* Wait for either the server thread to complete or timeout *)
  Lwt.pick [ server_thread; timeout ] >>= fun () -> Lwt.return_unit

let suite =
  "TCP Server Test Suite"
  >::: [
         "test_create_server" >:: test_create_server;
         "test_parse_request" >:: test_parse_request;
         "test_parse_malformed_request" >:: test_parse_malformed_request;
         "test_parse_empty_request" >:: test_parse_empty_request;
         "test_parse_incomplete_request" >:: test_parse_incomplete_request;
         ( "test_server_lifecycle" >:: fun ctx ->
           try Lwt_main.run (test_server_lifecycle ctx) with
           | Unix.Unix_error (err, func, arg) ->
               failwith
                 (Printf.sprintf "Unix error in %s: %s (%s)" func
                    (Unix.error_message err) arg)
           | exn -> failwith (Printexc.to_string exn) );
       ]

let () = run_test_tt_main suite
