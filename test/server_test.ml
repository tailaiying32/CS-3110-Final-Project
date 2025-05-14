open OUnit2
open Lwt
open Final_project
open Tcp_server

let socketpair () =
  let r, w = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  (Lwt_unix.of_unix_file_descr w, r)

let read_all fd =
  let buf = Bytes.create 4096 in
  (* read once should be enough here, most responses <4K *)
  let n = Unix.read fd buf 0 (Bytes.length buf) in
  Bytes.sub_string buf 0 n

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

let test_parse_post_request _ =
  (* Even though parse_request ignores headers & body, it should still correctly
     pull out the method and path. *)
  let request_str = "POST /submit HTTP/1.0\r\nContent-Length: 5\r\n\r\nhello" in
  let request = parse_request request_str in
  assert_equal "POST" (Request.request_method request);
  assert_equal "/submit" (Request.url request)

let test_read_request_single_chunk _ =
  (* Create a pair of connected UNIX-domain sockets *)
  let fd_read, fd_write = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let client_sock = Lwt_unix.of_unix_file_descr fd_read in
  let raw = "GET / HTTP/1.1\r\nHost: test\r\n\r\n" in
  (* Write the entire request in one go *)
  let _ = Unix.write fd_write (Bytes.of_string raw) 0 (String.length raw) in
  (* read_request should return the full string in one shot *)
  let got = Lwt_main.run (read_request client_sock) in
  assert_equal ~msg:"single‑chunk read_request" raw got

let test_read_request_multiple_chunks _ =
  let fd_read, fd_write = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let client_sock = Lwt_unix.of_unix_file_descr fd_read in
  let part1 = "GET /multi" in
  let part2 = "part HTTP/1.1\r\nHost: test\r\n\r\n" in
  (* Simulate two separate TCP fragments *)
  let _ = Unix.write fd_write (Bytes.of_string part1) 0 (String.length part1) in
  let _ = Unix.write fd_write (Bytes.of_string part2) 0 (String.length part2) in
  let got = Lwt_main.run (read_request client_sock) in
  assert_equal ~msg:"multi‑chunk read_request" (part1 ^ part2) got

let test_read_request_eof _ =
  (* If the client half of the socketpair is closed immediately, Lwt_unix.read
     will return 0 and we'll hit the 0‑case. *)
  let fd_read, fd_write = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.close fd_write;
  let client_sock = Lwt_unix.of_unix_file_descr fd_read in
  let got = Lwt_main.run (read_request client_sock) in
  assert_equal ~msg:"EOF returns empty acc" "" got

let test_read_request_partial_then_eof _ =
  (* Write a chunk without any newline, then close; we should get exactly that
     chunk back when EOF is seen. *)
  let fd_read, fd_write = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let client_sock = Lwt_unix.of_unix_file_descr fd_read in
  let partial = "HELLO-WITH-NO-NEWLINE" in
  ignore
    (Unix.write fd_write (Bytes.of_string partial) 0 (String.length partial));
  Unix.close fd_write;
  let got = Lwt_main.run (read_request client_sock) in
  assert_equal ~msg:"partial then EOF should return exactly the data" partial
    got

let test_write_response_payload _ =
  (* set up a pair; server_sock is where write_response writes *)
  let server_sock, client_fd = socketpair () in

  (* build a minimal response: status 200, body "OK" *)
  let response =
    Response.response_of 200 "OK" (Headers.t_of "" "") (Body.t_of_assoc_lst [])
  in

  (* dummy request to exercise the logging format_status_code path *)
  let request =
    Request.request_of "GET" "/foo" (Headers.t_of "" "")
      (Body.t_of_assoc_lst [])
  in

  (* fire it off *)
  Lwt_main.run (write_response server_sock response request 0);

  (* read back what was sent *)
  let got = read_all client_fd in
  let want = Response.string_of_response response in

  assert_equal ~msg:"write_response should send exactly string_of_response" want
    got

let test_format_method _ =
  (* Test GET method - should be green *)
  let get_formatted = format_method "GET" in
  assert_equal ~printer:(fun x -> x) "\027[32mGET\027[0m" get_formatted;

  (* Test POST method - should be yellow *)
  let post_formatted = format_method "POST" in
  assert_equal ~printer:(fun x -> x) "\027[33mPOST\027[0m" post_formatted;

  (* Test unknown method - should be red *)
  let unknown_formatted = format_method "PUT" in
  assert_equal ~printer:(fun x -> x) "\027[31mPUT\027[0m" unknown_formatted;

  (* Test case insensitivity *)
  let get_lower_formatted = format_method "get" in
  assert_equal ~printer:(fun x -> x) "\027[32mget\027[0m" get_lower_formatted;

  (*Test delete method*)
  let delete_str = format_method "DELETE" in
  assert_equal ~printer:(fun x -> x) "\027[31mDELETE\027[0m" delete_str

let test_format_status_code _ =
  (* Test 2xx status codes - should be green *)
  let success_codes = [ 200; 201; 299 ] in
  List.iter
    (fun code ->
      let formatted = format_status_code code in
      assert_equal
        ~printer:(fun x -> x)
        (Printf.sprintf "\027[32m%d\027[0m" code)
        formatted)
    success_codes;

  (* Test 4xx and 5xx status codes - should be red *)
  let error_codes = [ 400; 404; 500; 503 ] in
  List.iter
    (fun code ->
      let formatted = format_status_code code in
      assert_equal
        ~printer:(fun x -> x)
        (Printf.sprintf "\027[31m%d\027[0m" code)
        formatted)
    error_codes;

  (* Test other status codes - should be yellow *)
  let other_codes = [ 100; 300; 301; 302 ] in
  List.iter
    (fun code ->
      let formatted = format_status_code code in
      assert_equal
        ~printer:(fun x -> x)
        (Printf.sprintf "\027[33m%d\027[0m" code)
        formatted)
    other_codes

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
  let config = { port; host = "localhost"; max_connections = 1 } in
  let server = create config in
  let handler _req =
    Response.response_of 200 "OK" (Headers.t_of "" "") (Body.t_of_assoc_lst [])
  in

  (* This is the Lwt thread we'll actually run *)
  let test_thread =
    (* start the server; returns a thread that resolves once accept_loop
       exits *)
    let server_thread = start server handler in

    (* give the OS a moment to bind/listen *)
    Lwt_unix.sleep 0.1 >>= fun () ->
    (* now it should be flagged as running *)
    assert_bool "server should be running" (is_running server);

    (* shut it down *)
    stop server >>= fun () ->
    (* give the accept_loop a moment to notice is_running = false *)
    Lwt_unix.sleep 0.1 >>= fun () ->
    assert_bool "server should have stopped" (not (is_running server));

    (* finally wait for the server thread to finish cleanly *)
    server_thread
  in

  (* run *that* whole thing under one Lwt_main.run *)
  Lwt_main.run test_thread

let test_start_bind_error _ =
  let bad_cfg = { port = -1; host = "localhost"; max_connections = 1 } in
  let srv = create bad_cfg in
  let handler _ = assert_failure "handler should not be invoked" in

  assert_raises (Invalid_argument "port must be 0-65535") (fun () ->
      Lwt_main.run (start srv handler))

let test_stop_without_start _ =
  let cfg =
    { port = get_random_port (); host = "localhost"; max_connections = 1 }
  in
  let srv = create cfg in
  (* srv.socket = None and is_running = false *)
  Lwt_main.run (stop srv);
  assert_bool "stop should leave is_running = false" (not (is_running srv))

let test_port_validation _ =
  (* Test valid port numbers *)
  let valid_ports = [ 0; 1; 8080; 65535 ] in
  List.iter
    (fun port ->
      let config = { port; host = "localhost"; max_connections = 1 } in
      let server = create config in
      assert_equal port (get_config server).port)
    valid_ports;

  (* Test invalid port numbers *)
  let invalid_ports = [ -1; -100; 65536; 100000 ] in
  List.iter
    (fun port ->
      let config = { port; host = "localhost"; max_connections = 1 } in
      let server = create config in
      assert_raises (Invalid_argument "port must be 0-65535") (fun () ->
          Lwt_main.run
            (start server (fun _ ->
                 assert_failure "handler should not be invoked"))))
    invalid_ports

let test_request_getters_and_string_of_request _ =
  let headers = Headers.t_of "localhost" "text/plain" in
  let body = Body.t_of_assoc_lst [ ("msg", "hello") ] in
  let req = Request.request_of "GET" "/index" headers body in

  (* Test accessors *)
  assert_equal "GET" (Request.request_method req);
  assert_equal "/index" (Request.url req);
  assert_equal headers (Request.headers req);
  assert_equal body (Request.body req);

  (* Test string_of_request formatting *)
  let stringified = Request.string_of_request req in
  assert_bool "string_of_request contains method"
    (String.contains stringified 'G');
  assert_bool "string_of_request contains body field"
    (String.contains stringified 'm')

let test_response_getters _ =
  let headers = Headers.t_of "localhost" "text/plain" in
  let body = Body.t_of_assoc_lst [ ("msg", "hello") ] in
  let resp = Response.response_of 200 "OK" headers body in

  (* Test accessors *)
  assert_equal 200 (Response.status_code resp);
  assert_equal "OK" (Response.status_message resp);
  assert_equal headers (Response.headers resp);
  assert_equal body (Response.body resp)

let test_parse_request_with_json_body _ =
  let json_body = {|{"user": "deniz", "role": "student"}|} in
  let request_str =
    "POST /json HTTP/1.1\r\nContent-Type: application/json\r\n\r\n" ^ json_body
  in
  let req = Tcp_server.parse_request request_str in
  let body = Request.body req in
  assert_equal "POST" (Request.request_method req);
  assert_equal "/json" (Request.url req);
  assert_equal (Some "deniz") (Body.safe_lookup "user" body);
  assert_equal (Some "student") (Body.safe_lookup "role" body)

let server_creation_tests =
  [
    "test_create_server" >:: test_create_server;
    "test_port_validation" >:: test_port_validation;
  ]

let request_parsing_tests =
  [
    "test_parse_request" >:: test_parse_request;
    "test_parse_malformed_request" >:: test_parse_malformed_request;
    "test_parse_empty_request" >:: test_parse_empty_request;
    "test_parse_incomplete_request" >:: test_parse_incomplete_request;
    "test_request_getters" >:: test_request_getters_and_string_of_request;
    "test_parse_post_request" >:: test_parse_post_request;
    "test_parse_request_with_json_body" >:: test_parse_request_with_json_body;
  ]

let request_reading_tests =
  [
    "test_read_request_single_chunk" >:: test_read_request_single_chunk;
    "test_read_request_multiple_chunks" >:: test_read_request_multiple_chunks;
    "test_read_request_eof" >:: test_read_request_eof;
    "test_read_request_partial_then_eof" >:: test_read_request_partial_then_eof;
  ]

let response_tests =
  [
    "test_write_response_payload" >:: test_write_response_payload;
    "test_format_method" >:: test_format_method;
    "test_format_status_code" >:: test_format_status_code;
    "test_response_getters" >:: test_response_getters;
  ]

let server_lifecycle_tests =
  [
    "test_server_lifecycle" >:: test_server_lifecycle;
    "test_start_bind_error" >:: test_start_bind_error;
    "test_stop_without_start" >:: test_stop_without_start;
  ]

let suite =
  "TCP Server Test Suite"
  >::: List.flatten
         [
           server_creation_tests;
           request_parsing_tests;
           request_reading_tests;
           response_tests;
           server_lifecycle_tests;
         ]

let () = run_test_tt_main suite
