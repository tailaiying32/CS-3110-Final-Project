open Headers

type response = {
  status_code : int;
  status_message : string;
  headers : t;
  body : Body.t;
}

let status_code response = response.status_code
let status_message response = response.status_message
let headers response = response.headers
let body response = response.body

let response_of status_code status_message headers body =
  { status_code; status_message; headers; body }

let string_of_status status_code status_message =
  Printf.sprintf "HTTP/1.1 %d %s" status_code status_message

let string_of_response response =
  let body = Body.string_of_t response.body in
  let body_length = String.length body in
  let headers_with_length =
    t_of_with_length response.headers.host response.headers.content_type
      body_length
  in
  let status = string_of_status response.status_code response.status_message in
  let headers = string_of_t headers_with_length in
  Printf.sprintf "%s\r\n%s\r\n%s" status headers body

let not_found () =
  response_of 404 "Not Found"
    (t_of "localhost" "text/plain")
    (Body.t_of_assoc_lst [])
