type response = {
  status_code : int;
  status_message : string;
  headers : Headers.t;
  body : Body.t;
}

let response_of status_code status_message headers body =
  { status_code; status_message; headers; body }

let string_of_status status_code status_message =
  Printf.sprintf "===== CODE/MESSAGE =====\n%d : %s\n" status_code
    status_message

let string_of_response response =
  Printf.sprintf "%s\n%s\n%s\n"
    (string_of_status response.status_code response.status_message)
    (Headers.string_of_t response.headers)
    (Body.string_of_t response.body)

let not_found () =
  response_of 404 "Not found" (Headers.t_of "" "") (Body.t_of_assoc_lst [])
