type request = {
  request_method : string;
  url : string;
  headers : Headers.t;
  body : Body.t;
}

let request_of request_method url headers body =
  { request_method; url; headers; body }

let string_of_request request =
  Printf.sprintf
    "===== METHOD =====\n\
     %s\n\
     ===== URL =====\n\
     %s\n\
     ===== HEADERS =====\n\
     %s\n\
     ===== BODY =====\n\
     %s\n"
    request.request_method request.url
    (Headers.string_of_t request.headers)
    (Body.string_of_t request.body)
