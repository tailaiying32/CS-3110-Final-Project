type response = {
  code : int;
  header : string;
  body : string;
}

let response_of code header body = { code; header; body }

let string_of_response response =
  Printf.sprintf
    "===== CODE =====\n%d\n===== HEADER =====\n%s\n===== BODY =====\n%s\n"
    response.code response.header response.body

let not_found () = response_of 404 "Not found" ""
