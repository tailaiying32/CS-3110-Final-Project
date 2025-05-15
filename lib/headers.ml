type t = {
  host : string;
  content_type : string;
  content_length : int;
}

let t_of host content_type = { host; content_type; content_length = 0 }

let t_of_with_length host content_type content_length =
  { host; content_type; content_length }

let string_of_t headers =
  Printf.sprintf "Content-Type: %s\r\nHost: %s\r\nContent-Length: %d\r\n"
    headers.content_type headers.host headers.content_length
