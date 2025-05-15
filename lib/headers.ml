type t = {
  host : string;
  content_type : string;
}

let t_of host content_type = { host; content_type }

let string_of_t headers =
  Printf.sprintf "Content-Type: %s\r\nHost: %s\r\nContent-Length: 0\r\n"
    headers.content_type headers.host
