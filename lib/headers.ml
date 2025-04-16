type t = {
  host : string;
  content_type : string;
}

let t_of host content_type = { host; content_type }

let string_of_t headers =
  Printf.sprintf "Host: %s\r\nContent-Type: %s" headers.host
    headers.content_type
