type request = {
  host : string;
  header : string;
  body : string;
}

let request_of host header body = { host; header; body }
