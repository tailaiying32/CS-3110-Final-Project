type request = {
  path : string;
  header : string;
  body : string;
}

let request_of path header body = { path; header; body }
