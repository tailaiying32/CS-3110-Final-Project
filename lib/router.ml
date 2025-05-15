open Response

type t = (string * string * (Body.t -> QueryParams.t -> response)) list

let init () = []
let add (router : t) method_str path fn = (method_str, path, fn) :: router

let get_base_path url =
  match String.split_on_char '?' url with
  | base :: _ -> base
  | [] -> url

let rec get_response (router : t) method_str path args =
  let base_path = get_base_path path in
  let query_params = QueryParams.of_string path in
  match router with
  | [] -> not_found ()
  | (m, p, v) :: t ->
      if m = method_str && p = base_path then v args query_params
      else get_response t method_str path args
