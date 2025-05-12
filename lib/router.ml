open Response

type t = (string * string * (Body.t -> response)) list

let init () = []
let add (router : t) method_str path fn = (method_str, path, fn) :: router

let rec get_response (router : t) method_str path args =
  match router with
  | [] -> not_found ()
  | (m, p, v) :: t ->
      if m = method_str && p = path then v args
      else get_response t method_str path args
