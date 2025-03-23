open Response

type t = (string * (string -> response)) list

let init () = []
let add (router : t) path fn = (path, fn) :: router

let rec get_response (router : t) path args =
  match router with
  | [] -> not_found ()
  | (k, v) :: t -> if k = path then v args else get_response t path args
