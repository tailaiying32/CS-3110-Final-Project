type elt =
  | Simple of string * string
  | Nested of string * elt

type t = elt list

let key_of_elt = function
  | Simple (f, _) -> f
  | Nested (f, _) -> f

let keys = List.map key_of_elt
