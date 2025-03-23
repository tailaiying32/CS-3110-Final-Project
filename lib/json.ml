type value =
  | Dict of (string * value) list
  | Entry of string

type t = (string * value) list

exception EmptyJson

let keys (json : t) = List.map fst json
let values (json : t) = List.map snd json
let pairs (json : t) = json

let rec add (json : t) key value : t =
  match json with
  | [] -> [ (key, value) ]
  | (k, v) :: t ->
      if k = key then (key, value) :: t else (k, v) :: add t key value

let rec del (json : t) key : t =
  match json with
  | [] -> []
  | (k, v) :: t -> if k = key then t else (k, v) :: del t key

let rec lookup (json : t) key =
  match json with
  | [] -> raise EmptyJson
  | (k, v) :: t -> if k = key then v else lookup t key
