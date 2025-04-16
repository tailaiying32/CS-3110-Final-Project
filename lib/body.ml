type t = (string * string) list

let t_of_assoc_lst assoc_lst : t = assoc_lst

let string_of_t body =
  let json_str =
    List.fold_left
      (fun acc (k, v) -> acc ^ Printf.sprintf "\"%s\": \"%s\"," k v)
      "{" body
  in
  let json_str =
    if json_str = "{" then "{}"
    else String.sub json_str 0 (String.length json_str - 1) ^ "}"
  in
  json_str
