type t = (string * string) list

let t_of_assoc_lst assoc_lst : t = assoc_lst

let string_of_t body =
  List.fold_left
    (fun acc (k, v) -> acc ^ Printf.sprintf "\n%s : %s" k v)
    "===== BODY =====" body
