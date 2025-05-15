type t = (string * string) list

let empty = []

let parse_query_string query_str =
  if query_str = "" then empty
  else
    String.split_on_char '&' query_str
    |> List.filter_map (fun param ->
           match String.split_on_char '=' param with
           | [ key; value ] -> Some (Uri.pct_decode key, Uri.pct_decode value)
           | _ -> None)

let of_string query_str =
  match String.split_on_char '?' query_str with
  | [ _path; query ] -> parse_query_string query
  | _ -> empty

let get key params =
  List.find_map (fun (k, v) -> if k = key then Some v else None) params

let get_all key params =
  List.filter_map (fun (k, v) -> if k = key then Some v else None) params

let to_string params =
  match params with
  | [] -> ""
  | _ ->
      "?"
      ^ (params
        |> List.map (fun (k, v) -> Uri.pct_encode k ^ "=" ^ Uri.pct_encode v)
        |> String.concat "&")
