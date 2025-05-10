open Yojson.Basic
open Yojson.Basic.Util

let extract_json_block block =
  let len = String.length block in
  (* Get the start index of the json block *)
  let rec find_start i =
    if i >= len then None
    else if block.[i] = '{' then Some i
    else find_start (i + 1)
  in
  (* Get the end index of the json block, taking into account nested
     structure *)
  let rec find_end i depth =
    if i >= len then None
    else
      match block.[i] with
      | '{' -> find_end (i + 1) (depth + 1)
      | '}' -> if depth = 1 then Some i else find_end (i + 1) (depth - 1)
      | _ -> find_end (i + 1) depth
  in
  match find_start 0 with
  | None -> None
  | Some start -> (
      match find_end (start + 1) 1 with
      | None -> None
      | Some stop -> Some (String.sub block start (stop - start + 1)))

let assoc_of_json_string json_str =
  try
    let json = Yojson.Basic.from_string json_str in
    json |> to_assoc |> List.map (fun (key, value) -> (key, to_string value))
  with _ -> []
