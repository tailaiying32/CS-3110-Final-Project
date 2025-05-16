open Csv

type t = {
  filepath : string;
  mutable records : string list list;
  mutable headers : string list;
  mutable dirty : bool;
}

type record = (string * string) list
type query = (string * string) list

(* convert a string list list to a record list *)
let records_of_csv_data headers data =
  List.map
    (fun row ->
      List.mapi
        (fun i value ->
          if i < List.length headers then (List.nth headers i, value)
          else ("column" ^ string_of_int i, value))
        row)
    data

(* convert a record list to string list list *)
let csv_data_of_records headers records =
  List.map
    (fun record ->
      List.map
        (fun header -> try List.assoc header record with Not_found -> "")
        headers)
    records

(* autoincrement the id for a new record *)
let next_id_from_database records =
  let current_max =
    List.fold_left
      (fun max_id record ->
        try
          let id_str = List.assoc "id" record in
          (* try to parse id as integer *)
          try
            let id_int = int_of_string id_str in
            if id_int > max_id then id_int else max_id
          with Failure _ ->
            (* if not integer, just keep current max *)
            max_id
        with Not_found -> max_id)
      0 records
  in
  string_of_int (current_max + 1)

(* make sure new record has an id field *)
let ensure_id_field db record =
  if List.mem_assoc "id" record then record
  else
    let all_records = records_of_csv_data db.headers db.records in
    let next_id = next_id_from_database all_records in
    ("id", next_id) :: record

(* create a new database *)
let create filepath =
  let headers, records =
    try
      let csv = Csv.load filepath in
      match csv with
      | [] -> ([ "id" ], [])
      | headers :: data ->
          (* "id" field must always be in headers *)
          let headers =
            if List.mem "id" headers then headers else "id" :: headers
          in
          (headers, data)
    with Sys_error _ -> ([ "id" ], [] (* default if file doesn't exist *))
  in
  { filepath; records; headers; dirty = false }

(* save the database to disk *)
let save db =
  if db.dirty then
    try
      let data = db.headers :: db.records in
      Csv.save db.filepath data;
      db.dirty <- false
    with e ->
      Printf.eprintf "Error saving database: %s\n" (Printexc.to_string e)

(* get all records *)
let get_all db = records_of_csv_data db.headers db.records

(* match record against query *)
let matches_query record query =
  List.for_all
    (fun (field, value) ->
      try List.assoc field record = value with Not_found -> false)
    query

(* get records matching a query *)
let get_by_query db query =
  let records = records_of_csv_data db.headers db.records in
  List.filter (fun r -> matches_query r query) records

(* get a record by ID *)
let get_by_id db id =
  let records = records_of_csv_data db.headers db.records in
  try
    Some
      (List.find
         (fun r -> try List.assoc "id" r = id with Not_found -> false)
         records)
  with Not_found -> None

(* add a record *)
let add db record =
  let record_with_id = ensure_id_field db record in
  let id = List.assoc "id" record_with_id in

  (* convert existing records to assoc lists *)
  let records_assoc = records_of_csv_data db.headers db.records in

  (* remove existing record with same id, if any *)
  let filtered_records =
    List.filter
      (fun r -> try List.assoc "id" r <> id with Not_found -> true)
      records_assoc
  in

  (* update headers with any new fields *)
  let all_headers = db.headers in
  let new_headers =
    List.fold_left
      (fun acc (field, _) ->
        if not (List.mem field acc) then acc @ [ field ] else acc)
      all_headers record_with_id
  in

  (* update database *)
  db.headers <- new_headers;
  db.records <-
    csv_data_of_records new_headers (record_with_id :: filtered_records);
  db.dirty <- true;
  save db

(* delete a record by id *)
let delete db id =
  let orig_len = List.length db.records in

  (* convert to assoc lists for easier filtering *)
  let records_assoc = records_of_csv_data db.headers db.records in

  let filtered_records =
    List.filter
      (fun r -> try List.assoc "id" r <> id with Not_found -> true)
      records_assoc
  in

  db.records <- csv_data_of_records db.headers filtered_records;
  let deleted = List.length filtered_records < orig_len in

  if deleted then begin
    db.dirty <- true;
    save db
  end;
  deleted

(* delete records matching a query *)
let delete_by_query db query =
  let orig_len = List.length db.records in

  (* convert to assoc lists for easier filtering *)
  let records_assoc = records_of_csv_data db.headers db.records in

  let filtered_records =
    List.filter (fun r -> not (matches_query r query)) records_assoc
  in

  db.records <- csv_data_of_records db.headers filtered_records;
  let deleted = orig_len - List.length filtered_records in

  if deleted > 0 then begin
    db.dirty <- true;
    save db
  end;
  deleted
