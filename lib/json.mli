val extract_json_block : string -> string option
(** [extract_json_block raw_string] is the section of [raw_string] corresponding
    to a JSON object, denoted by the presence of \{ and \}. *)

val assoc_of_json_string : string -> (string * string) list
(** [assoc_of_json_string json_string] is the association list representation of
    [json_string]. *)
