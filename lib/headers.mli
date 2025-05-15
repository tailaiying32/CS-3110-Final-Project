type t = {
  host : string;
  content_type : string;
  content_length : int;
}

val t_of : string -> string -> t
(** [t_of host content_type] creates a new header with the given host and
    content type, with content length 0 *)

val t_of_with_length : string -> string -> int -> t
(** [t_of_with_length host content_type content_length] creates a new header
    with the given host, content type and content length *)

val string_of_t : t -> string
(** [string_of_t headers] returns the string representation of the headers *)
