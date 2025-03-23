type response

val response_of : int -> string -> string -> response
(** [response_of code header body] is the response data type with code [code],
    header [header], and body [body]. *)

val string_of_response : response -> string
(** [string_of_response response] is the string representation of [response]. *)

val not_found : unit -> response
(** [not_found ()] is the response corresponding to 404 Not Found. *)
