open Headers
open Body

type response

val response_of : int -> string -> Headers.t -> Body.t -> response
(** [response_of stts_cd stts_msg header body] is the response data type with
    code [stts_cd], status message [stts_msg], header [header], and body [body].
*)

val string_of_response : response -> string
(** [string_of_response response] is the string representation of [response]. *)

val not_found : unit -> response
(** [not_found ()] is the response corresponding to 404 Not Found. *)
