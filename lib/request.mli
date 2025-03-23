type request

val request_of : string -> string -> string -> request
(** [request_of path header body] is the request data type that should be
    handled by path [path], with header [header] and body [body]. *)
