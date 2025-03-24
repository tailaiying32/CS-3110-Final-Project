type request

val request_of : string -> string -> Headers.t -> Body.t -> request
(** [request_of request_method url headers body] is the request data type with
    [request_method] [url] [headers] [body]. *)

val string_of_request : request -> string
(** [string_of_request request] is the string representation of [request]. *)
