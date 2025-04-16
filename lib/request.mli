type request

val request_method : request -> string
(** [request_method request] returns the HTTP method of [request]. *)

val url : request -> string
(** [url request] returns the URL of [request]. *)

val headers : request -> Headers.t
(** [headers request] returns the headers of [request]. *)

val body : request -> Body.t
(** [body request] returns the body of [request]. *)

val request_of : string -> string -> Headers.t -> Body.t -> request
(** [request_of request_method url headers body] is the request data type with
    [request_method] [url] [headers] [body]. *)

val string_of_request : request -> string
(** [string_of_request request] is the string representation of [request]. *)
