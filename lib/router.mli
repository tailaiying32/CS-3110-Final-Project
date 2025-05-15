type t

val init : unit -> t
(** [init ()] is an empty router, with no path mappings. *)

val add :
  t -> string -> string -> (Body.t -> Query_params.t -> Response.response) -> t
(** [add router method path fn] adds the [(method, path, fn)] triple to the
    router. The [fn] function now takes both a request body and query
    parameters. *)

val get_response : t -> string -> string -> Body.t -> Response.response
(** [get_response router method path body] is the [response] obtained when the
    function corresponding to [method] and [path] is applied to [body] and any
    query parameters extracted from [path]. *)
