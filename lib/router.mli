type t

val init : unit -> t
(** [init ()] is an empty router, with no path mappings. *)

val add : t -> string -> string -> (Body.t -> Response.response) -> t
(** [add router path fn] adds the [(path, fn)] pair to the router. *)

val get_response : t -> string -> string -> Body.t -> Response.response
(** [get_response router path arguments] is the [response] obtained when the
    function corresponding to [path] is applied to [arguments]. *)
