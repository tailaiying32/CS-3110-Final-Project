type t
(** The type of a TCP server *)

type config = {
  port : int;
  host : string;
  max_connections : int;
}
(** Configuration for the TCP server *)

val create : config -> t
(** [create config] creates a new TCP server with the given configuration *)

val start : t -> (Request.request -> Response.response) -> unit Lwt.t
(** [start server handler] starts the server and uses [handler] to process
    incoming requests. The handler function takes a request and returns a
    response. *)

val stop : t -> unit Lwt.t
(** [stop server] gracefully stops the server and closes all connections *)

val is_running : t -> bool
(** [is_running server] returns whether the server is currently running *)

val get_config : t -> config
(** [get_config server] returns the current configuration of the server *)
