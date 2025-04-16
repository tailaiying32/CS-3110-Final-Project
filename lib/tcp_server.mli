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

val parse_request : string -> Request.request
(** [parse_request request_str] parses a raw HTTP request string into a
    Request.t *)

val read_request : Lwt_unix.file_descr -> string Lwt.t
(** [read_request client_sock] reads a complete HTTP request from the client
    socket. Returns a string containing the raw request data. *)

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
