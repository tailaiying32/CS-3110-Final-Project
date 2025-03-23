(** [value] is the type representing the valid values of a json object *)
type value =
  | Dict of (string * value) list
  | Entry of string

type t
(** [t] is the type representing json objects. *)

exception EmptyJson
(** [EmptyJson] is the exception to be raised when attempting to index and empty
    json object. *)

val keys : t -> string list
(** [keys json] is a list of the keys of [json]. *)

val values : t -> value list
(** [values json] is a list of the values of [json]. *)

val pairs : t -> (string * value) list
(** [pairs json] is the list representation of [json]. *)

val add : t -> string -> value -> t
(** [add json key value] is the [json] with the new [(key, value)] added. *)

val del : t -> string -> t
(** [del json key] is the [json] with [key] removed. *)

val lookup : t -> string -> value
(** [lookup json key] is the value in [json] paired with [key]. *)
