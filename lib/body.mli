type t

val t_of_assoc_lst : (string * string) list -> t
(** [t_of_assoc_lst assoc_lst] is the body type representation of [assoc_lst] *)

val string_of_t : t -> string
(** [string_of_t body] is the string representation of [body]. *)

val lookup : string -> t -> string
(** [lookup key body] is the value associated with [key] in [body]. *)

val safe_lookup : string -> t -> string option
(** [safe_lookup key body] is Some value associated with [key] in [body], or
    None if key doesn't exist. *)
