type t
(** The type representing query parameters as key-value pairs *)

val empty : t
(** An empty set of query parameters *)

val parse_query_string : string -> t
(** [parse_query_string query_str] parses a query string (without the leading
    '?') into a set of query parameters. For example, "foo=bar&baz=qux" becomes
    [("foo", "bar"); ("baz", "qux")]. Invalid parameters are ignored. *)

val of_string : string -> t
(** [of_string str] parses a full URL path with potential query string into a
    set of query parameters. For example, "/path?foo=bar" extracts just the
    query parameters [("foo", "bar")]. If no query string is present, returns
    empty parameters. *)

val get : string -> t -> string option
(** [get key params] returns [Some value] if [key] exists in [params], otherwise
    returns [None]. If multiple values exist for the key, returns the first one.
*)

val get_all : string -> t -> string list
(** [get_all key params] returns all values associated with [key] in [params].
    Returns an empty list if the key doesn't exist. *)

val to_string : t -> string
(** [to_string params] converts the query parameters back to a string
    representation, including the leading '?' if parameters exist. Returns an
    empty string for empty parameters. Values are properly URL-encoded. *)
