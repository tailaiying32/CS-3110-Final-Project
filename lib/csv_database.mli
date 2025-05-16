type t
(** type representing the csv database *)

type record = (string * string) list
(** type of a single record in the database (key-value pair) *)

type query = (string * string) list
(** a query for the database to filter records *)

val create : string -> t
(** [create filepath] craetes a new CSV database using the file at [filepath]
    creates a new file if [filepath] does not exist *)

val get_all : t -> record list
(** [get_all db] returns all records in the database *)

val get_by_query : t -> query -> record list
(** [get_by_query db query] returns all records matching the query *)

val get_by_id : t -> string -> record option
(** [get_by_id db id] returns the record with the given ID, if it exists *)

val add : t -> record -> unit
(** [add db record] adds a new record to the database. If a record with the same ID
    already exists, it will be replaced. *)

val delete : t -> string -> bool
(** [delete db id] deletes the record with the given ID from the database. Returns
    true if a record was deleted, false otherwise. *)

val delete_by_query : t -> query -> int
(** [delete_by_query db query] deletes all records matching the query. Returns the
    number of records deleted. *)
