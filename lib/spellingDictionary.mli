exception InvalidPath of string
(** [InvalidPath] is raised when a path is given that is not a valid path to a
    dictionary. *)

exception EarlyQuitting
(** [EarlyQuitting] is raised when the driver program quits early, before going
    through all the words in the file.*)

type t
(** [type t] is a type representing a spelling dictionary *)

val sprint_t : t -> string
(** [sprint_t t_val] is the string representation of [t_val]. *)

val t_of_list : string list -> t
(** [t_of_list lst] is the type t representation of [lst]. *)

val list_of_t : t -> string list
(** [list_of_t t_val] is the string list representation of [t_val]. *)

val union : t -> t -> t
(** [union s t] is all the elements either in [s] or [t]. *)

val diff : t -> t -> t
(** [diff s t] returns the elements in [s] that are not in [t]. *)

val dict_from_file : string -> t
(** [dict_from_file file_path] is the spelling dictionary obtained from
    [file_path]. Raises: [InvalidPath msg] if the file_path is invalid *)

val combined_dictionary : string -> string -> t
(** [combined_dictionary sys_dict_file_path user_dict_file_path] is the [union]
    of the spelling dictionary at [sys_dict_file_path] and = the spelling
    dictionary at [user_dict_file_path]. Raises: [InvalidPath] if either
    [sys_dict_file_path] or [user_dict_file_path] or both cannot be accessed. *)

val spell_check : string -> t -> bool
(** [spell_check word spell_dict] is the whether or not [word] is correctly
    spelled according to [spell_dict]. *)

val lookup_corrections : string -> t -> t
(** [lookup_corrections word spell_dict] are the words in [spell_dict] with an
    edit distance of 1 from [word]. *)

val print_corrections : string -> string list -> unit
(** [print_corrections word corrections] prints [corrections] for [word] in a
    readable format. *)

val add_to_dict : string -> t -> t
(** [add_to_dict word spell_dict] is the [spell_dict] with [word] added to it.
*)

val update_user_dict : string -> t -> unit
(** [update_user_dict file_path spell_dict] updates the user dictionary at
    [file_path] to be [spell_dict]. *)

type split
(** [type split] is the type for the splits of a string. *)

val word_of_str : string -> split
(** [word_of_str str] is the [split] of [str] with constructor Word. *)

val non_word_of_str : string -> split
(** [non_word_of_str str] is the [split] of [str] with constructor NonWord. *)

val str_of_split : split -> string
(** [str_of_split splt] is the [str] value of [splt]. *)

val sprint_splits : split list -> string
(** [sprint_splits splts] is the string representation of [splts]. *)

val is_word : split -> bool
(** [is_word splt] is whether ot not splt is a valid word. *)

val get_splits : string -> split list
(** [get_splits line] is the splits of [line]. *)
