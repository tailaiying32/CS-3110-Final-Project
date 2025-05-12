val seed_from_date : unit -> int
(** [seed_from_date ()] is an integer seed based on today's date for
    reproducible randomness. *)

val word_bag : string array
(** [word_bag] is all valid 5-letter words for wordle. *)

val word_today : string
(** [word_today] is the 5-letter word for today. *)

val check_word : string -> string
(** [check_word guess] is an explanation of how well [guess] matches
    [word_today]. *)

val add_attempt : string -> string
(** [add_attempt guess] adds [guess] to the list of attempts. *)

val reset_game : unit -> string list
(** [reset_game ()] resets the list of attempts to an empty list. *)

val delete_last_attempt : unit -> string option
(** [delete_last_attempt ()] deletes the last attempt from the list of attempts.
*)
