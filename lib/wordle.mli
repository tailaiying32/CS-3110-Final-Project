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
