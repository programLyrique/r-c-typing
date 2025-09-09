(** Extra helpers for working with [in_channel].
    Mimics some of the Core [In_channel] API. *)

include module type of In_channel
(** Re-export everything from the stdlib [In_channel]. *)

val of_string : string -> in_channel
(** [of_string s] creates an [in_channel] that reads from the contents of [s]. 
    The channel should be closed with [close_in] when no longer needed. *)

val with_string : string -> f:(in_channel -> 'a) -> 'a
(** [with_string s ~f] creates an [in_channel] from [s],
    passes it to [f], and ensures the channel is closed afterwards. *)

val iter_lines : in_channel -> f:(string -> unit) -> unit
(** [iter_lines ic ~f] reads all lines from [ic] until EOF
    and calls [f] on each line. *)

val fold_lines : in_channel -> init:'a -> f:('a -> string -> 'a) -> 'a
(** [fold_lines ic ~init ~f] folds over the lines of [ic], starting
    with [init] and applying [f] to accumulate a result. *)