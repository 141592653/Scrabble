(**Asks for yes or no repeatedly*)
val ask_bool : unit -> bool

(**Asks for settings for the new game*)
val ask_new_game : unit -> unit

(**Pretty printing of a board*)
val pp_board : Format.formatter -> char array array  -> unit

(** The main loop of the program*)
val main_loop : unit -> unit
