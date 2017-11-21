(**Asks for yes or no repeatedly*)
val ask_bool : unit -> bool

(**Pretty printing of a board*)
val pp_board : Format.formatter -> char array array  -> unit
