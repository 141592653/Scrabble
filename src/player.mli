(** Ce module contient les classes représentant
 *  les joueurs humains ou non humains*)

(**Cette classe représente un joueur humain ou non humain*)
class virtual player : string -> int -> string ->
  object
    (**Name of the player *)
    val name : string

    (**Current score*)
    val score : int

    (**The hand of the player*)
    val letters : string

    (**True if the player has given up*)
    val give_up : bool

    (**Asks the player to play*)
    method virtual play : string -> unit

    (**How to ask to the player to play*)
    method virtual ask_action : unit -> Action.action

    (** Name getter*)
    method get_name : string

    (**Score getter*)
    method get_score : int

    (**Letters getter*)
    method get_letters : string

    (**Give up getter*)
    method given_up : bool

    (**Picks letters given by the UI*)
    method pick : string -> unit

    (** Number of letters missing for having enough letters*)
    method letters_missing : int
  end

(** This class represents human players *)
class humanPlayer : string -> int -> string ->
  object
    inherit player
    method play : string -> unit
    method ask_action : unit -> Action.action
  end

(** Network player *)
class networkPlayer : string -> int -> string -> Unix.file_descr ->
  object
    inherit player
    val sock : Unix.file_descr
    method play : string -> unit
    method ask_action : unit -> Action.action
  end
