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
    val given_up : bool

    (**Asks the player to play*)
    method virtual play : string -> unit

    (**How to ask to the player to play*)
    method virtual ask_action : unit -> Action.action

    (** Name getter*)
    method get_name : string

    (**Score getter*)
    method get_score : int

    (**Score setter (the number given in argument will be added
     * to the overall score of the player)*)
    method add_to_score : int -> unit

    (**Letters getter*)
    method get_letters : string

    (**Give up getter*)
    method given_up : bool

    (**Use when the player wants to give up*)
    method give_up : unit -> unit

    (**Picks letters given by the UI*)
    method pick : string -> unit

    (** Number of letters missing for having enough letters*)
    method letters_missing : int

    (**Is the player human*)
			       (*method virtual is_human : bool*)

  end

(** This class represents human players *)
class humanPlayer : string -> int -> string ->
  object
    inherit player
    method play : string -> unit
    method  ask_action : unit -> Action.action
  end
