(** Ce module contient les classes représentant
 *  les joueurs humains ou non humains*)

(**Cette classe représente un joueur humain ou non humain*)
class virtual player : string -> int -> string ->
  object
    (** Name of the player *)
    val name : string

    (** Current score *)
    val score : int

    (** The hand of the player *)
    val letters : string

    (** True if the player has given up *)
    val given_up : bool

    (** Asks the player to play *)
    method play : string -> unit

    (** How to ask to the player to play *)
    method virtual ask_action : unit -> Action.action

    (** Display the current state of the game to the player *)
    method virtual send_game : string -> unit

    (** The player is local or in network *)
    method virtual network_player : bool

    (** Name getter *)
    method get_name : string

    (** Score getter *)
    method get_score : int

    (** Score setter (the number given in argument will be added to the overall score of the player) *)
    method add_to_score : int -> unit

    (** Letters getter *)
    method get_letters : string

    (** Give up getter *)
    method given_up : bool

    (** Use when the player wants to give up *)
    method give_up : unit -> unit

    (** Picks letters given by the UI *)
    method pick : string -> unit

    (** Number of letters missing for having enough letters *)
    method missing_letters : int

    (** State whether the set of letters of the argument is included
      * in the set of letters of the player*)
    method can_play : string -> bool


  end

(** This class represents human players *)
class humanPlayer : string -> int -> string ->
  object
    inherit player
    method play : string -> unit
    method ask_action : unit -> Action.action
    method send_game : string -> unit
    method network_player : bool
  end

(** Network player *)
class networkPlayer : string -> int -> string -> Unix.file_descr ->
  object
    inherit player
    val sock : Unix.file_descr
    method play : string -> unit
    method ask_action : unit -> Action.action
    method send_game : string -> unit
    method network_player : bool
  end
