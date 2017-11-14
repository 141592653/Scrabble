(** Ce module contient les classes représentant 
 *  les joueurs humains ou non humains*)


(**Cette classe représente un joueur humain ou non humain*)
class virtual player : string -> int -> string ->
       object
			 
		     
	 (**Nom du joueur *)
	 val name : string

	 (**Score courant du joueur*)
	 val score : int

	 (**Le jeu du joueur*)
	 val letters : string
			   
	 (**Asks the player to play*)
	 method virtual play : unit -> unit

	 (** Name getter*)
	 method get_name : string

	 (**Score getter*)
	 method get_score : int

	 (**Letters getter*)
	 method get_letters : string

	 (**Prints the name and the game of a player*)
	 method print : char array array -> unit


       end

(** This class represents human players *)
class humanPlayer : string -> int -> string ->
      object
	inherit player
	method  play : unit -> unit
      end
