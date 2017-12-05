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

	 (**True if the player has given up*)
	 val give_up : bool
			   
	 (**Asks the player to play*)
	 method virtual play : string -> unit

	 (** Name getter*)
	 method get_name : string

	 (**Score getter*)
	 method get_score : int

	 (**Letters getter*)
	 method get_letters : string

	 (**Give up getter*)
	 method given_up : bool

	 (**Is theµ player human*)
	 method virtual is_human : bool



       end

(** This class represents human players *)
class humanPlayer : string -> int -> string ->
      object
	inherit player
	method play : string -> unit
	method is_human : bool
      end
