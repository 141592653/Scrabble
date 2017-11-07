(**Nom de la map*)
val get_name : unit -> string
					 
(**Tableau représentant le plateau*)
val board : char array array
	      
(**Tableau contenant l'ensemble des joueurs*)
val get_players : unit -> Player.player array
			    
(**Numéro du joueur dont c'est le tour*)
val get_turn : unit -> int
			 
(**Tests functions *)		
val tests : OUnit2.test list
