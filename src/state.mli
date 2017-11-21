(** Note sur le format de fichier. Un fichier json a la forme : 
 * 
 *
 * [{
 *   "name" : "Niveau 1",
 *
 *  "players" :
 *
 *  {
 *	"beauGosseDu84" : {
 *	    "score" : 2,
 *         "letters" : "AAUBYCE"
 *	},
 *	"42" : {
 *	    "score" : 5,
 *	    "letters" : "OYVYU"
 *	},
 *	"cotcotcodète" : {
 *	    "score" : 8,
 *	    "letters" : "__UIOI"
 *	},
 *	"winteriscoming" :{
 *	    "score" : 10,
 *	    "letters" : "EEEE_"
 *	}
 *  },
 *  "map" : "test/parse_test.txt",
 *  "turn" : 1
 * }]
 
 * Une grille de scrabble peut contenir : 
 * - Des lettres majuscules (ce sont les lettres standard)
 * - Des lettres minuscules (ce sont les jokers qui ont été placés)
 * - Des espaces (ce sont des cases vides)
 *)

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
