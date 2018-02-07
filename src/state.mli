(** Note sur le format de fichier. Un fichier json a la forme :
 *
 *[{
 *   "name" : "Niveau 1",
 *   "players" :
 *   {
 *     "beauGosseDu84" : {
 *       "score" : 2,
 *       "letters" : "AAUBYCE"
 *     },
 *     "42" : {
 *       "score" : 5,
 *       "letters" : "OYVYU"
 *     },
 *     "cotcotcodète" : {
 *       "score" : 8,
 *       "letters" : "__UIOI"
 *     },
 *     "winteriscoming" :{
 *       "score" : 10,
 *       "letters" : "EEEE_"
 *     }
 *   },
 *   "map" : "test/parse_test.txt",
 *   "turn" : 1
 * }]
 *
 * Une grille de scrabble peut contenir :
 * - Des lettres majuscules (ce sont les lettres standard)
 * - Des lettres minuscules (ce sont les jokers qui ont été placés)
 * - Des espaces (ce sont des cases vides)
 *)

(** Player info: network player * name *)
type player_info = Info of bool * string

(**The bag of letters*)
val bag : Rules.bag
				   
(** Create a new game *)
val new_game : player_info array -> unit
				      
(** Load a game *)
val open_game : string -> unit

(**this function returns the letters used from the players game.
If there are no letters, then the move is not legal.*)
val is_legal : int -> int -> Rules.orientation -> string -> string

(** Add a word to the board and returns the number of points made*)
val add_word : int -> int -> Rules.orientation -> string -> int

(** name of the map *)
val get_name : unit -> string

(** array representing the board *)
val board : char array array

(** array containing all the players *)
val get_players : unit -> Player.player array

(** index of the player who is to play now *)
val get_turn : unit -> int

(** Tests functions *)
val tests : OUnit2.test list
