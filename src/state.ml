(** Contient l'état du jeu à un moment donné*)

open OUnit2
open Yojson

let name = ref ""
let players = ref [||]
let turn = ref 0
let board = Array.make_matrix 15 15 ' '

let get_name () = !name
let get_players () = !players
let get_turn () = !turn

(* ********************** Début parsing **************************** *)

let parse_pos p = match p with
    |`Assoc l -> begin
		match l with
		|[("l", `Int line);("c",`Int column)]
		 when line <= 14 && column <= 14 -> (line,column)
		|_ -> failwith "Une position est mal renseignée"
    end
   |_-> failwith "Une position est mal renseignée"

let parse_pos_test _ =
  assert_equal (parse_pos (`Assoc [("l",`Int 2);("c",`Int 4)])) (2,4)

let parse_player p =
  let (name,pos) = p in
  (name,parse_pos pos)

(*parse players ;D *)
let parse_players p =
  match p with
  | `Assoc l -> List.map parse_player l
  | _ -> failwith ("The player structure  should be an Associative list")
		 

(*parse name players map or turn*)
let parse_npmt  players_tmp map_file npmt =
  match fst npmt with
  |"name" -> begin
	     match snd npmt with
	       |`String s -> name := s
              |_ -> failwith "The name of a map should be a string"
	   end
  |"map" -> begin
	    match snd npmt with
	      |`String s -> map_file := s
             |_ -> failwith "The name of the file containing the map \
			     should be a string"
	  end
  |"turn" -> begin
	     match snd npmt with
	       |`Int i -> turn := i
              |_ -> failwith "The name of the file containing the map \
			      should be a string"
	   end
  |"players" -> players_tmp := parse_players (snd npmt)
  |_ -> failwith ("The entry "^(fst npmt)^ " is not understood")

		 

let tests = ["parse position">::parse_pos_test]
  
  
  
