(** Contient l'état du jeu à un moment donné*)

open OUnit2
open Yojson
open Player

let max_nb_letters = 7

let name = ref ""
let players = ref [||]
let turn = ref 0
let board = Array.make_matrix 15 15 ' '

let get_name () = !name
let get_players () = !players
let get_turn () = !turn

(* ********************** Json parsing **************************** *)
let is_valid_letter c =
  let n = int_of_char c in 
  n = 95 || (n>=65 && n<=90) || (n>=97 && n<=122)
   

(*Récupère le nom, le score et les lettres du joueur p *)
let parse_player p =
  let (name,score_letters) = p in
  match score_letters with
      |`Assoc l -> begin
		  match l with
		  |[("score", `Int score);("letters",`String s_a)] ->
		    let s = String.uppercase_ascii s_a in 
		    String.iter (fun c -> if is_valid_letter c
					  then ()
					else
					  failwith "Une lettre d'un jeu \
						    d'un joueur est mal \
						    renseignée"
				) s;
		    if String.length s <= max_nb_letters then
		      new humanPlayer name score s
		    else
		      failwith "Un joueur a un jeu de plus de 8 lettres"       
		  |_ -> failwith "Un joueur est mal renseigné"
		end
    |_-> failwith "Une position est mal renseignée"
  
  
      
(*parse players ;D *)
let parse_players p =
  match p with
  | `Assoc l -> List.map parse_player l
  | _ -> failwith ("The player structure should be an Associative list")
		 

(*parse name players map or turn*)
let parse_npmt  players_tmp map_file npmt =
  match fst npmt with
  |"name" -> begin
	     match snd npmt with
	       |`String s -> name := s;
			     if !map_file = "" then
			       map_file := s^".txt"
				       
              |_ -> failwith "The name of a map should be a string"
	   end
  |"map" -> begin
	    match snd npmt with
	      |`String s -> map_file := s;
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

(*This is the parser of a json file*)
let parse_main_json json_a = 
  let players_tmp = ref [] and map_file = ref "" in
  begin
    match json_a with
    | `Assoc l -> List.iter (parse_npmt players_tmp map_file) l
    | _ -> failwith ("The main structure of should be an Associative list")
  end;
  (!players_tmp,!map_file)

(*joueur temporaire*)
let default_player = new Player.humanPlayer "" 0 ""

let json_from_file s =
  try
    Yojson.Basic.from_channel (open_in s)
  with
  |Json_error log ->
    failwith ("The file is not a json file.\
		Here is the log of the json parser : "
	      ^log)
			 
let open_game json_file =
  let (ps,b) = parse_main_json (json_from_file json_file) in
  players := Array.make (List.length ps) default_player;
  List.iteri (fun i p -> !players.(i)<-p) ps
  
    
let json_parsing_test _ =
  open_game "test/parse_test.json";
  assert_equal (get_turn ()) 1;
  assert_equal !players.(0)#get_name  "beauGosseDu84";
  assert_equal !players.(0)#get_letters  "AAUBYCE";
  assert_equal !players.(1)#get_score   5

(* *********************** Fin Json parsing *************************** *)

(* *********************** Parsing du plateau ************ *)
let board_line_of_string s =
  if String.length s <= 15 then
    begin
      let bl = Array.make 15 ' ' in (*for board line*)
      String.iteri (fun i c ->
		    if is_valid_letter c || int_of_char c = 32 then
		      bl.(i) <- c
		    else
		      failwith "Unknown character."		      
		   ) s;
      bl
    end
  else
    failwith "A line has to many characters."


let blos_test _ =
  assert_equal (board_line_of_string "aa B") [|'a';'a';' ';'B';' ';' ';' ';' ';
					     ' ';' ';' ';' ';' ';' ';' '|]

					
  
  

let tests = ["json parsing" >:: json_parsing_test;
	    "board line of string" >:: blos_test]
  
  
  
