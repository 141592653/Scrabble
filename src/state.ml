(** Contient l'état du jeu à un moment donné*)

open OUnit2
open Yojson

let max_nb_letters = 7

let name = ref ""
let players = ref [||]
let turn = ref 0
let bag = ref ""
let board = Array.make_matrix 15 15 ' '

let get_name () = !name
let get_players () = !players
let get_turn () = !turn

(* ********************** Json parsing **************************** *)
let is_valid_letter c =
  c = '_' || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')

(*Récupère le nom, le score et les lettres du joueur p *)
let parse_player p =
  let (name,score_letters) = p in
  match score_letters with
  | `Assoc l -> begin
      match l with
      | [("score", `Int score);("letters",`String s_a)] ->
         let s = String.uppercase_ascii s_a in
         String.iter (fun c -> if is_valid_letter c then ()
                            else failwith "Une lettre d'un jeu d'un joueur \
                                           est mal renseignée") s;
         if String.length s <= max_nb_letters then
           new Player.humanPlayer name score s
         else
           failwith "Un joueur a un jeu de plus de 8 lettres"
      | _ -> failwith "Un joueur est mal renseigné"
    end
  | _-> failwith "Une position est mal renseignée"

(*parse players ;D *)
let parse_players p =
  match p with
  | `Assoc l -> List.map parse_player l
  | _ -> failwith ("The player structure should be an Associative list")

(*parse name players map or turn*)
let parse_npmt  players_tmp map_file npmt =
  match fst npmt with
  | "name" -> begin
      match snd npmt with
      | `String s -> name := s;
                    if !map_file = "" then
                      map_file := s^".txt"
      | _ -> failwith "The name of a map should be a string"
    end
  | "map" -> begin
      match snd npmt with
      | `String s -> map_file := s;
      | _ -> failwith "The name of the file containing the map \
                      should be a string"
    end
  | "turn" -> begin
      match snd npmt with
      | `Int i -> turn := i
      | _ -> failwith "The name of the file containing the map \
                      should be a string"
    end
  | "players" -> players_tmp := parse_players (snd npmt)
  | "bag" -> begin
      match snd npmt with
      | `String s -> bag := s;
      | _ -> failwith "The bag must be a string"
    end
  | _ -> failwith ("The entry "^(fst npmt)^ " is not understood")

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
               Here is the log of the json parser : " ^ log)

let board_line_of_string s =
  if String.length s <= 15 then
    begin
      let bl = Array.make 15 ' ' in (*for board line*)
      String.iteri (fun i c ->
          if is_valid_letter c || c = ' ' then bl.(i) <- c
          else failwith "Unknown character.") s;
      bl
    end
  else
    failwith "A line has to many characters."


let blos_test _ =
  assert_equal (board_line_of_string "aa B")
    [|'a'; 'a'; ' '; 'B'; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '|]

let open_board file_name =
  let in_chan = open_in file_name in
  try
    let i = ref 0 in
    while true do
      board.(!i)<-board_line_of_string (input_line in_chan);
      i := !i + 1
    done
  with
  |End_of_file -> ()
  |Invalid_argument _ -> failwith "Le plateau contient trop de lignes."

let open_game json_file =
  let (ps,b) = parse_main_json (json_from_file json_file) in
  players := Array.make (List.length ps) default_player;
  List.iteri (fun i p -> !players.(i)<-p) ps;
  open_board b

(* *********************** End parsing ********************************* *)


(* *********************** Bag gestion ********************************* *)
(* creation of the bag*)
let distrib = [('_',2);('A',9);('B',2);('C',2);('D',3);('E',15);('F',2);
               ('G',2);('H',2);('I',8);('J',1);('K',1);('L',5);('M',3);
               ('N',6);('O',6);('P',2);('Q',1);('R',6);('S',6);('T',6);
               ('U',6);('V',2);('W',1);('X',1);('Y',1);('Z',1)]

let bag_list = ref []

let add_letter c =
  bag_list := !bag_list @ Array.to_list (Array.make (snd c) (fst c))

(*creation of a new random bag*)
let new_bag () =
  List.iter add_letter distrib;
  Random.self_init ();
  let tmp = List.map (fun c -> (Random.bits (), c)) !bag_list in
  let sorted = List.sort compare tmp in
  bag_list := List.map snd sorted

(*Piocher n lettres*)
let pick_letters n =
  try (*si on a assez de lettres*)
    let ret = String.sub !bag 0 n in
    bag := String.sub !bag n (String.length !bag - n);
    ret
  with (*sinon*)
    _ -> let ret = !bag in
        bag := "";
        ret

let empty_board () =
  for i = 0 to Array.length board - 1 do
    board.(i) <- Array.make 15 ' '
  done

let new_game names =
  turn := 0;
  new_bag ();
  Random.self_init ();
  let tmp = Array.map (fun c -> (Random.bits (), c)) names in
  let nb_names = Array.length names in
  players := Array.make nb_names default_player;
  for i = 0 to - 1 do
    (!players).(i) <- new Player.humanPlayer
                       names.(i) 0 (pick_letters max_nb_letters)
  done;
  empty_board ()

let json_parsing_test _ =
  open_game "test/parse_test.json";
  assert_equal (get_turn ()) 1;
  assert_equal !players.(0)#get_name  "beauGosseDu84";
  assert_equal !players.(0)#get_letters  "AAUBYCE";
  assert_equal !players.(1)#get_score   5;
  assert_equal board.(14)
    [|'a'; 'a'; ' '; 'B'; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '; ' '|]

let tests = ["json parsing" >:: json_parsing_test;
             "board line of string" >:: blos_test]
