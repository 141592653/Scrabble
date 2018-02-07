(** Contient l'état du jeu à un moment donné*)

open Unix
open OUnit2
open Yojson

type player_info = Info of bool * string

let name = ref ""
let players = ref [||]
let turn = ref 0
let bag = ref ""
let board = Array.make_matrix 15 15 ' '

let get_name () = !name
let get_players () = !players
let get_turn () = !turn

let state_pos_of_word_pos l c o i =
  let o' = Rules.int_of_orientation o in
  (l+i*o',c + i*(1-o'))


let whole_word l c o length=
  let begin_word = ref "" in
  let i = ref (-1) in
  (*moving backward*)
  while (let (l',c') = state_pos_of_word_pos l c o !i in
         l' >= 0 && c' >= 0 && board.(l').(c') <> ' ') do
    let (l',c') = state_pos_of_word_pos l c o !i in
    begin_word := (String.make 1 board.(l').(c')) ^ !begin_word;
    i := !i - 1
  done;
  let fst_pos = state_pos_of_word_pos l c o (!i + 1) in
  i:=length;

  let end_word = ref "" in
  (*moving forward*)
  while (let (l',c') = state_pos_of_word_pos l c o !i in
         l' < Array.length board && c' < Array.length board.(0) && board.(l').(c') <> ' ') do
    let (l',c') = state_pos_of_word_pos l c o !i in
    end_word :=  !end_word ^ (String.make 1 board.(l').(c'))  ;
    i := !i + 1
  done;
  (fst_pos,!begin_word,!end_word)

(* here we do not verify anything  because the verification should have been
 * using is_legal*)
let add_word l_arg c_arg o w_arg =
  let ((l,c),begin_w,end_w) = whole_word l_arg c_arg o (String.length w_arg) in
  let w = begin_w ^ w_arg ^ end_w in
  let score = ref 0 in
  let word_mul = ref 1 in
  let letter_used = ref 0 in
  for i = 0 to String.length w - 1 do
    let (l',c') = state_pos_of_word_pos l c o i in
    let val_letter = Rules.score_of_char w.[i] in

    if board.(l').(c') = ' ' then
      begin
	letter_used := !letter_used + 1;
        board.(l').(c') <- w.[i];
        begin
	  let (_,begin_cross,end_cross) =
	    whole_word l' c' (Rules.inv_orientation o) 1 in
	  let cross = begin_cross ^ (String.make 1 w.[i]) ^ end_cross in
	  let cross_length = String.length cross in
          match Rules.score_modifiers.(l').(c') with
          |Rules.NONE ->
	    score := !score + val_letter;
	    if cross_length > 1 then
	      score := !score + Rules.no_mul_score cross
          |Rules.MUL_LETTER n ->
	    score := !score + n*val_letter;
	    if cross_length > 1 then
	      score := !score + Rules.no_mul_score cross + (n-1)*val_letter
          |Rules.MUL_WORD n ->
            score := !score + val_letter;
            word_mul := !word_mul * n;
	    if cross_length > 1 then
	      score := !score + n * Rules.no_mul_score cross
        end
      end
    else
      score := !score + val_letter;
  done;
  if !letter_used = Rules.max_nb_letters then
    !score * !word_mul + 50 (* if it's a scrabble*)
  else
    !score * !word_mul


exception CantReplace

(*this function returns the letters used from the players game.
If there are no letters, then the move is not legal.*)
let is_legal l_arg c_arg o w_arg =
  let ((l,c),begin_w,end_w) = whole_word l_arg c_arg o (String.length w_arg) in

  let w = begin_w ^ w_arg ^ end_w in
  (* Printf.printf "begin:%s|end:%s|w:%s" begin_w end_w w; *)
  let seen_middle = ref false in
  (* whether the word is connected to the main component *)
  let connected = ref false in
  let used_letters = ref "" in
  try
    let upper_w = String.uppercase_ascii w in
    (*if the word is not found raises an exception wich will be caught later*)
    if not (Array.exists (fun w_dict -> upper_w = w_dict) Rules.dictionary) then
      failwith ("Le mot "^w^" n'est pas dans l'officiel du scrabble.");

    let seen_middle = ref false in
    (*whether the word is connected to the main component*)
    let connected = ref false in
    let used_letters = ref "" in
    for i = 0 to String.length w - 1 do
      let (l',c') = state_pos_of_word_pos l c o i in
      if board.(l').(c') = ' ' then
        used_letters := !used_letters ^ (String.make 1 w.[i])
      else if board.(l').(c') <> w.[i] then
        raise CantReplace
      else
	connected := true;

      (*if we write next to an already there word*)
      let (cross,begin_cross,end_cross) =
	whole_word l' c' (Rules.inv_orientation o) 1 in
      if String.length begin_cross + String.length end_cross > 0 then
	connected := true;
      let upper_ww = String.uppercase_ascii
		       (begin_cross ^
			  (String.make 1 board.(l').(c') ) ^ end_cross) in
      if not (Array.exists (fun w_dict -> upper_ww = w_dict) Rules.dictionary) then
	failwith ("Le mot "^w^" n'est pas dans l'officiel du scrabble.");

      if (l',c') = (7,7) then
        seen_middle := true
    done;
    if (!connected && board.(7).(7) <> ' ') ||
	 !seen_middle then
      (*if this is not the first move or (7,7) was seen*)
      !used_letters
    else
      ""

  with
  |Failure s -> Format.fprintf Format.str_formatter "%s" s; ""
  | _ -> ""

(* ********************** Json parsing **************************** *)
let is_valid_letter c =
  c = '_' || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')

(*Récupère le nom, le score et les lettres du joueur p *)
let parse_player p =
  let (name, score_letters) = p in
  match score_letters with
  | `Assoc l -> begin
      match l with
      | [("score", `Int score);("letters",`String s_a)] ->
         let s = String.uppercase_ascii s_a in
         String.iter (fun c -> if is_valid_letter c then ()
                            else failwith "Une lettre d'un jeu d'un joueur \
                                           est mal renseignée") s;
         if String.length s <= Rules.max_nb_letters then
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
    failwith ("The file is not a json file. \
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


let bag = new Rules.bag Rules.fr_distrib

let empty_board () =
  for i = 0 to Array.length board - 1 do
    board.(i) <- Array.make 15 ' '
  done

let new_game infos =
  (* Don't initialize network if all players are locals *)
  let network = not (Array.for_all (fun a -> match a with | Info(b, _) -> not b) infos) in
  let serv_sock = if network then socket PF_INET SOCK_STREAM 0 else stderr in
  if network then begin
      setsockopt serv_sock SO_REUSEADDR true;
      bind serv_sock (ADDR_INET(inet_addr_any, Rules.server_port));
      listen serv_sock 1024
  end;

  turn := 0;
  Random.self_init ();
  let tmp = Array.map (fun c -> (Random.bits (), c)) infos in
  let nb_names = Array.length infos in
  players := Array.make nb_names default_player;
  for i = 0 to nb_names - 1 do
    match infos.(i) with
    | Info(true, name) -> (* Network  player *)
       (!players).(i) <- new Player.networkPlayer name 0
			     (bag#pick_letters Rules.max_nb_letters) serv_sock
    | Info(false, name) -> (* Local player *)
       (!players).(i) <- new Player.humanPlayer name 0
			     (bag#pick_letters Rules.max_nb_letters)
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
