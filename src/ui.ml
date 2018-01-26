

(*Pretty printing of a player*)
let pp_player p b =
  Format.printf "@[<v 0>C'est au tour de %s de jouer.@,\
                 Votre jeu est : %s@,\
                 Votre score est : %d@,\
                 Voici l'état du jeu : @,@,"
    p#get_name p#get_letters p#get_score;
  Misc.pp_board Format.std_formatter b;
  Format.printf "@,@,@]"



let ask_new_player i =
  Printf.printf "Joueur %d, quel est votre pseudo ?\n" i;
  Misc.ask_string ()

let ask_new_game () =
  Printf.printf "Combien y a-t-il de joueur ? \
                 (l'ordre des joueurs ne sera pas l'ordre de jeu)) \n";
  let nb_players = Misc.ask_int () in
  let player_names = Array.make nb_players "" in
  for i = 0 to nb_players - 1 do
    player_names.(i) <- ask_new_player i
  done;
  State.new_game player_names


		 


	     
let rec main_loop () =
  let game_finished = ref true in
  let players = State.get_players () in

  (*all players have to play...*)
  for i = 0 to Array.length players - 1 do
    if not players.(i)#given_up  then (*unless they've given up*)
      begin
        game_finished := false;
	
        pp_player players.(i) State.board;
	Printf.printf "\n\n";
	
	let a = ref (players.(i)#ask_action ()) in
	while (match !a with
	       |Action.HELP -> Misc.print_action_doc ();true
	       |Action.WORD(l,c,o,w) ->
		 let letters_played =  State.is_legal l c o w in 
		 if letters_played = "" then
		   begin
		     Printf.printf
		       "Le mot que vous avez joué ne respecte pas les \
			règles du jeu.\n";
		     true		     
		   end
		 else if not (players.(i)#can_play letters_played) then
		   begin
		     Printf.printf
		       "Vous avez joué des lettres qui ne sont pas dans \
			votre jeu\n";
		     true		     
		   end
		 else
		   false
		 
	       |_ -> false)

	do
	  a :=  players.(i)#ask_action ()
	done;
	
	match !a with
	|Action.PICK -> ()
	|Action.WORD(l,c,o,w) ->
	  let score = State.add_word l c o w in
	  players.(i)#add_to_score score
      end
  done;
  if not !game_finished then
    main_loop()
