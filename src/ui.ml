

(*Pretty printing of a player*)
let pp_player p b =
  Format.printf "@[<v 0>C'est au tour de %s de jouer.@,\
                 Votre jeu est : %s@,\
                 Votre score est : %d@,\
                 Voici l'état du jeu : @,"
    p#get_name p#get_letters p#get_score;
  Misc.pp_board Format.std_formatter b;
  Format.printf "@]"



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
  let ngu = ref 0 in (* ngu : not given up
                      * (number of players who haven't give up)*)
  let players = State.get_players () in

  (*all players have to play...*)
  for i = 0 to Array.length players - 1 do
    if not players.(i)#given_up  then (*unless they've given up*)
      begin
        pp_player players.(i) State.board;
	Printf.printf "\n";
        ignore (players.(i)#ask_action ())
      end
  done;
  if !ngu >= 2 then
    main_loop()
