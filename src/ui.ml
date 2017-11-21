let convert_blanks c =
  if c = ' ' then
    '.'
  else
    c
(*Pretty printing of a board*)   
let pp_board f g = 
  if not (Array.length g = 0) then 
    begin
      Format.fprintf f "@[<v 2>";

      (*ligne du haut*)
      for _ = 1 to 2*Array.length g.(0) + 1 do
	Format.fprintf f "_";
      done;
      Format.fprintf f "@,";

      (*corps*)
      for i = 0 to Array.length g - 1 do 
	Format.fprintf f "|";
	for j = 0 to Array.length g.(i)-2 do 
	  Format.fprintf f "%c " (convert_blanks g.(i).(j))
	done;
	Format.fprintf f "%c" (convert_blanks
				 g.(i).(Array.length g.(i)-1));
	Format.fprintf f "|@," 
      done;

      (*ligne du bas*)
      for _ = 1 to 2*Array.length g.(0) + 1 do
	Format.fprintf f "‾";
      done; 
      
      Format.fprintf f "@,@]"
    end

(*Pretty printing of a player*)
let pp_player p b =
  Format.printf "@[<v 0>C'est au tour de %s de jouer.@,\
		 Votre jeu est : %s@,\
		 Votre score est : %d@, \
		 Voici l'état du jeu : @,"
		p#get_name p#get_letters p#get_score;
  pp_board Format.std_formatter b;
  Format.printf "@]"

let not_understood () =
  Format.printf "Je n'ai pas compris ce que vous venez de taper.@,"
		 
let rec ask_bool ()  =
  Format.printf "[O/o pour Oui, N/n pour Non]";
  match String.uppercase_ascii (read_line ()) with
  |"O" -> true
  |"N" -> false
  |_ -> not_understood ();
	ask_bool ()


  
let rec main_loop () =
  let ngu = ref 0 in (* ngu : not given up
		      * (number of players who haven't give up)*)
  let players = State.get_players () in 
  for i = 0 to Array.length players - 1 do
    if not players.(i)#given_up  then
      begin
	pp_player players.(i) State.board;
	let move = read_line () in 
	players.(i)#play move
      end
  done;
  if !ngu >= 2 then 
    main_loop()
  
