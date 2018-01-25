let convert_blanks c =
  if c = ' ' then
    '.'
  else
    c

(*Pretty printing of a board*)
let pp_board f g =
  if not (Array.length g = 0) then
    begin
      Format.fprintf f "@[<v 2>  ";

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
        Format.fprintf f "%c"
          (convert_blanks g.(i).(Array.length g.(i)-1));
        Format.fprintf f "|@,"
      done;

      (*ligne du bas*)
      for _ = 1 to 2*Array.length g.(0) + 1 do
        Format.fprintf f "‾";
      done;

      Format.fprintf f "@]"
    end

(*Pretty printing of a player*)
let pp_player p b =
  Format.printf "@[<v 0>C'est au tour de %s de jouer.@,\
                 Votre jeu est : %s@,\
                 Votre score est : %d@,\
                 Voici l'état du jeu : @,"
    p#get_name p#get_letters p#get_score;
  pp_board Format.std_formatter b;
  Format.printf "@]"

let not_understood () =
  Printf.printf "Je n'ai pas compris ce que vous venez de taper.\n"

let rec ask_bool ()  =
  Printf.printf "[O/N]: ";
  match String.uppercase_ascii (read_line ()) with
  |"O" -> true
  |"N" -> false
  |_ -> not_understood ();
       ask_bool ()

let  ask_string () =
  Printf.printf "[Entrez un mot]: ";
  read_line ()

let rec ask_int () =
  Printf.printf "[Entrez un nombre]: ";
  try
    int_of_string (read_line ())
  with
    _ -> not_understood ();
        ask_int ()

let ask_new_player i =
  Printf.printf "Joueur %d en réseau ?\n" i;
  let n = ask_bool () in
  if n then begin
    Printf.printf "Joueur local; quel est votre pseudo ?\n" i;
    Info(n, ask_string ()) end
  else
    Info(n, "")

let ask_new_game () =
  Printf.printf "Combien y a-t-il de joueurs ? (l'ordre des joueurs ne sera pas l'ordre de jeu)\n";
  let nb_players = ask_int () in
  let players = Array.make nb_players player_info in
  for i = 0 to nb_players - 1 do
    players.(i) <- ask_new_player i
  done;
  State.new_game players


(*************** Parsing of actions ***************************)
let print_action_doc () =
  Printf.printf "Vous pouvez entrer deux types d'action : \n";
  Printf.printf
    "- pour jouer un mot :\n\
     * ligne de la première lettre du mot (une lettre) \n\
     * colonne de la première lettre du mot (un nombre) \n\
     * vertical ou horizontal (V/H) \n\
     * insérer un espace \n\
     * entrez le mot en utilisant des lettres\
     majuscules pour les lettres standard et des \
     lettres minuscules pour les Jokers.\n\n\
     \
     Exemple : E10V BONjOUR\n\n\
     \
     - pour entrer une action spéciale :\n\
     * entrez # puis le nom de l'action spéciale \
     sans ajouter d'espace.\
     * voici les noms d'actions spéciales acceptés : \n\
     aide, piocher\n\n"

type word_pos = int*int*string
exception CantParse

let parse_orientation c =
  match c with
  |'v'|'V' -> Rules.V
  |'h'|'H' -> Rules.H
  |_ -> raise CantParse

let parse_word s =
  let line_char = Char.uppercase_ascii s.[0] in
  let line =
    if line_char >= 'A' && line_char <= 'O' then
      int_of_char line_char - int_of_char 'A'
    else
      raise CantParse
  in
  (*the number can be up to 15 so 1 or 2 characters*)
  let (num_length,orient) =
    if s.[3] = ' ' then
      (1,parse_orientation s.[2])
    else if s.[4] = ' ' then
      (2,parse_orientation s.[3])
    else
      raise CantParse
  in
  let col = int_of_string (String.sub s 1 num_length) in
  (line,col,orient,
   String.sub s (3 + num_length) (String.length s - 3 - num_length))


let rec ask_again player  =
  not_understood ();
  print_action_doc ();
  ask_action player

and ask_action player =
  Printf.printf "[Entrez une action] ";
  let s = read_line () in
  if String.length s = 0 then
    begin
      ask_again player
    end;

  if s.[0] = '#' then
    match String.sub s 1 (String.length s - 1 ) with
    |"aide" -> print_action_doc ();
              ask_action player
    |"piocher" -> ()
    |_ -> ask_again player
  else
    begin
    try
      let (l,c,o,w) = parse_word s in
      State.add_word l c o w;
      Printf.printf "%d %d %s" l c w
    with
    |_ -> ask_again player
    end

let rec main_loop () =
  let ngu = ref 0 in (* ngu : not given up (number of players who haven't give up)*)
  let players = State.get_players () in

  (*all players have to play...*)
  for i = 0 to Array.length players - 1 do
    if not players.(i)#given_up  then (*unless they've given up*)
      begin
        pp_player players.(i) State.board;
        Printf.printf "\n";
        ask_action players.(i)
      end
  done;
  if !ngu >= 2 then
    main_loop()
