(*converts spaces into .*)
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

      (*top line*)
      for _ = 1 to 2*Array.length g.(0) + 1 do
        Format.fprintf f "_";
      done;
      Format.fprintf f "@,";

      (*body*)
      for i = 0 to Array.length g - 1 do
        Format.fprintf f "|";
        for j = 0 to Array.length g.(i)-2 do
          Format.fprintf f "%c " (convert_blanks g.(i).(j))
        done;
        Format.fprintf f "%c"
          (convert_blanks g.(i).(Array.length g.(i)-1));
        Format.fprintf f "|@,"
      done;

      (*bottom line*)
      for _ = 1 to 2*Array.length g.(0) + 1 do
        Format.fprintf f "‾";
      done;

      Format.fprintf f "@]"
    end



let not_understood () =
  Printf.printf "Je n'ai pas compris ce que vous venez de taper.\n"

let rec ask_bool ()  =
  Printf.printf "[O/o pour Oui, N/n pour Non] ";
  match String.uppercase_ascii (read_line ()) with
  |"O" -> true
  |"N" -> false
  |_ -> not_understood ();
       ask_bool ()

let  ask_string () =
  Printf.printf "[Entrez un mot] ";
  read_line () 

let rec ask_int () =
  Printf.printf "[Entrez un nombre] ";
  try
    int_of_string (read_line ())
  with
    _ -> not_understood ();
         ask_int ()


let print_action_doc () =
  Printf.printf "Vous pouvez entrer deux types d'action : \n";
  Printf.printf " - pour jouer un mot :\n\
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
