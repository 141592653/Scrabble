(*converts spaces into .*)
let pp_char_on_board f c i j =
  match Rules.score_modifiers.(i).(j) with
  |Rules.NONE ->
    if c = ' ' then
      Format.fprintf f "."
    else
      Format.fprintf f "%c" c
  |Rules.MUL_LETTER i ->
    if i <= 2 && c = ' 'then
      Format.fprintf f "\027[104m"
    else if c = ' ' then
      Format.fprintf f "\027[44m";
    if c = ' ' then
      Format.fprintf f "%d" i
    else
      Format.fprintf f "%c" c;
    Format.fprintf f "\027[0m";
  |Rules.MUL_WORD i ->
    if i <= 2 && c = ' 'then
      Format.fprintf f "\027[45m"
    else if c = ' ' then
      Format.fprintf f "\027[41m";
    if c = ' ' then
      Format.fprintf f "%d" i
    else
      Format.fprintf f "%c" c;
    Format.fprintf f "\027[0m"

(*prints 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5*)
let line_of_numbers f g =
  Format.fprintf f "  ";
  for i = 1 to min 9 (Array.length g.(0)) do
    Format.fprintf f "%d " i
  done;

  for i = 10 to Array.length g.(0) do
    Format.fprintf f "%d " (i-10)
  done;
  Format.fprintf f "@,"

(*Pretty printing of a board , f is the formatter, g the grid*)
let pp_board f g =
  if not (Array.length g = 0) then
    begin
      Format.fprintf f "@[<v 2>  ";
      line_of_numbers f g;

      (*top line*)
      Format.fprintf f " ";
      for _ = 1 to 2*Array.length g.(0) + 1 do
        Format.fprintf f "_";
      done;
      Format.fprintf f "@,";

      (*body*)
      for i = 0 to Array.length g - 1 do
        let line_letter = char_of_int (i + int_of_char 'A') in
        Format.fprintf f "%c|" line_letter;
        for j = 0 to Array.length g.(i)-2 do
          pp_char_on_board f g.(i).(j) i j;
          Format.fprintf f " "
        done;
        let last_column = Array.length g.(i)-1 in
        pp_char_on_board f g.(i).(last_column) i last_column;
        Format.fprintf f "|%c@," line_letter
      done;

      (*bottom line*)
      Format.fprintf f " ";
      for _ = 1 to 2*Array.length g.(0) + 1 do
        Format.fprintf f "‾";
      done;
      Format.fprintf f "@,";
      line_of_numbers f g;

      Format.fprintf f "@]"
    end

let not_understood f =
  Format.fprintf f "Je n'ai pas compris ce que vous venez de taper.\n"

let rec ask_bool f  =
  Printf.printf "[O/o pour Oui, N/n pour Non] ";
  match String.uppercase_ascii (read_line ()) with
  |"O" -> true
  |"N" -> false
  |_ -> not_understood f;
       ask_bool f

let  ask_string f =
  Printf.printf "[Entrez un mot] ";
  read_line ()

let rec ask_int f =
  Printf.printf "[Entrez un nombre] ";
  try
    int_of_string (read_line ())
  with
    _ -> not_understood f;
         ask_int f

let print_action_doc f =
  Format.fprintf f "Vous pouvez entrer deux types d'action : \n";
  Format.fprintf f
    " - pour jouer un mot :\n\
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

let contains s1 s2 =
  let re = Str.regexp_string s2 in
  try ignore (Str.search_forward re s1 0); true
  with Not_found -> false


let string_to_list s =
  let l = ref [] in
  for i =  String.length s - 1 downto 0 do
    l := s.[i] :: !l
  done;
  !l


(*deletes a character in a string*)
let delete_char s c =
  let i = if c >='A'&& c <= 'Z' then
	    String.index s c
	  else if c >='a'&& c <= 'z' then
	    String.index s '_'
	  else
	    failwith "Unknown character played"
  in
  (String.sub s 0 i)^
    (String.sub s (i+1) (String.length s - i - 1)) 
