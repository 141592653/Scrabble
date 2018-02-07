let time_limit = ref 100
let filename = ref ""

let main_func file =
  Printf.printf "*********************** Bienvenue dans Scrabble \
                 **************************** \n";
  Printf.printf "Voulez vous créer une nouvelle partie ?\n";
  if Misc.ask_bool Format.std_formatter then
    begin
      Ui.ask_new_game ();
      Ui.main_loop ()
    end
  else
    begin
      Printf.printf "Voulez vous rejoindre une partie en ligne ?\n";
      if Misc.ask_bool Format.std_formatter then
          Ui.main_loop_network ()
      else 
        begin
	  Printf.printf "Entrez le chemin relatif menant au \
			 fichier que vous voulez charger : \n";
	  let found_file = ref false in
	  while not !found_file do
	    found_file :=
	      try
		State.open_game (Misc.ask_string ());
		true
	      with
	      |Failure s | Sys_error s -> Printf.printf "%s\n" s; false
	  done;
	  Ui.main_loop ()
	end  
    end
  


let main () =
  Arg.parse
    [("-t", Arg.Set_int time_limit,
      "Permet de régler la durée maximum de réflexion de \
       l'IA en millisecondes.Par défaut : 100.")]
    (fun str -> filename := str) "Ce programme permet de jouer au scrabble";
  main_func !filename

let () = main ()
