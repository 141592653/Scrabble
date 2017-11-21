let time_limit = ref 100
		
let main_func file =
  Printf.printf "*********************** Bienvenue dans Scrabble x\
		  **************************** \n";
   Printf.printf "Voulez vous créer une nouvelle partie ? \n";
   if Ui.ask_bool () then
     Ui.ask_new_game ()
   else
     Printf.printf "oooh"

let main() = 
  Arg.parse
    [("-t",Arg.Int (fun n -> time_limit := n),
      "Permet de régler la durée maximum de réflexion de \
       l'IA en millisecondes.Par défaut : 100.")]
    main_func "Ce programme permet de jouer au scrabble"

let () = main()


	    
