let time_limit = ref 100
		
let main_func file =
  Format.fprintf Format.std_formatter
		 "@[<v 0>*********************** Bienvenue dans Scrabble\
		  **************************** @,@]"
  

let main() = 
  Arg.parse [("-t",Arg.Int (fun n -> time_limit := n),
	      "Permet de régler la durée maximum de réflexion de \
	       l'IA en millisecondes.Par défaut : 100.")]
	    main_func "Ce programme permet de jouer au scrabble"

let () = main()
