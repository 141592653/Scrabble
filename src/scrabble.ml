let time_limit = ref 100
let filename = ref ""

let main_func file =
  Printf.printf "*********************** Bienvenue dans Scrabble x\
                 **************************** \n";
   Printf.printf "Voulez vous créer une nouvelle partie ? \n";
   if Ui.ask_bool () then
     Ui.ask_new_game ()
   else
     Printf.printf "oooh";
   Ui.main_loop ()

let main() =
  Arg.parse
    [("-t", Arg.Set_int time_limit,
      "Permet de régler la durée maximum de réflexion de \
       l'IA en millisecondes.Par défaut : 100.")]
    (fun str -> filename := str) "Ce programme permet de jouer au scrabble";
  main_func !filename

let () = main()
