let time_limit = ref 100
let filename = ref ""

let main_func file =
  Printf.printf "*********************** Bienvenue dans Scrabble \
                 **************************** \n";
  Printf.printf "Voulez vous créer une nouvelle partie ?\n";
  if Misc.ask_bool Format.std_formatter then begin
      Ui.ask_new_game ();
      Ui.main_loop ()
  end else begin
      Printf.printf "Voulez vous rejoindre une partie en ligne ?\n";
      if Misc.ask_bool Format.std_formatter then begin
          Ui.main_loop_network ()
      end else begin
          Printf.printf "Ooooh ='(\n";
       end;
  end


let main () =
  Arg.parse
    [("-t", Arg.Set_int time_limit,
      "Permet de régler la durée maximum de réflexion de \
       l'IA en millisecondes.Par défaut : 100.")]
    (fun str -> filename := str) "Ce programme permet de jouer au scrabble";
  main_func !filename

let () = main ()
