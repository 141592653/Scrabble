(*Pretty printing of a player*)
let pp_player f p b =
  Format.fprintf f "@[<v 0>C'est au tour de %s de jouer.@,\
                    Votre jeu est : %s@,\
                    Votre score est : %d@,\
                    Voici l'état du jeu : @,@,"
    p#get_name p#get_letters p#get_score;
  Misc.pp_board f b;
  Format.fprintf f "@,@,@]"

let ask_new_player i =
  Printf.printf "Joueur %d en réseau ?\n" i;
  let n = Misc.ask_bool Format.std_formatter in
  if not n then begin
      Printf.printf "Joueur local; quel est votre pseudo ?\n";
      State.Info(n, Misc.ask_string ()) end
  else
    State.Info(n, "")

let ask_new_game () =
  Printf.printf "Combien y a-t-il de joueurs ? (l'ordre des joueurs ne sera pas l'ordre de jeu)\n";
  let nb_players = Misc.ask_int Format.std_formatter in
  let player_infos = Array.make nb_players (State.Info(false, "")) in
  for i = 0 to nb_players - 1 do
    player_infos.(i) <- ask_new_player i
  done;
  State.new_game player_infos



let rec main_loop () =
  let game_finished = ref true in
  let players = State.get_players () in

  (* all players have to play... *)
  for i = 0 to Array.length players - 1 do
    if not players.(i)#given_up then (* unless they've given up *)
      begin
        game_finished := false;

        pp_player Format.str_formatter players.(i) State.board;
        players.(i)#send_game (Format.flush_str_formatter ());

        Printf.printf "\n\n";

        let a = ref (players.(i)#ask_action ()) in
        while (match !a with
               |Action.HELP -> Misc.print_action_doc ();true
               |Action.WORD(l,c,o,w) ->
                 let letters_played =  State.is_legal l c o w in
                 if letters_played = "" then
                   begin
                     Printf.printf
                       "Le mot que vous avez joué ne respecte pas les \
                        règles du jeu.\n";
                     true
                   end
                 else if not (players.(i)#can_play letters_played) then
                   begin
                     Printf.printf
                       "Vous avez joué des lettres qui ne sont pas dans \
                        votre jeu\n";
                     true
                   end
                 else
                   false

               |_ -> false)

        do
          a :=  players.(i)#ask_action ()
        done;

        match !a with
        |Action.PICK -> ()
        |Action.WORD(l,c,o,w) ->
          let score = State.add_word l c o w in
          players.(i)#add_to_score score
      end
  done;
  if not !game_finished then
    main_loop()

open Unix

let rec main_loop_network () =
  (* Setup client socket *)
  let sock = socket PF_INET SOCK_STREAM 0 in
  Printf.printf "Entrez l'addresse du serveur: \n";
  let addr_str = Misc.ask_string () in
  let s_addr = List.hd (getaddrinfo addr_str "" [AI_FAMILY(PF_INET)]) in
  (match s_addr.ai_addr with
   | ADDR_INET(addr, _) -> connect sock (ADDR_INET(addr, Rules.server_port))
   | _ ->  failwith "Failed to connect to server");
  (* Connected *)
  Printf.printf "Connecté au serveur !\nEntrez votre pseudo:\n%!";
  let name = Misc.ask_string () in
  let rc = send_substring sock name 0 (min 50 (String.length name)) [] in
  if rc <= 0 then failwith "Failed to send name to server!";

  let buffer = Bytes.create 4096 in
  let rec loop () =
    let n = recv sock buffer 0 4096 [] in
    let str = Bytes.sub_string buffer 0 n in

    if Misc.contains str "\nà vous de jouer !\n" then begin
        Printf.printf "[Entrez une action] ";
        let rec aux () =
          let answer = read_line () in if (String.length answer) = 0 then aux () else answer
        in let answer = aux () in
        let _ = send_substring sock answer 0 (String.length answer) [] in ()
      end;
    Printf.printf "%s\n%!" str;
    loop ()

  in loop ()
