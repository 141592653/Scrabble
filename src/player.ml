open Bytes
open Unix

class virtual player (a_name:string) (a_score:int) (a_letters:string) =
  object
    val name = a_name
    val mutable score = a_score
    val mutable letters = a_letters
    val mutable given_up = false
    method virtual play : string -> unit
    method virtual ask_action : unit -> Action.action
    method virtual network_player : bool
    method virtual send_game : string -> unit
    method get_name = name
    method get_letters = letters
    method get_score = score
    method add_to_score n = score <- score + n
    method given_up = given_up
    method give_up () = given_up <- true
    method pick s =
      if String.length s + String.length letters >
           Rules.max_nb_letters then
        failwith "A player had more letters than he is allowed."
      else
        letters <- letters ^ s
    method letters_missing  = Rules.max_nb_letters - String.length letters
  end

class humanPlayer (a_name:string) (a_score:int) (a_letters:string) =
  object (self)
    inherit player a_name a_score a_letters

    method ask_action () =
      Printf.printf "[Entrez une action] ";
      let oa = Action.parse_action (read_line ()) in
      match oa with (*option action*)
      |None -> Misc.not_understood Format.std_formatter;
              Misc.print_action_doc Format.std_formatter;
              self#ask_action ()
      |Some a -> a

    method play s = print_string s
    method send_game str = Format.fprintf Format.std_formatter "%s\n\n" str
    method network_player = false
  end

class networkPlayer (a_name:string) (a_score:int) (a_letters:string) (serv_sock:file_descr) =
  let () = Printf.printf "En attente de connexion d'un joueur...\n%!" in
  let (s, address) = accept serv_sock in
  let _ = match address with
    | ADDR_INET(addr, port) ->
       Printf.printf "Joueur connecté depuis %s sur le port %d\n%!" (string_of_inet_addr addr) port
    | _ -> ()
  in
  let name_bytes = create 50 in
  let name_len = recv s name_bytes 0 50 [] in
  let name = sub_string name_bytes 0 name_len in
  object (self)
    inherit player name a_score a_letters
    val mutable sock = s
    method play str = print_string str

    method ask_action () =
      let your_turn_msg = "\nà vous de jouer !\n" in
      let rc = send_substring sock your_turn_msg 0 (String.length your_turn_msg) [] in
      if rc <= 0 then begin
          Printf.printf "Le joueur %s s'est déconnecté\n%!" name;
          given_up <- true;
          Action.GIVE_UP
        end
      else begin
          Printf.printf "En attente du joueur %s...\n%!" name;
          let play_bytes = create 100 in
          let size = recv sock play_bytes 0 100 [] in

          let action = Action.parse_action (sub_string play_bytes 0 size) in
          match action with
          |None -> Misc.not_understood Format.str_formatter;
                  Misc.print_action_doc Format.str_formatter;
                  let str = Format.flush_str_formatter () in
                  let _ = send_substring sock str 0 (String.length str) [] in
                  self#ask_action ()
          |Some action -> action

        end

    method send_game str =
      let rc = send_substring sock str 0 (String.length str) [] in
      if rc != (String.length str) then failwith "Failed to send data to player";
      Unix.sleepf 0.5

    method network_player = true
  end
