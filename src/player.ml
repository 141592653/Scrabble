open Bytes
open Unix
open OUnit2

class virtual player (a_name:string) (a_score:int) (a_letters:string) =
  object (self)
    val name = a_name
    val mutable score = a_score
    val mutable letters = a_letters
    val mutable give_up = false
    method virtual play : string -> unit
    method virtual ask_action : unit -> Action.action
    (*method virtual is_human : bool*)
    method get_name = name
    method get_letters = letters
    method get_score = score
    method given_up = give_up
    method pick s =
      if String.length s + String.length letters >
           Rules.max_nb_letters then
        failwith "A player had more letters than he is allowed."
      else
        letters <- letters ^ s

    method letters_missing  =
      Rules.max_nb_letters - String.length letters
  end

class humanPlayer (a_name:string) (a_score:int) (a_letters:string) =
  object (self)
    inherit player a_name a_score a_letters

		   
    method ask_action () =
      Printf.printf "[Entrez une action] ";
      let oa = Action.parse_action (read_line ()) in
      match oa with (*option action*)
      |None -> Misc.not_understood ();
	       Misc.print_action_doc ();
	       self#ask_action ()
      |Some a -> a
     

	  
    (*method is_human = true*)
    method play s = print_string s
  end

class networkPlayer (a_name:string) (a_score:int) (a_letters:string) (serv_sock:file_descr) =
  let () = Printf.printf "En attente de connexion d'un joueur..." in
  let (s, addr) = accept serv_sock in
  let name_bytes = create 50 in
  let name_len = recv s name_bytes 0 50  [] in
  let name = sub_string name_bytes 0 name_len in
  object (self)
    inherit player name a_score a_letters
    val mutable sock = s
    method is_human = true
    method play s = print_string s
  end
