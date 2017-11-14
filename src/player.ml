open OUnit2
       
class virtual player (a_name:string) (a_score:int) (a_letters:string)=
	object (self)
	  val name = a_name
	  val mutable score = a_score
	  val mutable letters = a_letters
      	  method virtual play : unit -> unit
	  method get_name = name
	  method get_letters = letters
	  method get_score = score
	  method print b =
	    Format.printf "@[<v 0>C'est au tour de %s de jouer.@,\
			   Votre jeu est : %s@,\
			   Votre score est : %d@, \
			   Voici l'état du jeu : @," name letters score;
	    Ui.pp_board Format.std_formatter b;
	    Format.printf "@]"
	 
	end

class humanPlayer (a_name:string) (a_score:int) (a_letters:string)  =
object (self)
  inherit player a_name a_score a_letters
  (*TODO*)
  method play () = ()
end	 
