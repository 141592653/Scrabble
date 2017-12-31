open OUnit2

class virtual player (a_name:string) (a_score:int) (a_letters:string) =
  object (self)
    val name = a_name
    val mutable score = a_score
    val mutable letters = a_letters
    val mutable give_up = false
    method virtual play : string -> unit
    method virtual is_human : bool
    method get_name = name
    method get_letters = letters
    method get_score = score
    method given_up = give_up
    method pick s =
      if String.length s + String.length letters >
	   Bag.max_nb_letters then
	failwith "A player had more letters than he is allowed."
      else
	letters <- letters ^ s

    method letters_missing  =
      Bag.max_nb_letters - String.length letters
  end

class humanPlayer (a_name:string) (a_score:int) (a_letters:string) =
  object (self)
    inherit player a_name a_score a_letters
    method is_human = true
    method play s = print_string s
  end
