type orientation = H | V

let int_of_orientation o = match o with
  | H -> 0
  | V -> 1

let inv_orientation o = match o with
  |H -> V
  |V -> H

		     
let load_dictionary file =
  let in_chan = open_in file in
  let length = ref 0 in 
  begin
    try
     while true do
       
       ignore (input_line in_chan);
       length := !length + 1
     done
    with
    |End_of_file ->
      ()
  end;
  let dict = Array.make !length "" in
  let in_chan = open_in file in
  for i = 0 to !length - 1 do
    dict.(i) <- input_line in_chan
  done;
  (* I don't understand why but the character 13 appears at the 
   * end of every word. We need to remove it*)
  Array.map (fun w -> String.sub w 0 (String.length w-1))  dict
 
      


		  

let dictionary = load_dictionary "ods.txt"
		       

let max_nb_letters = 7

type score_modifier =
  | NONE
  | MUL_LETTER of int
  | MUL_WORD of int

let score_modifiers =
  [|[|MUL_WORD 3; NONE;NONE;MUL_LETTER 2;NONE;NONE;NONE;
      MUL_WORD 3;NONE;NONE;NONE;MUL_LETTER 2;NONE;NONE;MUL_WORD 3|];
    [|NONE; MUL_WORD 2;NONE;NONE;NONE; MUL_LETTER 3;NONE;NONE;NONE;
      MUL_LETTER 3;NONE;NONE;NONE;MUL_WORD 2;NONE|];
    [|NONE;NONE;MUL_WORD 2;NONE;NONE;NONE;MUL_LETTER 2;NONE;
      MUL_LETTER 2;NONE;NONE;NONE;MUL_WORD 2;NONE;NONE|];
    [|MUL_LETTER 2;NONE;NONE;MUL_WORD 2;NONE;NONE;NONE;
      MUL_LETTER 2;NONE;NONE;NONE;MUL_WORD 2;NONE;NONE; MUL_LETTER 2|];
    [|NONE;NONE;NONE;NONE;MUL_WORD 2;NONE;NONE;NONE;NONE;NONE;
      MUL_WORD 2;NONE;NONE;NONE;NONE|];
    [|NONE;MUL_LETTER 3;NONE;NONE;NONE;MUL_LETTER 3;NONE;NONE;NONE;
      MUL_LETTER 3;NONE;NONE;NONE;MUL_LETTER 3; NONE|];
    [|NONE;NONE;MUL_LETTER 2;NONE;NONE;NONE;MUL_LETTER 2;NONE;MUL_LETTER 2;
      NONE;NONE;NONE;MUL_LETTER 2;NONE;NONE|];
    [|MUL_WORD 3;NONE;NONE;MUL_LETTER 2;NONE;NONE;NONE;MUL_WORD 2;
      NONE;NONE;NONE;MUL_LETTER 2;NONE;NONE;MUL_WORD 3|];
    [|NONE;NONE;MUL_LETTER 2;NONE;NONE;NONE;MUL_LETTER 2;NONE;MUL_LETTER 2;
      NONE;NONE;NONE;MUL_LETTER 2;NONE;NONE|];
    [|NONE;MUL_LETTER 3;NONE;NONE;NONE;MUL_LETTER 3;NONE;NONE;NONE;
      MUL_LETTER 3;NONE;NONE;NONE;MUL_LETTER 3; NONE|];
    [|NONE;NONE;NONE;NONE;MUL_WORD 2;NONE;NONE;NONE;NONE;NONE;
      MUL_WORD 2;NONE;NONE;NONE;NONE|];
    [|MUL_LETTER 2;NONE;NONE;MUL_WORD 2;NONE;NONE;NONE;
      MUL_LETTER 2;NONE;NONE;NONE;MUL_WORD 2;NONE;NONE; MUL_LETTER 2|];
    [|NONE;NONE;MUL_WORD 2;NONE;NONE;NONE;MUL_LETTER 2;NONE;
      MUL_LETTER 2;NONE;NONE;NONE;MUL_WORD 2;NONE;NONE|];
    [|NONE; MUL_WORD 2;NONE;NONE;NONE; MUL_LETTER 3;NONE;NONE;NONE;
      MUL_LETTER 3;NONE;NONE;NONE;MUL_WORD 2;NONE|];
    [|MUL_WORD 3; NONE;NONE;MUL_LETTER 2;NONE;NONE;NONE;
      MUL_WORD 3;NONE;NONE;NONE;MUL_LETTER 2;NONE;NONE;MUL_WORD 3|]
   |]




(* *********************** Bag gestion ********************************* *)

type letter_distrib = (char*int) list

(* creation of the bag*)
let fr_distrib = [('_',2);('A',9);('B',2);('C',2);('D',3);('E',15);('F',2);
               ('G',2);('H',2);('I',8);('J',1);('K',1);('L',5);('M',3);
               ('N',6);('O',6);('P',2);('Q',1);('R',6);('S',6);('T',6);
               ('U',6);('V',2);('W',1);('X',1);('Y',1);('Z',1)]

(*the number of point for each letter*)
let fr_points = [|1;3;3;2;1;4;2;4;1;8;10;1;2;1;1;3;8;1;1;1;1;4;10;10;10;10|]

let points = fr_points

let score_of_char c =
  if  c >= 'A' && c <= 'Z' then
    points.(int_of_char c - int_of_char 'A')
  else
    0


let string_to_list s =
  let l = ref [] in
  for i =  String.length s - 1 downto 0 do
    l := s.[i] :: !l
  done;
  !l

(*returns the score of a word without taking into account modfiers*)
let no_mul_score word =
  List.fold_left (fun x y -> x + y) 0
		 (List.map score_of_char (string_to_list word))


(*conversion of a distribution to a randomly shuffled string*)
let string_of_distrib distrib =
   let bag_list = ref [] in
   let add_letter c =
    bag_list := !bag_list @ Array.to_list (Array.make (snd c) (fst c));
   in
   List.iter add_letter distrib; (*now we've got the list of char not shuffled*)
   (*let'sshuffle it !*)
   Random.self_init ();
   let tmp = List.map (fun c -> (Random.bits (), c)) !bag_list in
   let sorted = List.sort compare tmp in
   (*now we've got a shuffled list of char*)
   (*let's transform it into a string*)
   bag_list := List.map snd sorted;
   String.concat "" (List.map Char.escaped !bag_list)

class bag (distrib : letter_distrib) =
  let bag_a = string_of_distrib distrib in
  object
    val mutable bag_str = bag_a

    (*Piocher n lettres*)
    method pick_letters n =
      try (*si on a assez de lettres*)
        let ret = String.sub bag_str 0 n in
        bag_str <- String.sub bag_str n (String.length bag_str - n);
        ret
      with (*sinon*)
        _ -> let ret = bag_str in
            bag_str <- "";
            ret
  end

let server_port = 14159
