type action =
  |HELP
  |PICK
  |WORD of int*int*Rules.orientation*string

type word_pos = int*int*string

exception CantParse

let parse_orientation c =
  match c with
  |'v'|'V' -> Rules.V
  |'h'|'H' -> Rules.H
  |_ -> raise CantParse

let parse_word s =
  let line_char = Char.uppercase_ascii s.[0] in
  (*number of the line*)
  let line = 
    if line_char >= 'A' && line_char <= 'O' then
      int_of_char line_char - int_of_char 'A'
    else
      raise CantParse
  in
  (*the number can be up to 15 so 1 or 2 characters*)
  let (num_length,orient) = 
    if s.[3] = ' ' then
      (1,parse_orientation s.[2])
    else if s.[4] = ' ' then
      (2,parse_orientation s.[3])
    else
      raise CantParse
  in
  (*number of the column*)
  let col = int_of_string (String.sub s 1 num_length) - 1 in
  (line,col,orient,
   String.sub s (3+num_length) (String.length s - 3 - num_length))

    
let parse_action s = (*s means string*)
  if String.length s = 0 then
    None
  else if s.[0] = '#' then
    match String.sub s 1 (String.length s - 1 ) with
    |"aide" -> Some HELP
    |"piocher" -> Some PICK
    |_ -> None
  else
    begin
      try 
	let (l,c,o,w) = parse_word s in
	Some (WORD (l,c,o,w))	
      with 
      |_ -> None		   
    end
  
