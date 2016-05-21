(* Fonctions de lecture de fichier *)
let read_line_test chanel =
	try
		Some (input_line chanel)
	with
		|End_of_file -> None
let read_file_tab chanel = 
	let rec aux acc = match  read_line_test chanel with
		|None -> acc
		|Some x -> aux (x :: acc)
	in List.rev (aux [])
	
(* Concatenation de caractères et chaines*)
let chars_to_string chars= 
	let rec aux acc = function
		|[] -> acc
		|x :: ls -> aux (acc ^ (String.make 1 x)) ls
	in aux "" chars
	
let string_super_map f str =
	let rec aux acc = function
		|x when x = String.length str -> acc
		|x -> aux (acc ^ (f str.[x])) (x + 1)
	in aux "" 0
	
	
(* Mise en place du premier encodeur *)
let make_pairs str =
	let rec aux acc pointeur pointeurbis = match (pointeur, pointeurbis) with
		|(x, y) when x = String.length str -> acc
		|(x, y) when y = String.length str -> aux acc (x + 1) 0
		|(x, y) -> aux ((chars_to_string [str.[x]; str.[y]]) :: acc) x (y + 1)
	in List.rev (aux [] 0 0)
	
let find_pair  hash caractere = 
	try
		Hashtbl.find hash caractere
	with
		Not_found -> ""

(* Debut *)
let contenu_tab = 
	let temp = read_file_tab (open_in "Cle36.txt")
	in List.nth temp 0
let dico = 
	let dicov = Hashtbl.create 36
	in let paires = make_pairs "ADFGVX"
	in let _ = String.iteri (fun key value -> Hashtbl.add dicov value (List.nth paires key)) contenu_tab
	in dicov
let phrase =
	let input = read_file_tab (open_in "Input.txt")
	in String.concat "" input
let conversion = string_super_map  (function x -> find_pair dico x) phrase