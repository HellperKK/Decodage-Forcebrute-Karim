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
	
let read_first_line chanel =
	let chan = open_in chanel
	in let line = input_line chan 
	in let _ = close_in chan
	in line
	
(* Fonctions d'ecriture de fichier *)
let write_txt chanel str =
	let chan = open_out chanel
	in let _ = output_string chan str
	in close_out chan
	
(* Manipulation de caractères et chaines*)
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
	
let string_to_char_list str =
	let rec aux acc = function
		|"" -> acc
		|ch -> aux (ch.[0] :: acc) (String.sub ch 1 ((String.length ch)-1))
	in List.rev (aux [] str)
	
(* Manipulation de tableaux*)
let rec repeat valeur fois =
	let rec aux acc = function
		|0 -> acc
		|x -> aux (valeur :: acc) (x - 1)
	in aux [] fois
    
let slice_list liste a b =
	let rec aux acc i = 
		if i > b then acc
		else aux ((List.nth liste i) :: acc) (i + 1)
	in List.rev (aux [] a)
	
let split_list liste taille escape =
	let rec aux acc = function
		|[] -> acc
		|x when List.length x < taille -> (x @ (repeat escape (taille - List.length x))) :: acc
		|x -> aux ((slice_list x 0 (taille - 1)) :: acc) (slice_list x taille (List.length x - 1))
	in List.rev (aux [] liste)

let rows_to_columns tab = 
	let rec aux acc = function
		|x when List.flatten x = [] -> acc
		|x -> aux ((List.map (function y -> List.nth y 0) x) :: acc) (List.map (function y -> slice_list y 1 (List.length y - 1)) x)
	in List.rev (aux [] tab)
	
let count_l liste item =
	let rec aux count = function
	|[] -> count
	|x :: ls when x = item -> aux (count + 1) ls
	|_ :: ls -> aux count ls
	in aux 0 liste
	
let elements_order liste =
	let rec aux acc = function
	|[] -> acc
	|x :: ls -> let temp = (count_l liste x) - (count_l ls x) - 1
		in aux (temp :: acc) ls
	in List.rev (aux [] liste)
	
(* Mise en place du premier encodeur *)
let make_pairs str =
	let rec aux acc pointeur pointeurbis = match (pointeur, pointeurbis) with
		|(x, y) when x = String.length str -> acc
		|(x, y) when y = String.length str -> aux acc (x + 1) 0
		|(x, y) -> aux ((chars_to_string [str.[x]; str.[y]]) :: acc) x (y + 1)
	in List.rev (aux [] 0 0)
	
let find_pair hash caractere = 
	try
		Hashtbl.find hash caractere
	with
		Not_found -> ""

(* Debut *)
let cle_un = read_first_line "Cle36.txt"
let dico = 
	let dicov = Hashtbl.create 36
	in let paires = make_pairs "ADFGVX"
	in let _ = String.iteri (fun key value -> Hashtbl.add dicov value (List.nth paires key)) cle_un
	in dicov
let phrase =
	let input = read_file_tab (open_in "Input.txt")
	in String.concat "" input
let conversion_un = string_super_map  (function x -> find_pair dico x) phrase
let cle_deux = read_first_line "CleT.txt"
let tableau_un = 
	let tab = string_to_char_list conversion_un
	in split_list tab (String.length cle_deux) 'X'
let tableau_deux = rows_to_columns tableau_un
let cle_deuxb = string_to_char_list cle_deux
let tableau_final = 
	let aux = elements_order cle_deuxb
	in List.map (function x -> List.nth tableau_deux x) aux
(* let _ = write_txt "Output.txt" conversion *)